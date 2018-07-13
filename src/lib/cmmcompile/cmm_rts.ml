(* A collection of functions for interacting with memory,
 * written in CMM. Since they're CMM, they can be (and are) inlined. *)
open Cmm
open Cmm_trap

type alignment = int
type cmm_address = Cmm.expression

module Memory = struct
  let nodbg = Debuginfo.none
  let unimplemented = Ctuple []
  let int_call name = Cextcall (name, typ_int, false, None)
  let i64_to_native = Int64.to_nativeint
  let page_size =
    Cconst_natint (i64_to_native Libwasm.Memory.page_size)
  let size_int = Arch.size_int

  module MemoryAccessors = struct
    (* Accessors for `wasm_rt_memory_t`, defined in wasm2c's `wasm-rt.h`.
     * NOTE: This is a possible portability hazard as we're hardcoding GCC's
     * struct layout, which isn't defined by the C99 spec IIRC.
     * Nonetheless, since wasm_rt_memory_t is word-aligned, it's the
     * obvious struct encoding to use.
     *)
    (* Root pointer: address of the memory structure *)
    let size_uint32 = 4

    (* CHECK: Caddi, Cadda, or Caddv? Or does it matter not? *)
    let add_ptr addr offset = Cop (Caddi, [addr; offset], nodbg)

    let data_pointer root =
      Cop (Cload (Word_int, Mutable), [root], nodbg)

    let pages root =
      Cop (Cload (Thirtytwo_unsigned, Mutable),
        [Cop (Caddi, [root; Cconst_int size_int], nodbg)], nodbg)

    let max_pages root =
      Cop (Cload (Thirtytwo_unsigned, Mutable),
        [Cop (Caddi, [root; Cconst_int (size_int + size_uint32)], nodbg)], nodbg)

    let memory_size root =
      Cop (Cload (Thirtytwo_unsigned, Mutable),
        [Cop (Caddi, [root; Cconst_int (size_int + (size_uint32 * 2))], nodbg)], nodbg)
  end

  (* Size, in bytes, of CMM memory chunks *)
  let chunk_size = function
    | Byte_unsigned | Byte_signed -> 1
    | Sixteen_unsigned | Sixteen_signed -> 2
    | Thirtytwo_unsigned | Thirtytwo_signed -> 4
    | Word_int | Word_val -> size_int
    | Single -> 4
    | Double | Double_u -> 8


  (* Number of bytes offset for the given WASM address *)
  let effective_offset addr offset =
    (* I *think* addresses refer to bytes... So we don't need to do anything
     * special here *)
    if offset = Nativeint.zero then
      addr
    else
      let normalised_offset =
        Cconst_natint (Nativeint.logand offset
          (Nativeint.of_string "0x00000000FFFFFFFF")) in
      Cop (Cadda, [addr; normalised_offset], nodbg)


  let effective_address root offset =
    Cop (Caddi, [MemoryAccessors.data_pointer root; offset], nodbg)


  let with_mem_check ~root ~trap_ty ~effective_offset ~chunk ~expr =
    let out_of_bounds =
      Cop (Ccmpa Cgt,
        [Cop (Caddi, [effective_offset; Cconst_int (chunk_size chunk)], nodbg);
         MemoryAccessors.memory_size root], nodbg) in

    Cifthenelse (
      out_of_bounds,
      trap trap_ty TrapOOB,
      expr)

  (* Specific for amd64 right now. *)
  let chunk_of_type ty =
    let open Libwasm.Types in
    match ty with
      | I32Type -> Thirtytwo_unsigned
      | I64Type -> Word_int
      | F32Type -> Single
      | F64Type -> Double

  let chunk_of_loadop (op: Libwasm.Ast.loadop) =
    let open Libwasm.Memory in
    match op.sz with
      | Some (Pack8, ZX) -> Byte_unsigned
      | Some (Pack8, SX) -> Byte_signed
      | Some (Pack16, ZX) -> Sixteen_unsigned
      | Some (Pack16, SX) -> Sixteen_signed
      | Some (Pack32, ZX) -> Thirtytwo_unsigned
      | Some (Pack32, SX) -> Thirtytwo_signed
      | None -> chunk_of_type op.ty

  let chunk_of_storeop (op: Libwasm.Ast.storeop) =
    let open Libwasm.Memory in
    match op.sz with
      | Some (Pack8) -> Byte_unsigned
      | Some (Pack16) -> Sixteen_unsigned
      | Some (Pack32) -> Thirtytwo_unsigned
      | None -> chunk_of_type op.ty


  (* Public API *)
  let load ~root ~dynamic_pointer ~(op:Libwasm.Ast.loadop) =
    let open Libwasm.Types in
    let static_offset = Nativeint.of_int32 op.offset in
    let chunk = chunk_of_loadop op in
    let eo = effective_offset dynamic_pointer static_offset in
    let eo_ident = Ident.create "eo" in
    let eo_var = Cvar eo_ident in
    let base_expr =
      Cop (Cload (chunk, Mutable), [effective_address root eo_var], nodbg) in
    let expr =
      (* HACK: OCaml helpfully transforms a F32 into a F64 when
       * loading. This won't do, since WASM expects F32 store / loads
       * to be bit-preserving, so we have to emulate via a C call. *)
      if op.ty = F32Type then
        Cop (Cextcall ("wasm_rt_load_f32", typ_float, false, None),
          [root; eo_var], nodbg)
      else base_expr in

    Clet (eo_ident, eo,
      with_mem_check
        ~root
        ~trap_ty:(trap_ty op.ty)
        ~effective_offset:eo_var
        ~chunk
        ~expr)

  let store ~root ~dynamic_pointer ~(op:Libwasm.Ast.storeop) ~to_store =
    let open Libwasm.Types in
    let static_offset = Nativeint.of_int32 op.offset in
    let chunk = chunk_of_storeop op in
    let eo = effective_offset dynamic_pointer static_offset in
    let eo_ident = Ident.create "eo" in
    let eo_var = Cvar eo_ident in
    (* As above. *)
    let expr =
      if op.ty = F32Type then
        Cop (Cextcall ("wasm_rt_store_f32", typ_void, false, None),
          [root; eo_var; to_store], nodbg)
      else
        Cop (Cstore (chunk, Assignment),
        [effective_address root eo_var; to_store], nodbg) in
    Clet (eo_ident, eo,
      with_mem_check
        ~root
        ~trap_ty:typ_void
        ~effective_offset:eo_var
        ~chunk
        ~expr)

  let grow root pages =
    (* I *think* it's safe to put false as allocation flag here, since
     * we're not using the OCaml GC... I may be wrong. *)
    Cop (Cextcall ("wasm_rt_grow_memory", typ_int, false, None),
      [root; pages], nodbg)

  let size root =
    Cop (Cdivi, [MemoryAccessors.memory_size root; page_size], nodbg)

end


module Globals = struct

  let set ~symbol ~ty ~to_store =
    if ty = Libwasm.Types.F32Type then
      Cop (Cextcall ("wasm_rt_set_global_f32", typ_void, false, None),
        [symbol; to_store], nodbg)
    else
      let chunk = Memory.chunk_of_type ty in
      Cop (Cstore (chunk, Assignment), [symbol; to_store], nodbg)

  let get ~symbol ~ty =
    if ty = Libwasm.Types.F32Type then
      Cop (Cextcall ("wasm_rt_get_global_f32", typ_float, false, None),
        [symbol], nodbg)
    else
      let chunk = Memory.chunk_of_type ty in
      Cop (Cload (chunk, Mutable), [symbol], nodbg)
end

module Tables = struct
  let count env =
    let symb = Cconst_symbol (Compile_env.table_symbol env) in
    Cop (Cload (Word_int, Immutable), [symb], nodbg)

  (* Precondition: function ID is normalised (i.e., top 32 bits cleared *)
  let function_offset func_id =
    Cop (Caddi,
      [Cop (Cmuli, [func_id; Cconst_int (2 * Arch.size_int)], nodbg);
       Cconst_int (2 * Arch.size_int)], nodbg)

  (* Pointer to table data root *)
  let table_root env =
    let root_symb = Cconst_symbol (Compile_env.table_symbol env) in
    Cop (Cload (Word_int, Mutable), [root_symb], nodbg)

  let function_hash env func_id =
    let root = table_root env in
    let address = Cop (Caddi, [root; function_offset func_id], nodbg) in
    (* Alas, this must be mutable -- loading other modules may change it *)
    Cop (Cload (Word_int, Mutable), [address], nodbg)

  let function_pointer env func_id =
    let root = table_root env in
    let pointer_offset =
      Cop (Caddi, [function_offset func_id; Cconst_int Arch.size_int], nodbg) in
    let address = Cop (Caddi, [root; pointer_offset], nodbg) in
    Cop (Cload (Word_int, Mutable), [address], nodbg)

  let set_table_entry env func_id hash function_name =
    let root = table_root env in
    let offset = function_offset func_id in
    let pointer_offset =
      Cop (Caddi, [function_offset func_id; Cconst_int Arch.size_int], nodbg) in
    let hash_address =
      Cop (Caddi, [root; offset], nodbg) in
    let pointer_address =
      Cop (Caddi, [root; pointer_offset], nodbg) in
    let store_hash =
      Cop (Cstore (Word_int, Assignment),
        [hash_address; Cconst_natint hash], nodbg) in
    let store_pointer =
      Cop (Cstore (Word_int, Assignment),
        [pointer_address; Cconst_symbol function_name], nodbg) in
    Csequence (store_hash, store_pointer)


end

