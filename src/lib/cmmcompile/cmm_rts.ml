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
    let size_uint32 = 4

    (* CHECK: Caddi, Cadda, or Caddv? Or does it matter not? *)
    let add_ptr addr offset = Cop (Caddi, [addr; offset], nodbg)

    let data_pointer root = Cop (Cload (Word_int, Mutable), [root], nodbg)

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
    if offset = 0 then
      addr
    else
      Cop (Caddi, [addr; Cconst_int offset], nodbg)


  let effective_address root offset =
    Cop (Caddi, [MemoryAccessors.data_pointer root; offset], nodbg)


  let with_mem_check ~root ~trap_ty ~effective_offset ~chunk ~expr =
    (* if ( effective_address + size(chunk_expr) > mem_size) then trap else expr *)
    Cifthenelse (
      Cop (Ccmpa Cgt,
        [Cop (Caddi, [effective_offset; Cconst_int (chunk_size chunk)], nodbg);
         MemoryAccessors.memory_size root], nodbg),
      trap trap_ty TrapOOB, expr)

  (* Specific for amd64 right now. *)
  let unsigned_chunk_of_type ty =
    let open Libwasm.Types in
    match ty with
      | I32Type -> Thirtytwo_unsigned
      | I64Type -> Word_int
      | F32Type -> Single
      | F64Type -> Double

  let signed_chunk_of_type ty sign_extension =
    let open Libwasm.Memory in
    let open Libwasm.Types in
    match ty with
      | I32Type ->
          if sign_extension = SX then Thirtytwo_signed
          else Thirtytwo_unsigned
      | I64Type -> Word_int
      | F32Type -> Single
      | F64Type -> Double

  (* Public API *)
  let load ~root ~dynamic_pointer ~(op:Libwasm.Ast.loadop) =
    let static_offset = Int32.to_int op.offset in
    let ext =
      match op.sz with Some (_, x) -> x | None -> Libwasm.Memory.ZX in
    let chunk = signed_chunk_of_type op.ty ext in
    let eo = effective_offset dynamic_pointer static_offset in
    let eo_ident = Ident.create "eo" in
    let eo_var = Cvar eo_ident in
    Clet (eo_ident, eo,
      with_mem_check
        ~root
        ~trap_ty:(trap_ty op.ty)
        ~effective_offset:eo_var
        ~chunk
        ~expr:(Cop (Cload (chunk, Mutable),
          [effective_address root eo_var], nodbg)))

  let store ~root ~dynamic_pointer ~(op:Libwasm.Ast.storeop) ~to_store =
    let static_offset = Int32.to_int op.offset in
    let chunk = unsigned_chunk_of_type op.ty in
    let eo = effective_offset dynamic_pointer static_offset in
    let eo_ident = Ident.create "eo" in
    let eo_var = Cvar eo_ident in
    Clet (eo_ident, eo,
      with_mem_check
        ~root
        ~trap_ty:typ_void
        ~effective_offset:eo_var
        ~chunk
        ~expr:(Cop (Cstore (chunk, Assignment),
          [effective_address root eo_var; to_store], nodbg)))

  let grow root pages =
    (* I *think* it's safe to put false as allocation flag here, since
     * we're not using the OCaml GC... I may be wrong. *)
    Cop (Cextcall ("wasm_rt_grow_memory", typ_int, false, None),
      [root; pages], nodbg)

  let size root = MemoryAccessors.memory_size root

end
