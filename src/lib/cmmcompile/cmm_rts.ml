(* A collection of functions for interacting with memory,
 * written in CMM. Since they're CMM, they can be (and are) inlined. *)
open Cmm
open Cmm_trap

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
  let physical_offset addr =
    Cop (Cmuli, [Cconst_natint (i64_to_native addr); page_size], nodbg)

  let to_physical_addr root addr =
    Cop (Caddi, [MemoryAccessors.data_pointer root; physical_offset addr], nodbg)

  (* Public API *)
  let with_mem_check root addr chunk expr =
    (* if ( (addr * page_size) + size(chunk_expr) > mem_size) then trap else expr *)
    Cifthenelse (
      Cop (Ccmpa Cgt,
        [Cop (Caddi,
           [Cop (Cmuli, [Cconst_natint (i64_to_native addr); page_size], nodbg);
           page_size], nodbg);
         MemoryAccessors.memory_size root], nodbg),
      trap TrapOOB, expr)

  let load root addr chunk =
    with_mem_check root addr chunk
      (Cop (Cload (chunk, Mutable), [to_physical_addr root addr], nodbg))

  let store root addr to_store chunk =
    with_mem_check root addr chunk
      (Cop (Cstore (chunk, Assignment),
        [to_physical_addr root addr; to_store], nodbg))

  let grow root pages =
    (* I *think* it's safe to put false as allocation flag here, since
     * we're not using the OCaml GC... I may be wrong. *)
    Cop (Cextcall ("wasm_rt_grow_memory", typ_int, false, None),
      [root; pages], nodbg)

  let size root = MemoryAccessors.memory_size root

end
