(* A collection of functions for interacting with memory,
 * written in CMM. Since they're CMM, they can be (and are) inlined. *)
open Cmm

type cmm_address = Cmm.expression

module Memory = struct
  let nodbg = Debuginfo.none
  let unimplemented = Ctuple []
  let int_call name = Cextcall (name, typ_int, false, None)

  module MemoryAccessors = struct
    (* Accessors for `wasm_rt_memory_t`, defined in wasm2c's `wasm-rt.h`.
     * NOTE: This is a possible portability hazard as we're hardcoding GCC's
     * struct layout, which isn't defined by the C99 spec IIRC.
     * Nonetheless, since wasm_rt_memory_t is word-aligned, it's the
     * obvious struct encoding to use.
     *)
    let size_int = Arch.size_int
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



  (* Public API *)
  let with_mem_check root addr chunk expr = unimplemented

  let load root addr chunk = unimplemented

  let store root addr to_store chunk = unimplemented

  let grow root pages = unimplemented

  let size root = MemoryAccessors.memory_size root
end
