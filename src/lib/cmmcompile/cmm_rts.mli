(* A collection of functions for interacting with memory,
 * written in CMM. Since they're CMM, they can be (and are) inlined. *)

type cmm_address = Cmm.expression

module Memory : sig
  (* Checks whether a memory access is safe; evaluates the supplied
   * expression if so, and traps if not. *)
  val with_mem_check :
    cmm_address -> (* Pointer to memory root for this module *)
    Libwasm.Memory.address ->
    Cmm.memory_chunk ->
    Cmm.expression -> (* Expression to be run if check succeeds *)
    Cmm.expression


  (* Checked load: inserts memory check, and loads the required memory
   * chunk *)
  val load :
    cmm_address ->
    Libwasm.Memory.address ->
    Cmm.memory_chunk ->
    Cmm.expression

  (* Checked store: inserts memory check, converts the expression to
   * fit in the given memory chunk, and then stores at the given address  *)
  val store :
    cmm_address ->
    Libwasm.Memory.address ->
    Cmm.expression -> (* Expression to store *)
    Cmm.memory_chunk ->
    Cmm.expression

  (* Tries to grow the memory. Returns previous size (in pages) if
   * successful, and -1 otherwise. *)
  val grow :
    cmm_address ->
    Libwasm.Memory.size ->
    Cmm.expression

  val size :
    cmm_address ->
    Cmm.expression
end
