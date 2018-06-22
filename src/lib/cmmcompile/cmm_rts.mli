(* A collection of functions for interacting with memory,
 * written in CMM. Since they're CMM, they can be (and are) inlined. *)

type alignment = int
type cmm_address = Cmm.expression

module Memory : sig

  (* Checked load: inserts memory check, and loads the required memory
   * chunk *)
  val load :
    root:cmm_address ->
    dynamic_pointer:cmm_address ->
    op:Libwasm.Ast.loadop ->
    Cmm.expression

  (* Checked store: inserts memory check, converts the expression to
   * fit in the given memory chunk, and then stores at the given address  *)
  val store :
    root:cmm_address ->
    dynamic_pointer:cmm_address ->
    op:Libwasm.Ast.storeop ->
    to_store:Cmm.expression ->
    Cmm.expression

  (* Tries to grow the memory. Returns previous size (in pages) if
   * successful, and -1 otherwise. *)
  val grow :
    cmm_address ->
    Cmm.expression (* Number of pages *) ->
    Cmm.expression

  val size :
    cmm_address ->
    Cmm.expression
end