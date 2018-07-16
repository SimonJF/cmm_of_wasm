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

module Globals : sig
  val set :
    symbol:Cmm.expression ->
    ty:Libwasm.Types.value_type ->
    to_store:Cmm.expression ->
    Cmm.expression

  val get :
    symbol:Cmm.expression ->
    ty:Libwasm.Types.value_type ->
    Cmm.expression
end

module Tables : sig
  val count : Compile_env.t -> Cmm.expression
  val function_hash : Compile_env.t -> Cmm.expression -> Cmm.expression
  val function_pointer : Compile_env.t -> Cmm.expression -> Cmm.expression
  val uses_c_conventions : Compile_env.t -> Cmm.expression -> Cmm.expression
  val set_table_entry :
    Compile_env.t ->
    Cmm.expression -> (* Function ID *)
    nativeint -> (* Hash *)
    string -> (* Function name *)
    bool -> (* True if function uses C conventions *)
    Cmm.expression

end
