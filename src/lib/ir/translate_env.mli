type t

val create :
  stack:Var.t list ->
  continuation:Label.t ->
  return:Label.t ->
  locals:Var.t Util.Maps.Int32Map.t ->
  globals:Global.t Util.Maps.Int32Map.t ->
  functions:Func.t Util.Maps.Int32Map.t ->
  types:Libwasm.Types.func_type Util.Maps.Int32Map.t ->
  t

val continuation : t -> Label.t
val return : t -> Label.t

(* Creates a fresh copy of the environment *)
val copy : t -> t

(* Virtual stack *)
val stack : t -> Var.t list
val push : Var.t -> t -> unit
val pop : t -> Var.t
val pop2 : t -> (Var.t * Var.t)
val popn : int -> t -> Var.t list
val popn_rev : int -> t -> Var.t list
val peek : t -> Var.t
val peek2 : t -> (Var.t * Var.t)
val peekn : int -> t -> Var.t list
val peekn_rev : int -> t -> Var.t list
val set_stack : Var.t list -> t -> unit

(* Control stack *)
val push_label : Label.t -> t -> unit
val nth_label : Annotated.var -> t -> Label.t
(* Local variable store *)
val set_local : Annotated.var -> Var.t -> t -> unit
val get_local : Annotated.var -> t -> Var.t
val locals : t -> Var.t list
val set_locals : Var.t list -> t -> unit

(* Globals *)
val get_global : Annotated.var -> t -> Global.t

(* Functions *)
val get_function : Annotated.var -> t -> Func.t
val set_continuation : Label.t -> t -> unit

(* Types *)
val get_type : Annotated.var -> t -> Libwasm.Types.func_type

(* Debugging *)
val dump_stack : t -> unit
