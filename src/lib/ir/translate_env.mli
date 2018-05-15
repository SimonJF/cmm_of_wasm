open Libwasm
type t

val create :
  stack:Var.t list ->
  continuation:Label.t ->
  return:Label.t ->
  locals:Var.t list ->
  t

val continuation : t -> Label.t
val return : t -> Label.t

(* Virtual stack *)
val stack : t -> Var.t list
val push : Var.t -> t -> t
val pop : t -> (Var.t * t)
val pop2 : t -> ((Var.t * Var.t) * t)
val popn : int -> t -> (Var.t list * t)
val with_stack : Var.t list -> t -> t

(* Control stack *)
val push_label : Label.t -> t -> t
val nth_label : depth:int -> t -> Label.t

(* Local variable store *)
val set_local : Ast.var -> Var.t -> t -> t
val get_local : Ast.var -> t -> Var.t
val locals : t -> Var.t list

val with_continuation : t -> Label.t -> t
