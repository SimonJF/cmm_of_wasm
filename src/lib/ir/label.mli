module Id : sig
  type t
  val create : unit -> t
  val return : t
  val print : Format.formatter -> t -> unit
  val reset : unit -> unit
  val is_return : t -> bool
  val to_string : t -> string

  module M : Map.OrderedType with type t = t
  module Map : Map.S with type 'a t = 'a Map.Make(M).t and type key = t
end

type t
val create : arity:int -> local_ids:Annotated.var list -> t
val create_return : arity:int -> t
val id : t -> Id.t
val arity : t -> int
val local_ids : t -> Annotated.var list
val to_sexpr : t -> Libwasm.Sexpr.sexpr
val to_string : t -> string
