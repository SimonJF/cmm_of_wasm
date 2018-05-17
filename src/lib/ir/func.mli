type t
val create : name: Libwasm.Ast.name option -> ty: Libwasm.Types.func_type -> t
val print : Format.formatter -> t -> unit

val is_named : t -> string -> bool

val name : t -> string option
val type_ : t -> Libwasm.Types.func_type

val to_sexpr : t -> Libwasm.Sexpr.sexpr

module M : Map.OrderedType with type t = t
module Map : sig
  include Map.S with type 'a t = 'a Map.Make(M).t and type key = t
  val print : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end
