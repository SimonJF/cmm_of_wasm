type t
val create : name: Libwasm.Ast.name option -> Libwasm.Types.global_type -> t
val is_mutable : t -> bool
val print : Format.formatter -> t -> unit
val type_ : t -> Libwasm.Types.value_type

val name : t -> string option

module M : Map.OrderedType with type t = t
module Map : sig
  include Map.S with type 'a t = 'a Map.Make(M).t and type key = t
  val print : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end
