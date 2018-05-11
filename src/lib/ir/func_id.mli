type t
val create : name: Libwasm.Ast.name option -> t
val print : Format.formatter -> t -> unit

val is_named : t -> string -> bool

val name : t -> string option

module M : Map.OrderedType with type t = t
module Map : sig
  include Map.S with type 'a t = 'a Map.Make(M).t and type key = t
  val print : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end
