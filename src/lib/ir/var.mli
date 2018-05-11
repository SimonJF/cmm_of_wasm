type t
val create : Libwasm.Types.value_type -> t
val rename : t -> t
val type_  : t -> Libwasm.Types.value_type
val print : Format.formatter -> t -> unit
val reset : unit -> unit
val compare : t -> t -> int
val to_string : t -> string
module M : Map.OrderedType with type t = t
module Map : Map.S with type 'a t = 'a Map.Make(M).t and type key = t
