module Id : sig
  type t
  val create : unit -> t
  val to_string : t -> string
  val compare : t -> t -> int
end

type t
val create :
  name: Libwasm.Ast.name option ->
  Libwasm.Types.global_type ->
  Libwasm.Values.value ->
  t

val is_mutable : t -> bool
val print : Format.formatter -> t -> unit
val type_ : t -> Libwasm.Types.value_type

val name : t -> string option

val initial_value : t -> Libwasm.Values.value

val to_sexpr : t -> Libwasm.Sexpr.sexpr

