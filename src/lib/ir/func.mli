type t

val create_defined : Libwasm.Types.func_type -> t
val create_imported :
  module_name:string ->
  function_name:string ->
  Libwasm.Types.func_type -> t

val print : Format.formatter -> t -> unit

val with_name : t -> Libwasm.Ast.name -> t

val name : t -> string option
val type_ : t -> Libwasm.Types.func_type

val to_sexpr : t -> Libwasm.Sexpr.sexpr
val is_imported : t -> bool

module M : Map.OrderedType with type t = t
module Map : sig
  include Map.S with type 'a t = 'a Map.Make(M).t and type key = t
  val print : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end
