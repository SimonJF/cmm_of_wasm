module Id : sig
  type t
  val create : unit -> t
  val to_string : t -> string
  val compare : t -> t -> int
end

type t

type initial_value =
  | Constant of Libwasm.Values.value
  | AnotherGlobal of t

val create_defined :
  name: Libwasm.Ast.name option ->
  ty: Libwasm.Types.global_type ->
  initial_value: initial_value ->
  t

val create_imported :
  module_name: string ->
  global_name: string ->
  ty: Libwasm.Types.global_type ->
  t

val id : t -> Id.t
val is_mutable : t -> bool
val print : Format.formatter -> t -> unit
val type_ : t -> Libwasm.Types.value_type

type global_data =
  | DefinedGlobal of { name: string option; initial_value : initial_value }
  | ImportedGlobal of { module_name: string; global_name: string }

val data : t -> global_data
val to_sexpr : t -> Libwasm.Sexpr.sexpr

