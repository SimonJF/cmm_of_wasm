type t

val create :
  module_name:string ->
  memory:Ir.Stackless.memory option ->
  table:Ir.Stackless.table option ->
  t

val bind_var : Ir.Var.t -> Ident.t -> t -> unit
val lookup_var : Ir.Var.t -> t -> Ident.t
val bind_label : Ir.Label.t -> t -> int
val lookup_label : Ir.Label.t -> t -> int
val module_name : t -> string

val memory_symbol : t -> string
val table_symbol : t -> string
val func_symbol : Ir.Func.t -> t -> string
val global_symbol : Ir.Global.t -> t -> string

val add_constant : Ir.Var.t -> Cmm.expression -> t -> unit
val resolve_variable : Ir.Var.t -> t -> Cmm.expression

val fuel_ident : t -> Ident.t
val reset : t -> unit
