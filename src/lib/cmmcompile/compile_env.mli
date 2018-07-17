type t

val create :
  module_name:string ->
  memory:Ir.Stackless.memory option ->
  table:Ir.Stackless.table option ->
  imported_function_count:int ->
  t

val bind_var : Ir.Var.t -> Ident.t -> t -> unit
val lookup_var : Ir.Var.t -> t -> Ident.t
val bind_label : Ir.Label.t -> t -> int
val lookup_label : Ir.Label.t -> t -> int
val module_name : t -> string

val imported_function_count : t -> int

val memory_symbol : t -> string
val table_symbol : t -> string
val func_symbol : Ir.Func.t -> t -> string
val global_symbol : Ir.Global.t -> t -> string
