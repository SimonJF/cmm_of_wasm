type t

val empty :
  module_name:string ->
  memory:Ir.Stackless.memory option ->
  table:Ir.Stackless.table option ->
  t

val bind_var : Ir.Var.t -> Ident.t -> t -> t
val lookup_var : Ir.Var.t -> t -> Ident.t

val bind_label : Ir.Label.t -> t -> (int * t)
val lookup_label : Ir.Label.t -> t -> int

val module_name : t -> string
val func_symbol : Ir.Func.t -> t -> string
val memory_symbol : t -> string
val global_symbol : Ir.Global.t -> t -> string
val table_symbol : t -> string

val dump : t -> unit
