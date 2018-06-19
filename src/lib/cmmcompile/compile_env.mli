type t

val empty : string (* memory symbol name *) -> t

val bind_var : Ir.Var.t -> Ident.t -> t -> t
val lookup_var : Ir.Var.t -> t -> Ident.t

val bind_label : Ir.Label.t -> t -> (int * t)

val lookup_label : Ir.Label.t -> t -> int

val bind_internal_func_symbol : Ir.Func.t -> t -> (Symbol.symbol * t)

val bind_global_func_symbol : Ir.Func.t -> string -> t -> t

val lookup_func_symbol : Ir.Func.t -> t -> Symbol.symbol

val symbols : t -> Symbol.symbol list

val memory_symbol : t -> string

val dump : t -> unit
