type t

val empty : t

val bind_var : Ir.Var.t -> Ident.t -> t -> t
val lookup_var : Ir.Var.t -> t -> Ident.t

