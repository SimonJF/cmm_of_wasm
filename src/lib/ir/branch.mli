type t
val create : Label.t -> Var.t list -> t
val label : t -> Label.t
val arguments : t -> Var.t list
val to_sexpr : t -> Libwasm.Sexpr.sexpr
