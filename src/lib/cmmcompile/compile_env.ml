type t = {
  var_env : Ident.t Ir.Var.Map.t
}

let empty = { var_env = Ir.Var.Map.empty }

let bind_var v i env = {
  env with var_env = Ir.Var.Map.add v i env.var_env
}

let lookup_var v env = Ir.Var.Map.find v env.var_env

