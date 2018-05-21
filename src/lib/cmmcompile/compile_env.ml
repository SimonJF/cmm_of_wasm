type t = {
  var_env : Ident.t Ir.Var.Map.t;
  label_env : int Ir.Label.Id.Map.t;
  label_count : int
}

let empty = {
  var_env = Ir.Var.Map.empty;
  label_env = Ir.Label.Id.Map.empty;
  label_count = 0;
}

let bind_var v i env = {
  env with var_env = Ir.Var.Map.add v i env.var_env
}

let lookup_var v env = Ir.Var.Map.find v env.var_env

let bind_label lbl env =
  let lbl_id = env.label_count in
  (lbl_id,
    { env with
      label_count = lbl_id + 1;
      label_env = Ir.Label.Id.Map.add (Ir.Label.id lbl) lbl_id env.label_env })

let lookup_label lbl env = Ir.Label.Id.Map.find (Ir.Label.id lbl) env.label_env 
