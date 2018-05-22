type t = {
  var_env : Ident.t Ir.Var.Map.t;
  label_env : int Ir.Label.Id.Map.t;
  label_count : int;
  func_symbols : Symbol.symbol Ir.Func.Map.t;
  func_count : int
}

let empty = {
  var_env = Ir.Var.Map.empty;
  label_env = Ir.Label.Id.Map.empty;
  label_count = 0;
  func_count = 0;
  func_symbols = Ir.Func.Map.empty
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

let bind_internal_func_symbol func env =
  let open Symbol in
  (* FIXME: This will not be sufficient
   * for when we have separate compilation...
   * Will also need to think harder about exports, etc. *)
  let symbol = Internal_symbol ("_wasm" ^ (string_of_int env.func_count)) in
  let new_env =
    { env with func_count = env.func_count + 1;
      func_symbols = Ir.Func.Map.add func symbol env.func_symbols } in
  (symbol, new_env)

let lookup_func_symbol func env = Ir.Func.Map.find func env.func_symbols

let bind_global_func_symbol (md: Ir.Func.t) env =
  let open Symbol in
  let name =
    match Ir.Func.name md with
      | Some name -> name
      | _ -> failwith "Global function metadata must be named!" in
  { env with 
      func_symbols = 
        Ir.Func.Map.add md (Exported_symbol name) env.func_symbols 
  }
