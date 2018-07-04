type t = {
  var_env : Ident.t Ir.Var.Map.t;
  label_env : int Ir.Label.Id.Map.t;
  label_count : int;
  func_symbols : Symbol.symbol Ir.Func.Map.t;
  func_count : int;
  module_name : string;
  memory_module_name : string; (* Name of module containing memory *)
  table_module_name : string; (* Name of module containing table *)
}

let empty ~module_name ~memory_module_name ~table_module_name = {
  var_env = Ir.Var.Map.empty;
  label_env = Ir.Label.Id.Map.empty;
  label_count = 0;
  func_count = 0;
  func_symbols = Ir.Func.Map.empty;
  module_name;
  memory_module_name;
  table_module_name
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

let bind_internal_func_symbol prefix func env =
  let open Symbol in
  let name = (prefix ^ (string_of_int env.func_count)) in
  let symbol = Internal_symbol name in
  let new_env =
    { env with func_count = env.func_count + 1;
      func_symbols = Ir.Func.Map.add func symbol env.func_symbols } in
  (symbol, new_env)

let lookup_func_symbol func env = Ir.Func.Map.find func env.func_symbols

let bind_global_func_symbol (md: Ir.Func.t) (name: string) env =
  { env with
      func_symbols =
        Ir.Func.Map.add md (Symbol.Exported_symbol name) env.func_symbols
  }

let symbols env =
  List.map snd (Ir.Func.Map.bindings env.func_symbols)

let memory_symbol env =
  env.memory_module_name ^ "_Memory"

let global_symbol env glob =
  match Global.data glob with
    | DefinedGlobal _ ->
        env.module_name ^ "_Global" ^
          Ir.Global.(id glob |> Id.to_string)
    | ImportedGlobal { module_name; global_name } ->
        module_name ^ "_GlobalExport_" ^ global_name

let table_count_symbol env =
  env.table_module_name ^ "_TableCount"

let table_symbol env =
  env.table_module_name ^ "_Table"

let dump env =
  let open Ir in

  let print_var_env () =
    let bindings = Var.Map.bindings (env.var_env) in
    List.iter (fun (k, v) ->
      Printf.printf "%s : %s"
        (Var.to_string k)
    (Ident.unique_name v)) bindings;
    print_newline () in

  let print_label_env () =
    let bindings = Label.Id.Map.bindings (env.label_env) in
    List.iter (fun (k, v) ->
      let open Libwasm in
      Label.Id.print Format.str_formatter k;
      Printf.printf "%s : %d\n"
        (Format.flush_str_formatter ()) v) bindings;
      print_newline () in

  let print_func_symbols () =
    let bindings = Func.Map.bindings (env.func_symbols) in
    List.iter (fun (k, v) ->
      let open Libwasm in
      Ir.Func.print Format.str_formatter k;
      Printf.printf "%s : %s\n"
        (Format.flush_str_formatter ()) (Symbol.name v)) bindings;
      print_newline () in

  print_endline "Var env:";
  print_var_env ();
  print_endline "Label env:";
  print_label_env ();
  Printf.printf "Label count: %d\n" env.label_count;
  print_endline "Func symbols:";
  print_func_symbols ();
  Printf.printf "Func count: %d\n" env.func_count



