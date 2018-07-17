type t = {
  var_env : (Ir.Var.t, Ident.t) Hashtbl.t;
  label_env : (Ir.Label.Id.t, int) Hashtbl.t;
  mutable label_count : int;
  module_name : string;
  memory: Ir.Stackless.memory option;
  table : Ir.Stackless.table option;
  imported_function_count: int
}

let create ~module_name ~memory ~table ~imported_function_count = {
  var_env = Hashtbl.create 100;
  label_env = Hashtbl.create 20;
  label_count = 0;
  module_name;
  memory;
  table;
  imported_function_count
}

let module_name env = env.module_name

let bind_var v i env = Hashtbl.replace env.var_env v i

let lookup_var v env = Hashtbl.find env.var_env v

let bind_label lbl env =
  let lbl_id = env.label_count in
  env.label_count <- env.label_count + 1;
  Hashtbl.replace env.label_env (Ir.Label.id lbl) lbl_id;
  lbl_id

let lookup_label lbl env =
  Hashtbl.find env.label_env (Ir.Label.id lbl)

let func_symbol func env =
  Ir.Func.symbol ~module_name:env.module_name func

let global_symbol glob env =
  Ir.Global.symbol ~module_name:env.module_name glob

let table_symbol env =
  match env.table with
    | Some (ImportedTable { module_name; table_name; _ }) ->
        Printf.sprintf "%s_table_%s" module_name table_name
    | _ -> env.module_name ^ "_internaltable"

let memory_symbol env =
  match env.memory with
    | Some (ImportedMemory { module_name; memory_name; _ }) ->
        Printf.sprintf "%s_memory_%s" module_name memory_name
    | _ -> env.module_name ^ "_internalmemory"

let imported_function_count env = env.imported_function_count
(*
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
      Label.Id.print Format.str_formatter k;
      Printf.printf "%s : %d\n"
        (Format.flush_str_formatter ()) v) bindings;
      print_newline () in

  print_endline "Var env:";
  print_var_env ();
  print_endline "Label env:";
  print_label_env ();
  Printf.printf "Label count: %d\n" env.label_count

  *)
let dump _ = ()
