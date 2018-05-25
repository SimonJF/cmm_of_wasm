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
  let name = ("_wasm" ^ (string_of_int env.func_count)) in
  let symbol = Internal_symbol name in
  let new_env =
    { env with func_count = env.func_count + 1;
      func_symbols = Ir.Func.Map.add func symbol env.func_symbols } in
  (symbol, new_env)

let lookup_func_symbol func env = Ir.Func.Map.find func env.func_symbols

let bind_global_func_symbol (md: Ir.Func.t) env =
  let open Symbol in
  let name =
    match Ir.Func.name md with
      | Some name ->
          (* Internal name if we're generating CMM; standard name if not *)
          if Util.Command_line.generate_c () then
            Util.Names.internal_name name
          else name
      | _ -> failwith "Global function metadata must be named!" in
  { env with 
      func_symbols = 
        Ir.Func.Map.add md (Exported_symbol name) env.func_symbols 
  }

let symbols env =
  List.map snd (Ir.Func.Map.bindings env.func_symbols)

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


  
