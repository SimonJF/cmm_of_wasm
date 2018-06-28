open Util.Maps

type t = {
  stack : Var.t list;
  block_continuation: Label.t;
  function_return: Label.t;
  locals: Var.t Int32Map.t;
  labels: Label.t list;
  globals: Global.t Int32Map.t;
  functions: Func.t Int32Map.t;
  types: Libwasm.Types.func_type Int32Map.t
}

let create ~stack ~continuation ~return ~locals ~globals ~functions ~types =
  {
  stack;
  block_continuation = continuation;
  function_return = return;
  labels = [continuation];
  locals;
  globals;
  functions;
  types
}

let continuation env = env.block_continuation
let return env = env.function_return
let stack env = env.stack
let with_continuation lbl env =
  { env with block_continuation = lbl }

let push_label lbl env =
  { env with labels = lbl :: env.labels }

let set_local (var: Libwasm.Ast.var) value env =
  { env with locals = Int32Map.add (var.it) value (env.locals) }

let get_local (var: Libwasm.Ast.var) env =
  Int32Map.find (var.it) env.locals

let push x env = { env with stack = x :: (env.stack) }

let pop env =
  match env.stack with
    | [] -> failwith "FATAL (pop): Tried to pop from empty virtual stack"
    | x :: xs -> (x, { env with stack = xs })

let pop2 env =
  match env.stack with
    | x :: y :: xs -> ((x, y), { env with stack = xs })
    | _ -> failwith "FATAL (pop2): Tried to pop from empty virtual stack"

let nth_label (depth_var: Libwasm.Ast.var) env =
  List.nth env.labels (Int32.to_int depth_var.it) 

let popn n env =
  (* Printf.printf "popn n = %d, height = %d\n" n (List.length env.stack); *)
  let rec go n xs =
    if n = 0 then ([], xs) else
      match xs with
        | [] -> failwith "FATAL (popn): Tried to pop from empty virtual stack"
        | x :: xs ->
            let (returned, rest) = go (n - 1) xs in
            (x :: returned, rest) in
  let (popped, rest) = go n env.stack in
  popped, { env with stack = rest }

let popn_rev n env =
  let (args_rev, env) = popn n env in
  (List.rev args_rev, env)

let with_stack stack env = { env with stack }


let dump_stack env =
  List.map (Var.to_string) env.stack
  |> String.concat " :: "
  |> print_endline

let locals env =
  Int32Map.bindings env.locals
  |> List.map snd

let with_locals locals env =
  let (_, new_locals) =
  List.fold_left (fun (i, acc) v ->
    let acc = Int32Map.add i v acc in
    (Int32.(add i one), acc)
  ) (Int32.zero, Int32Map.empty) locals in
  { env with locals = new_locals}

let get_global (var: Libwasm.Ast.var) env =
  Int32Map.find (var.it) env.globals

let get_function (var: Libwasm.Ast.var) env =
  Int32Map.find (var.it) env.functions

let get_type (var: Libwasm.Ast.var) env =
  Int32Map.find (var.it) env.types
