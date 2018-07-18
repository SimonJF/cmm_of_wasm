open Util.Maps

type t = {
  mutable stack : Var.t list;
  mutable block_continuation: Label.t;
  mutable function_return: Label.t;
  mutable locals: Var.t array;
  mutable labels: Label.t list;
  globals: Global.t array;
  functions: Func.t array;
  types: Libwasm.Types.func_type array
}

let create ~stack ~continuation ~return ~locals ~globals ~functions ~types =
  (* Assumes a dense ordering -- which we are guaranteed to have by
   * OCaml's indexing system *)
  let array_of_map m =
    let max_key =
      match Int32Map.max_binding_opt m with
        | Some (k, _) -> (Int32.to_int k) + 1
        | None -> 0 in
    Array.init max_key (fun k -> Int32Map.find (Int32.of_int k) m) in
  let locals_arr = array_of_map locals in
  {
  stack;
  block_continuation = continuation;
  function_return = return;
  labels = [continuation];
  locals = locals_arr;
  globals = array_of_map globals;
  functions = array_of_map functions;
  types = array_of_map types
}

let copy env =
  let locals_copy = Array.copy env.locals in
  (* Globals, functions, and types do not change throughout execution,
   * so there is no need to copy them. *)
  { env with locals = locals_copy }

let continuation env = env.block_continuation
let return env = env.function_return
let stack env = env.stack
let set_continuation lbl env =
  env.block_continuation <- lbl

let push_label lbl env =
  env.labels <- lbl :: env.labels

let nth_label (depth_var: Annotated.var) env =
  List.nth env.labels (Int32.to_int depth_var)

let set_local (var: Annotated.var) value env =
  env.locals.(Int32.to_int var) <- value

let get_local (var: Annotated.var) env =
  env.locals.(Int32.to_int var)

let push x env =
  env.stack <- (x :: (env.stack))


let split_stack stack count =
  let rec go n xs =
    if n = 0 then ([], xs) else
      match xs with
        | [] -> failwith "FATAL (split_stack): Tried to pop from empty virtual stack"
        | x :: xs ->
            let (returned, rest) = go (n - 1) xs in
            (x :: returned, rest) in
  go count stack

let peek env =
  let [@warning "-8"] ([x], _) = split_stack env.stack 1 in
  x

let peek2 env =
  let [@warning "-8"] ([x; y], _) = split_stack env.stack 2 in
  (x, y)

let peekn n env =
  let (xs, _) = split_stack env.stack n in
  xs

let peekn_rev n env =
  let (xs, _) = split_stack env.stack n in
  List.rev xs

let pop env =
  let [@warning "-8"] ([x], rest) = split_stack env.stack 1 in
  env.stack <- rest;
  x

let pop2 env =
  let [@warning "-8"] ([x; y], rest) = split_stack env.stack 2 in
  env.stack <- rest;
  (x, y)

let popn n env =
  let (xs, rest) = split_stack env.stack n in
  env.stack <- rest;
  xs

let popn_rev n env =
  let (xs, rest) = split_stack env.stack n in
  env.stack <- rest;
  List.rev xs

let set_stack stack env = env.stack <- stack

let dump_stack env =
  List.map (Var.to_string) env.stack
  |> String.concat " :: "
  |> print_endline

let locals env = Array.to_list env.locals

let set_locals locals env =
  env.locals <- (Array.of_list locals)

let get_global (var: Annotated.var) env =
  env.globals.(Int32.to_int var)

let get_function (var: Annotated.var) env =
  env.functions.(Int32.to_int var)

let get_type (var: Annotated.var) env =
  env.types.(Int32.to_int var)

