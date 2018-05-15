open Libwasm

type t = {
  stack : Var.t list;
  block_continuation: Label.t;
  function_return: Label.t;
  label_stack: Label.t list;
  locals: Var.t list
}

let create ~stack ~continuation ~return ~locals = {
  stack;
  block_continuation = continuation;
  function_return = return;
  label_stack = [];
  locals = locals
}
 
let continuation env = env.block_continuation
let return env = env.function_return
let stack env = env.stack
let with_continuation env lbl =
  { env with block_continuation = lbl }

let push_label lbl env = {
  env with label_stack = lbl :: env.label_stack
}

(* TODO: These are inefficient as lists -- immutable
 * arrays would be a lot better. Maybe maps? *)
let set_local (var: Ast.var) value env =
  let rec go n = function
    | [] -> failwith "FATAL: set_local -- not enough variables"
    | x :: xs ->
        if (n = Int32.zero) then value :: xs
        else x :: (go (Int32.sub n 1l) xs) in
  let xs = go var.it env.locals in
  { env with locals = xs }

let get_local (var: Ast.var) env =
  Lib.List32.nth env.locals var.it

let push x env = { env with stack = x :: (env.stack) }

let pop env =
  match env.stack with
    | [] -> failwith "FATAL (pop): Tried to pop from empty virtual stack"
    | x :: xs -> (x, { env with stack = xs })

let pop2 env =
  match env.stack with
    | x :: y :: xs -> ((x, y), { env with stack = xs })
    | _ -> failwith "FATAL (pop2): Tried to pop from empty virtual stack"

let nth_label ~depth env =
  assert (List.length env.label_stack >= depth);
  List.nth env.label_stack depth

let popn n env =
  let rec go n xs = 
    if n = 0 then ([], xs) else
      match xs with
        | [] -> failwith "FATAL (popn): Tried to pop from empty virtual stack"
        | x :: xs -> 
            let (returned, rest) = go (n - 1) xs in
            (x :: returned, rest) in
  let (popped, rest) = go n env.stack in
  popped, { env with stack = rest }

let with_stack stack env = { env with stack }
let locals env = env.locals
