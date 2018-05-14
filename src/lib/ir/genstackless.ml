open Stackless
open Libwasm
(*
let ir_of_function f = ir_of_function' f.it
and ir_of_function' f = {
  ftype = f.type_;

}
*)

let ir_function _ = failwith "todo"
let ir_instrs = failwith "todo"

(* IR generation *)
let terminate (code: Stackless.statement list) (terminator: Stackless.terminator) = {
  body = List.rev code;
  terminator = terminator
}

let push_all stack xs =
  let rec go = function
    | [] -> ()
    | x :: xs -> Stack.push x stack; (go xs) in
  go xs

let pop_all stack =
  let rec go () =
    if (Stack.is_empty stack) then [] else Stack.pop stack :: go () in
  go ()

let ir_term instrs env =
  let open Libwasm.Ast in
  let rec transform_instrs env generated (instrs: Ast.instr list) =
  (* Stack manipulation *)
  let pop () = Stack.pop (Translate_env.stack env) in

  let pop2 () =
    let v1 = Stack.pop (Translate_env.stack env) in
    let v2 = Stack.pop (Translate_env.stack env) in
    (v1, v2) in

  let popn n =
    let rec go n =
      if n <= 0 then
        []
      else
        let v = Stack.pop (Translate_env.stack env) in
        v :: (go (n-1)) in
    go n in
    match instrs with
      | [] ->
          (* Have:
            * - A list of generated instructions
            * - Function return
            * - Block return *)
          (* Need: term *)
          let cont_label = (Translate_env.continuation env) in
          let arity = Label.arity cont_label in
          let returned = popn arity |> List.rev in
          let return_branch = Branch.create cont_label returned in
          let terminator = Stackless.Br return_branch in
          terminate generated terminator
      | x :: xs ->
          begin
          (* Binds a "pushed" variable, records on virtual stack *)
          let bind x ty =
            let v = Var.create ty in
            Stack.push v (Translate_env.stack env);
            transform_instrs env (Let (v, x) :: generated) xs in
          let nop = transform_instrs env generated xs in

          let label_and_args (n: int32 Source.phrase) =
            let lbl = Translate_env.nth_label (Int32.to_int n.it) env in
            let arity = Label.arity lbl in
            let args = popn arity in
            (lbl, args) in

          (* Ending a block / branch *)
          let end_block lbl tys instrs =
            (* Create argument variables for each argument type. *)
            (* TODO: Incorporate local variables *)
            let arg_vars = List.map Var.create tys |> List.rev in
            (* Push new variables to virtual stack *)
            let stack = Stack.copy (Translate_env.stack env) in
            push_all stack arg_vars;
            let env = Translate_env.with_stack stack env in
            (* Codegen continuation *)
            let term = transform_instrs env [] instrs in
            Cont (lbl, arg_vars, false, term)
          in
          let capture_continuation tys =
            let lbl = Label.create (List.length tys) in
            lbl, end_block lbl tys xs in
          match x.it with
            | Unreachable -> terminate generated Stackless.Unreachable
            | Nop -> nop
            | Drop -> let _ = pop () in nop
            | Select ->
                let cond = pop () in
                let ifnot = pop () in
                let ifso = pop () in
                bind 
                  (Stackless.Select { cond; ifso; ifnot }) 
                  (Var.type_ ifso)
            | Block (tys, is) ->
                (* Capture current continuation *)
                let (cont_lbl, cont) = capture_continuation tys in
                (* Codegen block, with return set to captured continuation *)
                let env =
                  Translate_env.create
                    (Stack.create ())
                    cont_lbl
                    (Translate_env.return env) in
                transform_instrs env (cont :: generated) is
            | Loop (tys, is) ->
                (* Loop: Capture current continuation. Generate loop continuation. *)
                let (cont_lbl, cont) = capture_continuation tys in

                (* Loop continuation creation *)
                let loop_lbl = Label.create 0 in
                let loop_env =
                  Translate_env.create
                    (Stack.create ())
                    loop_lbl
                    (Translate_env.return env) in
                let loop_branch = Branch.create loop_lbl [] in
                let loop_term = transform_instrs loop_env [] is in
                let loop_cont = Cont (loop_lbl, [], true, loop_term) in

                (* Block termination *)
                terminate (loop_cont :: cont :: generated) (Br loop_branch)

            | If (tys, t, f) ->
                (* Pop condition *)
                let cond = pop () in
                let fresh_env () =
                  let stack_copy = Stack.copy (Translate_env.stack env) in
                  Translate_env.create
                    ~stack:stack_copy
                    ~continuation:(Translate_env.continuation env)
                    ~return:(Translate_env.return env) in

                (* For each branch, make a continuation with a fresh label,
                 * containing the instructions in the branch. *)
                let make_branch instrs =
                  let lbl = Label.create 0 in
                  let env = fresh_env () in
                  let transformed = transform_instrs env [] instrs in
                  (* Each branch (currently, in WASM 1.0) may not take any
                   * arguments *)
                  Branch.create lbl [], 
                  Cont (lbl, [], false, transformed) in

                (* Make true and false branches *)
                let branch_t, cont_t = make_branch t in
                let branch_f, cont_f = make_branch f in
                let (cont_lbl, cont) = capture_continuation tys in

                (* Branches don't take arguments; provide labels *)
                let if_term =
                  Stackless.If { cond; ifso = branch_t; ifnot = branch_f } in
                terminate (cont_t :: cont_f :: cont :: generated) if_term
            | Br nesting ->
                let (lbl, args) = label_and_args nesting in
                let branch = Branch.create lbl args in
                terminate generated (Br branch)
            | BrIf nesting ->
                let cond = pop () in
                (* If condition is true, branch, otherwise continue *)
                let (cont_lbl, cont) = capture_continuation [] in
                let (lbl, args) = label_and_args nesting in
                let br_branch = Branch.create lbl args in
                let cont_branch = Branch.create cont_lbl [] in
                let if_term =
                  Stackless.If { cond; ifso = br_branch; ifnot = cont_branch } in
                terminate (cont :: generated) if_term
            | BrTable (vs, def) ->
                (* Grab the index off the stack *)
                let index = pop () in
                (* Create branches for all labels in the table *)
                let make_branch (lbl, args) = Branch.create lbl args in
                let branches =
                  List.map (fun nest -> label_and_args nest |> make_branch) vs in
                (* Create default branch *)
                let default = label_and_args def |> make_branch in
                let brtable_term =
                  Stackless.BrTable { index; es = branches; default } in
                terminate generated brtable_term
            | Return ->
                let arity =
                  Translate_env.continuation env
                  |> Label.arity in
                let returned = popn arity in
                let function_return =
                  Translate_env.return env in
                let branch = Branch.create function_return returned in
                terminate generated (Stackless.Br branch)
            | Call var -> failwith "todo"
            | CallIndirect var -> failwith "todo"
            | GetLocal var -> failwith "todo"
            | SetLocal var -> failwith "todo"
            | TeeLocal var -> failwith "todo"
            | GetGlobal var -> failwith "todo"
            | SetGlobal var -> failwith "todo"
            | Load loadop ->
                let addr = pop () in
                bind (Stackless.Load (loadop, addr)) loadop.ty
            | Store storeop ->
                let arg = pop () in
                let addr = pop () in
                let eff =
                  Stackless.Store { op = storeop; index = addr; value = arg } in
                transform_instrs env (Stackless.Effect eff :: generated) xs
            | MemorySize ->
                bind (Stackless.MemorySize) Types.I32Type
            | MemoryGrow ->
                let amount = pop () in
                bind (Stackless.MemoryGrow amount) Types.I32Type
            | Const literal ->
                let lit = literal.it in
                let ty = Values.type_of lit in
                bind (Stackless.Const lit) ty
            | Test testop ->
                let v = pop () in
                bind (Stackless.Test (testop, v)) (Var.type_ v)
            | Compare relop ->
                let (v2, v1) = pop2 () in
                let (v1ty, v2ty) = (Var.type_ v1, Var.type_ v2) in
                let _ = assert (v1ty = v2ty) in
                bind (Stackless.Compare (relop, v1, v2)) v1ty
            | Unary unop ->
                let v = pop () in
                bind (Stackless.Unary (unop, v)) (Var.type_ v)
            | Binary binop ->
                let (v2, v1) = pop2 () in
                let (v1ty, v2ty) = (Var.type_ v1, Var.type_ v2) in
                let _ = assert (v1ty = v2ty) in
                bind (Stackless.Binary (binop, v1, v2)) v1ty
            | Convert cvtop ->
                let v = pop () in
                bind (Stackless.Convert (cvtop, v)) (Var.type_ v)
          end in
  transform_instrs env instrs []

