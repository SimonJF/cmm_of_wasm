open Stackless
open Libwasm.Source

(* IR generation *)
let terminate 
  (code: Stackless.statement list)
  (terminator: Stackless.terminator) = {
  body = List.rev code;
  terminator = terminator
}

let ir_term env instrs =
  let open Libwasm.Ast in
  let rec transform_instrs env generated (instrs: instr list) =
    let label_and_args env (n: int32 phrase) =
      let lbl = Translate_env.nth_label (Int32.to_int n.it) env in
      let arity = Label.arity lbl in
      let (args, _) = Translate_env.popn arity env in
      (lbl, args) in

    (* Main *)
    match instrs with
      | [] ->
          (* Have:
            * - A list of generated instructions
            * - Function return
            * - Block return *)
          (* Need: term *)
          let cont_label = (Translate_env.continuation env) in
          let arity = Label.arity cont_label in
          let (returned, _) = Translate_env.popn arity env in
          let locals = Translate_env.locals env in
          let return_branch = Branch.create cont_label (returned @ locals) in
          let terminator = Stackless.Br return_branch in
          terminate generated terminator
      | x :: xs ->

          (* Binds a "pushed" variable, records on virtual stack *)
          let bind env x ty =
            let v = Var.create ty in
            let env = Translate_env.push v env in
            transform_instrs env (Let (v, x) :: generated) xs in

          let bind_local (env: Translate_env.t) (var: var) (x: Var.t) =
            let env = Translate_env.set_local var x env in
            transform_instrs env generated xs in

          (* Capturing a continuation *)
          let capture_continuation parameter_tys =
            let lbl = Label.create (List.length parameter_tys) in
            (* Create parameters for each argument type required by continuation. *)
            let arg_params = List.map Var.create parameter_tys in
            (* All continuations take the required parameters, followed
             * by the local variables (as required by SSA) *)
            let local_params = List.map Var.rename (Translate_env.locals env) in
            (* Push parameters onto to virtual stack *)
            let stack = Translate_env.stack env in
            let new_stack = arg_params @ local_params @ stack in
            let env = Translate_env.with_stack new_stack env in
            (* Codegen continuation *)
            let term = transform_instrs env [] instrs in
            let cont = Cont (lbl, arg_params @ local_params, false, term) in
            (lbl, cont) in

          let nop env = transform_instrs env generated xs in

          begin
          match x.it with
            | Unreachable -> terminate generated Stackless.Unreachable
            | Nop -> nop env
            | Drop -> let (_, env) = Translate_env.pop env in nop env
            | Select ->
                let [@warning "-8"] ([cond; ifnot; ifso], env) =
                  Translate_env.popn 3 env in
                bind 
                  env
                  (Stackless.Select { cond; ifso; ifnot }) 
                  (Var.type_ ifso)
            | Block (tys, is) ->
                (* Capture current continuation *)
                let (cont_lbl, cont) = capture_continuation tys in

                (* Codegen block, with return set to captured continuation *)
                let env =
                  env
                  |> Translate_env.with_stack []
                  |> Translate_env.with_continuation cont_lbl in
                transform_instrs env (cont :: generated) is
            | Loop (tys, is) ->
                (* Loop: Capture current continuation. Generate loop continuation. *)
                let (cont_lbl, cont) = capture_continuation tys in

                (* Loop continuation creation *)
                let loop_lbl = Label.create 0 in
                let locals = Translate_env.locals env in
                let loop_params = List.map Var.rename locals in
                let loop_env =
                  env
                  |> Translate_env.with_stack []
                  |> Translate_env.with_continuation cont_lbl
                  |> Translate_env.with_locals loop_params in
                let loop_branch = Branch.create loop_lbl locals in
                let loop_term = transform_instrs loop_env [] is in
                let loop_cont = Cont (loop_lbl, loop_params, true, loop_term) in

                (* Block termination *)
                terminate (loop_cont :: cont :: generated) (Br loop_branch)
            | If (tys, t, f) ->
                (* Pop condition *)
                let (cond, env) = Translate_env.pop env in
                let (cont_lbl, cont) = capture_continuation tys in

                let fresh_env locals =
                  env
                  |> Translate_env.with_stack []
                  |> Translate_env.with_continuation cont_lbl
                  |> Translate_env.with_locals locals in

                (* For each branch, make a continuation with a fresh label,
                 * containing the instructions in the branch. *)
                let make_branch instrs =
                  let lbl = Label.create 0 in
                  let locals = Translate_env.locals env in
                  let local_params = List.map Var.rename locals in
                  let env = fresh_env local_params in
                  let transformed = transform_instrs env [] instrs in
                  (* NOTE: We are currently creating parameters, and immediately
                   * applying the current locals as parameters in the branches!
                   * I'm confident we can optimise this, but being uniform and 
                   * careful to start... *)
                  Branch.create lbl locals, 
                  Cont (lbl, local_params, false, transformed) in

                (* Make true and false branches *)
                let branch_t, cont_t = make_branch t in
                let branch_f, cont_f = make_branch f in

                (* If term: condition variable, and corresponding branch instructions *)
                let if_term =
                  Stackless.If { cond; ifso = branch_t; ifnot = branch_f } in
                (* Finally put the generated continuations on the stack and terminate *)
                terminate (cont_t :: cont_f :: cont :: generated) if_term
            | Br nesting ->
                let (lbl, args) = label_and_args env nesting in
                let branch = Branch.create lbl args in
                terminate generated (Br branch)
            | BrIf nesting ->
                let (cond, env) = Translate_env.pop env in
                (* If condition is true, branch, otherwise continue *)
                let (cont_lbl, cont) = capture_continuation [] in
                let (lbl, args) = label_and_args env nesting in
                let br_branch = Branch.create lbl args in
                let cont_branch = Branch.create cont_lbl [] in
                let if_term =
                  Stackless.If { cond; ifso = br_branch; ifnot = cont_branch } in
                terminate (cont :: generated) if_term
            | BrTable (vs, def) ->
                (* Grab the index off the stack *)
                let (index, env) = Translate_env.pop env in
                (* Create branches for all labels in the table *)
                let make_branch (lbl, args) = Branch.create lbl args in
                let branches =
                  List.map (fun nest ->
                    label_and_args env nest |> make_branch) vs in
                (* Create default branch *)
                let default = label_and_args env def |> make_branch in
                let brtable_term =
                  Stackless.BrTable { index; es = branches; default } in
                terminate generated brtable_term
            | Return ->
                (* NOTE: I don't *think* we need to do anything with
                 * locals here as locals can't escape a function... *)
                let arity =
                  Translate_env.continuation env
                  |> Label.arity in
                let (returned, env) = Translate_env.popn arity env in
                let function_return = Translate_env.return env in
                let branch = Branch.create function_return returned in
                terminate generated (Stackless.Br branch)
            | Call var ->
                let open Libwasm.Types in
                (* TODO: I don't think we need to do locals here? *)
                let func = Translate_env.get_function var env in
                let FuncType (arg_tys, ret_tys) = Func.type_ func in
                (* Capture current continuation *)
                let (cont_lbl, cont) = capture_continuation ret_tys in
                (* Grab args to the function *)
                let (args, env) = Translate_env.popn (List.length arg_tys) env in
                let cont_branch = Branch.create cont_lbl args in
                (* Terminate *)
                terminate (cont :: generated) (Stackless.Call {
                  func; args; cont = cont_branch
                })
            | CallIndirect var -> failwith "todo"
            | GetLocal var ->
                let open Translate_env in
                let env = push (get_local var env) env in
                transform_instrs env generated xs
            | SetLocal var ->
                let (new_val, env) = Translate_env.pop env in
                bind_local env var new_val
            | TeeLocal var -> 
                let (new_val, _) = Translate_env.pop env in
                bind_local env var new_val
            | GetGlobal var ->
                let global = Translate_env.get_global var env in
                bind env (Stackless.GetGlobal global) (Global.type_ global)
            | SetGlobal var ->
                let (arg, env) = Translate_env.pop env in
                let global = Translate_env.get_global var env in
                let stmt = Stackless.Effect (Stackless.SetGlobal (global, arg)) in
                transform_instrs env (stmt :: generated) xs
            | Load loadop ->
                let (addr, env) = Translate_env.pop env in
                bind env (Stackless.Load (loadop, addr)) loadop.ty
            | Store storeop ->
                let (arg, addr), env = Translate_env.pop2 env in
                let eff =
                  Stackless.Store { op = storeop; index = addr; value = arg } in
                transform_instrs env (Stackless.Effect eff :: generated) xs
            | MemorySize ->
                bind env (Stackless.MemorySize) Libwasm.Types.I32Type
            | MemoryGrow ->
                let (amount, env) = Translate_env.pop env in
                bind env (Stackless.MemoryGrow amount) Libwasm.Types.I32Type
            | Const literal ->
                let lit = literal.it in
                let ty = Libwasm.Values.type_of lit in
                bind env (Stackless.Const lit) ty
            | Test testop ->
                let (v, env) = Translate_env.pop env in
                bind env (Stackless.Test (testop, v)) (Var.type_ v)
            | Compare relop ->
                let (v2, v1), env = Translate_env.pop2 env in
                let (v1ty, v2ty) = (Var.type_ v1, Var.type_ v2) in
                let _ = assert (v1ty = v2ty) in
                bind env (Stackless.Compare (relop, v1, v2)) v1ty
            | Unary unop ->
                let (v, env) = Translate_env.pop env in
                bind env (Stackless.Unary (unop, v)) (Var.type_ v)
            | Binary binop ->
                let (v2, v1), env = Translate_env.pop2 env in
                let (v1ty, v2ty) = (Var.type_ v1, Var.type_ v2) in
                let _ = assert (v1ty = v2ty) in
                bind env (Stackless.Binary (binop, v1, v2)) v1ty
            | Convert cvtop ->
                let (v, env) = Translate_env.pop env in
                bind env (Stackless.Convert (cvtop, v)) (Var.type_ v)
          end in
      transform_instrs env [] instrs

let ir_func
    (functions: Func.t Util.Maps.Int32Map.t)
    (globs: (Stackless.global * Global.t) Util.Maps.Int32Map.t)
    (ast_func: Libwasm.Ast.func)
    (func_metadata: Func.t) =
  let open Libwasm.Types in
  let func = ast_func.it in
  let locals = func.locals in
  let (FuncType (arg_tys, ret_tys)) as fty =
    Func.type_ func_metadata in
  let arg_params = List.map (Var.create) arg_tys in
  let local_params = List.map (Var.create) locals in
  let params = arg_params @ local_params in
  let arity = List.length ret_tys in

  let ret = Label.create_return arity in
  let env =
    Translate_env.create
      ~stack:[]
      ~continuation:ret
      ~return:ret
      ~label_stack:[ret]
      ~locals:local_params
      ~globals:globs
      ~functions:functions in

  let body = ir_term env func.body in
  {
    return = ret;
    type_ = fty;
    params;
    body;
  }

let ir_module (ast_mod: Libwasm.Ast.module_) =
    let open Util.Maps in
    let module Ast = Libwasm.Ast in
    let ast_mod = ast_mod.it in
    let types_map =
      List.fold_left (fun (i, acc) ty ->
        let acc = Int32Map.add i (ty.it) acc in
        (Int32.(add i one), acc)) (0l, Int32Map.empty) ast_mod.types |> snd in

    (* Create the function metadata map *)
    let func_metadata_map =
      List.fold_left (fun (i, acc) funcs ->
        let ty = Int32Map.find i types_map in
        let md = Func.create ~name:None ~ty:ty in
        (Int32.(add i one), Int32Map.add i md acc))
      (0l, Int32Map.empty) ast_mod.funcs |> snd in

    (* Next, prepare all of the globals *)
    let globals =
      List.fold_left (fun (i, acc) (glob: Ast.global) ->
        let glob = glob.it in
        (* My understanding from the spec is that this *must*
         * be a single value wrapped in "const"... But I may
         * be wrong *)
        let v =
          match List.map (fun i -> i.it) glob.value.it with
            | [Ast.Const lit] -> lit.it
            | _ -> failwith "FATAL: global with non-constant value" in
        let metadata =
          Global.create ~name:None glob.gtype in
        let ir_glob = { gtype = glob.gtype; value = v } in
        let acc = Int32Map.add i (ir_glob, metadata) acc in
        (Int32.(add i one), acc)) (0l, Int32Map.empty) ast_mod.globals
      |> snd in

    (* Now that that's all sorted, we can compile each function *)
    let funcs =
      let func_metadata =
        List.map snd (Int32Map.bindings func_metadata_map) in
      let zipped = List.combine ast_mod.funcs func_metadata in
      List.fold_left (fun (i, acc) (func, md) ->
        let compiled_func = ir_func func_metadata_map globals func md in
        let acc = Int32Map.add i (compiled_func, md) acc in
        (Int32.(add i one), acc)
      ) (0l, Int32Map.empty) zipped 
      |> snd in

    let start =
        (Libwasm.Lib.Option.map (fun (v: Ast.var) ->
          Int32Map.find (v.it) func_metadata_map)) ast_mod.start in

    (* And for now, that should be it? *)
    { funcs; globals; start }




