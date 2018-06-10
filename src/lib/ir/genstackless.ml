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
    let label_and_args env (n: Libwasm.Ast.var) =
      let lbl = Translate_env.nth_label n env in
      let arity = Label.arity lbl in
      let (args, _) = Translate_env.popn_rev arity env in
      let args =
        if Label.needs_locals lbl then
          args @ (Translate_env.locals env)
        else
          args in
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
          let (returned, _) = Translate_env.popn_rev arity env in
          let locals = Translate_env.locals env in
          (* Some labels (e.g., function returns and calls) don't need to record
           * new locals *)
          let args =
            if (Label.needs_locals cont_label) then
              returned @ locals
            else
              returned in
          let return_branch = Branch.create cont_label args in
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
          let capture_continuation env parameter_tys require_locals =
            let lbl = Label.create (List.length parameter_tys) require_locals in
            (* Create parameters for each argument type required by continuation. *)
            let arg_params = List.map Var.create parameter_tys in
            (* All continuations take the required parameters.
             * Some require the local parameters for SSA form as well.
             * Calls don't, as calls do not change locals, but blocks may. *)
            let local_params =
              if require_locals then
                List.map Var.rename (Translate_env.locals env)
              else [] in
            (* Push parameters onto to virtual stack *)
            let stack = Translate_env.stack env in
            (* TODO: I think that the argument parameters be reversed... *)
            let new_stack = arg_params @ stack in
            let env =
              let base_env = Translate_env.with_stack new_stack env in
              if require_locals then
                Translate_env.with_locals local_params base_env
              else base_env in
            (* Codegen continuation *)
            let term = transform_instrs env [] xs in
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
                let (cont_lbl, cont) = capture_continuation env tys true in
                (* Codegen block, with return set to captured continuation, and push label *)
                let env =
                  env
                  |> Translate_env.push_label cont_lbl
                  |> Translate_env.with_stack []
                  |> Translate_env.with_continuation cont_lbl in
                transform_instrs env (cont :: generated) is
            | Loop (tys, is) ->
                (* Loop: Capture current continuation. Generate loop continuation. *)
                let (cont_lbl, cont) = capture_continuation env tys true in

                (* Loop continuation creation *)
                let loop_lbl = Label.create 0 true in
                let locals = Translate_env.locals env in
                let loop_params = List.map Var.rename locals in
                let loop_env =
                  env
                  |> Translate_env.push_label loop_lbl
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
                let (cont_lbl, cont) = capture_continuation env tys true in

                let fresh_env =
                  env
                  |> Translate_env.push_label cont_lbl
                  |> Translate_env.with_stack []
                  |> Translate_env.with_continuation cont_lbl in

                (* For each branch, make a continuation with a fresh label,
                 * containing the instructions in the branch. *)
                let make_branch instrs =
                  let lbl = Label.create 0 false in
                  let env = fresh_env in
                  let transformed = transform_instrs env [] instrs in
                  (Branch.create lbl [],
                   Cont (lbl, [], false, transformed)) in

                (* Make true and false branches *)
                let branch_t, cont_t = make_branch t in
                let branch_f, cont_f = make_branch f in

                (* If term: condition variable, and corresponding branch instructions *)
                let if_term =
                  Stackless.If { cond; ifso = branch_t; ifnot = branch_f } in
                (* Finally put the generated continuations on the stack and terminate *)
                terminate (cont_f :: cont_t :: cont :: generated) if_term
            | Br nesting ->
                let (lbl, args) = label_and_args env nesting in
                let branch = Branch.create lbl args in
                terminate generated (Br branch)
            | BrIf nesting ->
                let (cond, env) = Translate_env.pop env in
                (* If condition is true, branch, otherwise continue *)
                let (cont_lbl, cont) = capture_continuation env [] false in
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
                (* NOTE: We don't need to do anything with
                 * locals here as locals can't escape a function. *)
                let arity =
                  Translate_env.return env
                  |> Label.arity in
                let (returned, env) = Translate_env.popn_rev arity env in
                let function_return = Translate_env.return env in
                let branch = Branch.create function_return returned in
                terminate generated (Stackless.Br branch)
            | Call var ->
                let open Libwasm.Types in
                let func = Translate_env.get_function var env in
                let FuncType (arg_tys, ret_tys) = Func.type_ func in
                (* TODO: Optimise for empty continuations, taking into account
                 * logic in "[]" case *)
                (* Capture current continuation *)
                let (cont_lbl, cont) = capture_continuation env ret_tys false in
                (* Grab args to the function *)
                let (args, env) = Translate_env.popn_rev (List.length arg_tys) env in
                let cont_branch = Branch.create cont_lbl [] in
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
                bind env (Stackless.Test (testop, v)) Libwasm.Types.I32Type
            | Compare relop ->
                let (v2, v1), env = Translate_env.pop2 env in
                let (v1ty, v2ty) = (Var.type_ v1, Var.type_ v2) in
                let _ = Translate_env.dump_stack env in
                let _ = assert (v1ty = v2ty) in
                bind env (Stackless.Compare (relop, v1, v2)) Libwasm.Types.I32Type
            | Unary unop ->
                let (v, env) = Translate_env.pop env in
                bind env (Stackless.Unary (unop, v)) (Var.type_ v)
            | Binary binop ->
                let (v2, v1), env = Translate_env.pop2 env in
                let (v1ty, v2ty) = (Var.type_ v1, Var.type_ v2) in
                let _ = Translate_env.dump_stack env in
                let _ = assert (v1ty = v2ty) in
                bind env (Stackless.Binary (binop, v1, v2)) v1ty
            | Convert cvtop ->
                let (v, env) = Translate_env.pop env in
                bind env (Stackless.Convert (cvtop, v)) (Var.type_ v)
          end in
      transform_instrs env [] instrs

let bind_locals env params locals =
  let var i = Libwasm.Source.(i @@ no_region) in
  (* Firstly, add locals entries for parameters, mapping to the parameter names. *)
  let (i, env) =
    List.fold_left (fun (i, env) arg_v ->
      (Int32.(add i one), Translate_env.set_local (var i) arg_v env)
    ) (Int32.zero, env) params in

  (* Finally, add locals entries for locals, let-bind them to default values,
   * and bind them to the fresh variable names. *)
  let (_, instrs_rev, env) =
    List.fold_left (fun (i, instrs, env) ty ->
      let v = Var.create ty in
      let x = Stackless.Const (Libwasm.Values.default_value ty) in
      let env = Translate_env.set_local (var i) v env in
      let instr = Let (v, x) in
      (Int32.(add i one), instr :: instrs, env)
    ) (i, [], env) locals in
  ((List.rev instrs_rev), env)

let ir_func
    (functions: Func.t Util.Maps.Int32Map.t)
    (globs: (Stackless.global * Global.t) Util.Maps.Int32Map.t)
    (ast_func: Libwasm.Ast.func)
    (func_metadata: Func.t) =
  let open Libwasm.Types in
  let func = ast_func.it in
  let (FuncType (arg_tys, ret_tys)) as fty =
    Func.type_ func_metadata in
  (* Create parameter names for each argument type *)
  let arg_params = List.map (Var.create) arg_tys in

  (* Create return label *)
  let arity = List.length ret_tys in
  let ret = Label.create_return arity in

  (* Create initial environment with empty stack and locals. *)
  let env : Translate_env.t =
    Translate_env.create
      ~stack:[]
      ~continuation:ret
      ~return:ret
      ~locals:Util.Maps.Int32Map.empty
      ~globals:globs
      ~functions:functions in

  (* Populate locals, and set them to their default values. *)
  let locals = func.locals in
  let (let_bindings, env) = bind_locals env arg_params locals in
  let fn_body = ir_term env func.body in
  let fn_body = { fn_body with body = let_bindings @ fn_body.body } in
  {
    return = ret;
    type_ = fty;
    params = arg_params;
    body = fn_body;
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
      List.fold_left (fun (i, acc) (func: Libwasm.Ast.func) ->
        let ty = Int32Map.find (func.it.ftype.it) types_map in
        let md = Func.create ty in
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

    (* Next, traverse exports and name function metadata *)
    let func_metadata_map =
      let open Libwasm.Ast in
      List.fold_left (fun acc export ->
        let export = export.it in
        match export.edesc.it with
          | FuncExport v ->
              let v = v.it in
              let md = Int32Map.find v acc in
              begin
                match Func.name md with
                  | Some _fn_name ->
                      (* Already added *)
                      acc
                  | None ->
                      (* Re-add function with export name *)
                      let md = Func.with_name md export.name in
                      Int32Map.add v md acc
              end
          | _ -> (* Other exports don't matter on this pass *) acc
      ) func_metadata_map ast_mod.exports in

    (* Now that that's all sorted, we can compile each function *)
    let funcs =
      List.fold_left (fun (i, acc) func ->
        let md = Int32Map.find i func_metadata_map in
        (* Printf.printf "Compiling %s\n%!" ( match Func.name md with | Some x -> x | None -> "unnamed"); *)
        let compiled_func = ir_func func_metadata_map globals func md in
        let acc = Int32Map.add i (compiled_func, md) acc in
        (Int32.(add i one), acc)
      ) (0l, Int32Map.empty) ast_mod.funcs
      |> snd in

    (* Finally, grab the start function, if one exists *)

    let start =
        (Libwasm.Lib.Option.map (fun (v: Ast.var) ->
          Int32Map.find (v.it) func_metadata_map)) ast_mod.start in

    let exports = ast_mod.exports in
    (* And for now, that should be it? *)
    { funcs; globals; start ; exports }




