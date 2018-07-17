open Stackless
open Libwasm.Source
open Util.Maps

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
      let args = Translate_env.peekn_rev arity env in
      let args =
        if Label.needs_locals lbl then
          args @ (Translate_env.locals env)
        else
          args in
      (lbl, args) in

    let branch_to_continuation cont_label env =
      let arity = Label.arity cont_label in
      let returned = Translate_env.popn_rev arity env in
      let locals = Translate_env.locals env in
      (* Some labels (e.g., function returns and calls) don't need to record
       * new locals *)
      let args =
        if (Label.needs_locals cont_label) then
          returned @ locals
        else
          returned in
      Branch.create cont_label args in


    (* Main *)
    match instrs with
      | [] ->
          let cont_label = (Translate_env.continuation env) in
          let return_branch = branch_to_continuation cont_label env in
          let terminator = Stackless.Br return_branch in
          terminate generated terminator
      | x :: xs ->
          let continuation_empty = (xs = []) in

          (* Binds a "pushed" variable, records on virtual stack *)
          let bind env x ty =
            let v = Var.create ty in
            Translate_env.push v env;
            transform_instrs env (Let (v, x) :: generated) xs in

          let bind_local (env: Translate_env.t) (var: var) (x: Var.t) =
            Translate_env.set_local var x env;
            transform_instrs env generated xs in

          (* Capturing a continuation *)
          let capture_continuation env parameter_tys require_locals =
            (* We require a fresh environment when capturing the continuation,
             * so that it doesn't interfere with creating the remainder of the
             * expression we are translating. *)
            let env = Translate_env.copy env in
            let lbl =
              Label.create
                ~arity:(List.length parameter_tys)
                ~needs_locals:require_locals in
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
            let new_stack = (List.rev arg_params) @ stack in
            Translate_env.set_stack new_stack env;
            let () =
              if require_locals then
                Translate_env.set_locals local_params env else () in
            (* Codegen continuation *)
            let term = transform_instrs env [] xs in
            let cont = Cont (lbl, arg_params @ local_params, false, term) in
            (lbl, [cont]) in

          (* Optimisation: Only capture a continuation if required. *)
          let capture_continuation_if_required env param_tys require_locals =
            if continuation_empty then
              (Translate_env.continuation env, [])
            else
              capture_continuation env param_tys require_locals in

          let nop env = transform_instrs env generated xs in

          begin
          match x.it with
            | Unreachable -> terminate generated Stackless.Unreachable
            | Nop -> nop env
            | Drop -> let _ = Translate_env.pop env in nop env
            | Select ->
                let [@warning "-8"] [cond; ifnot; ifso] =
                  Translate_env.popn 3 env in
                bind
                  env
                  (Stackless.Select { cond; ifso; ifnot })
                  (Var.type_ ifso)
            | Block (tys, is) ->
                (* Capture current continuation, if required *)
                let (cont_lbl, conts) =
                  capture_continuation_if_required env tys true in

                (* Codegen block, with return set to captured continuation, and push label *)
                Translate_env.push_label cont_lbl env;
                Translate_env.set_stack [] env;
                Translate_env.set_continuation cont_lbl env;
                transform_instrs env (conts @ generated) is
            | Loop (tys, is) ->
                (* Loop: Capture current continuation (if required). Generate loop continuation. *)
                let (cont_lbl, conts) =
                  capture_continuation_if_required env tys true in

                (* Loop continuation creation *)
                let loop_lbl = Label.create ~arity:0 ~needs_locals:true in
                let locals = Translate_env.locals env in
                let loop_params = List.map Var.rename locals in
                Translate_env.push_label loop_lbl env;
                Translate_env.set_stack [] env;
                Translate_env.set_continuation cont_lbl env;
                Translate_env.set_locals loop_params env;

                let loop_branch = Branch.create loop_lbl locals in
                let loop_term = transform_instrs env [] is in
                let loop_cont = Cont (loop_lbl, loop_params, true, loop_term) in

                (* Block termination *)
                terminate (loop_cont :: (conts @ generated)) (Br loop_branch)
            | If (tys, t, f) ->
                (* Pop condition *)
                let cond = Translate_env.pop env in
                let (cont_lbl, conts) =
                  capture_continuation_if_required env tys true in

                let fresh_env () =
                  let env_copy = Translate_env.copy env in
                  Translate_env.push_label cont_lbl env_copy;
                  Translate_env.set_stack [] env_copy;
                  Translate_env.set_continuation cont_lbl env_copy;
                  env_copy in

                (* For each branch, make a continuation with a fresh label,
                 * containing the instructions in the branch. *)
                let make_branch instrs =
                  if instrs = [] then
                    (branch_to_continuation cont_lbl env, [])
                  else
                    let lbl = Label.create ~arity:0 ~needs_locals:false in
                    let env = fresh_env () in
                    let transformed = transform_instrs env [] instrs in
                    (Branch.create lbl [],
                     [Cont (lbl, [], false, transformed)]) in

                (* Make true and false branches *)
                let branch_t, cont_t = make_branch t in
                let branch_f, cont_f = make_branch f in

                (* If term: condition variable, and corresponding branch instructions *)
                let if_term =
                  Stackless.If { cond; ifso = branch_t; ifnot = branch_f } in
                (* Finally put the generated continuations on the stack and terminate *)
                terminate (cont_f @ cont_t @ (conts @ generated)) if_term
            | Br nesting ->
                let (lbl, args) = label_and_args env nesting in
                let branch = Branch.create lbl args in
                terminate generated (Br branch)
            | BrIf nesting ->
                let cond = Translate_env.pop env in
                (* If condition is true, branch, otherwise continue *)
                let (cont_lbl, conts) =
                  capture_continuation_if_required env [] false in
                let (lbl, args) = label_and_args env nesting in
                let br_branch = Branch.create lbl args in
                let cont_branch = branch_to_continuation cont_lbl env in
                let if_term =
                  Stackless.If { cond; ifso = br_branch; ifnot = cont_branch } in
                terminate (conts @ generated) if_term
            | BrTable (vs, def) ->
                (* Grab the index off the stack *)
                let index = Translate_env.pop env in
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
                (* We don't need to do anything with locals here as locals
                 * can't escape a function. *)
                let function_return = Translate_env.return env in
                let arity = Label.arity function_return in
                let returned = Translate_env.popn_rev arity env in
                let branch = Branch.create function_return returned in
                terminate generated (Stackless.Br branch)
            | Call var ->
                let open Libwasm.Types in
                let func = Translate_env.get_function var env in
                let FuncType (arg_tys, ret_tys) = Func.type_ func in
                (* Grab args to the function *)
                let args = Translate_env.popn_rev (List.length arg_tys) env in
                (* Capture current continuation *)
                (* TODO: Maybe we can optimise this to not capture an
                 * empty continuation, but "call" is slightly more complicated *)
                let (cont_lbl, conts) =
                  capture_continuation env ret_tys false in
                let cont_branch = Branch.create cont_lbl [] in
                (* Terminate *)
                terminate (conts @ generated) (Stackless.Call {
                  func; args; cont = cont_branch
                })
            | CallIndirect var ->
                let open Libwasm.Types in
                let ty = Translate_env.get_type var env in
                let (FuncType (arg_tys, ret_tys)) = ty in
                let func_id = Translate_env.pop env in
                let args = Translate_env.popn_rev (List.length arg_tys) env in
                (* Capture current continuation *)
                let (cont_lbl, conts) =
                  capture_continuation env ret_tys false in
                let cont_branch = Branch.create cont_lbl [] in
                (* Terminate *)
                terminate (conts @ generated) (Stackless.CallIndirect {
                  type_ = ty; func = func_id; args; cont = cont_branch
                })
            | GetLocal var ->
                let open Translate_env in
                push (get_local var env) env;
                transform_instrs env generated xs
            | SetLocal var ->
                let new_val = Translate_env.pop env in
                bind_local env var new_val
            | TeeLocal var ->
                let new_val = Translate_env.peek env in
                bind_local env var new_val
            | GetGlobal var ->
                let global = Translate_env.get_global var env in
                bind env (Stackless.GetGlobal global) (Global.type_ global)
            | SetGlobal var ->
                let arg = Translate_env.pop env in
                let global = Translate_env.get_global var env in
                let stmt = Stackless.Effect (Stackless.SetGlobal (global, arg)) in
                transform_instrs env (stmt :: generated) xs
            | Load loadop ->
                let addr = Translate_env.pop env in
                bind env (Stackless.Load (loadop, addr)) loadop.ty
            | Store storeop ->
                let (arg, addr) = Translate_env.pop2 env in
                let eff =
                  Stackless.Store { op = storeop; index = addr; value = arg } in
                transform_instrs env (Stackless.Effect eff :: generated) xs
            | MemorySize ->
                bind env (Stackless.MemorySize) Libwasm.Types.I32Type
            | MemoryGrow ->
                let amount = Translate_env.pop env in
                bind env (Stackless.MemoryGrow amount) Libwasm.Types.I32Type
            | Const literal ->
                let lit = literal.it in
                let ty = Libwasm.Values.type_of lit in
                bind env (Stackless.Const lit) ty
            | Test testop ->
                let v = Translate_env.pop env in
                bind env (Stackless.Test (testop, v)) Libwasm.Types.I32Type
            | Compare relop ->
                let (v2, v1) = Translate_env.pop2 env in
                let (v1ty, v2ty) = (Var.type_ v1, Var.type_ v2) in
                let _ = assert (v1ty = v2ty) in
                bind env (Stackless.Compare (relop, v1, v2)) Libwasm.Types.I32Type
            | Unary unop ->
                let v = Translate_env.pop env in
                bind env (Stackless.Unary (unop, v)) (Var.type_ v)
            | Binary binop ->
                let (v2, v1) = Translate_env.pop2 env in
                let (v1ty, v2ty) = (Var.type_ v1, Var.type_ v2) in
                (* As above *)
                let _ = assert (v1ty = v2ty) in
                bind env (Stackless.Binary (binop, v1, v2)) v1ty
            | Convert cvtop ->
                let conversion_result_type =
                  begin
                  let open Libwasm.Types in
                  let open Libwasm.Values in
                  match cvtop with
                    | I32 _ -> I32Type
                    | I64 _ -> I64Type
                    | F32 _ -> F32Type
                    | F64 _ -> F64Type
                  end in
                let v = Translate_env.pop env in
                bind
                  env
                  (Stackless.Convert (cvtop, v))
                  conversion_result_type
          end in
      transform_instrs env [] instrs

let bind_locals params locals =
  (* Firstly, add locals entries for parameters, mapping to the parameter names. *)
  let (i, local_map) =
    List.fold_left (fun (i, m) arg_v ->
      (Int32.(add i one), Int32Map.add i arg_v m)
    ) (Int32.zero, Int32Map.empty) params in

  (* Finally, add locals entries for locals, let-bind them to default values,
   * and bind them to the fresh variable names. *)
  let (_, instrs_rev, local_map) =
    List.fold_left (fun (i, instrs, local_map) ty ->
      let v = Var.create ty in
      let x = Stackless.Const (Libwasm.Values.default_value ty) in
      let local_map = Int32Map.add i v local_map in
      let instr = Let (v, x) in
      (Int32.(add i one), instr :: instrs, local_map)
    ) (i, [], local_map) locals in
  ((List.rev instrs_rev), local_map)

let ir_func
    (functions: Func.t Int32Map.t)
    (globs: Global.t Int32Map.t)
    (ast_func: Libwasm.Ast.func)
    (func_metadata: Func.t)
    (type_map: Libwasm.Types.func_type Int32Map.t) =
  let open Libwasm.Types in
  let func = ast_func.it in
  let (FuncType (arg_tys, ret_tys)) as fty =
    Func.type_ func_metadata in
  (* Create parameter names for each argument type *)
  let arg_params = List.map (Var.create) arg_tys in

  (* Create return label *)
  let arity = List.length ret_tys in
  let ret = Label.create_return ~arity in

  (* Populate locals, and set them to their default values. *)
  let locals = func.locals in
  let (let_bindings, local_map) = bind_locals arg_params locals in

  (* Create initial environment with empty stack and locals. *)
  let env : Translate_env.t =
    Translate_env.create
      ~stack:[]
      ~continuation:ret
      ~return:ret
      ~locals:local_map
      ~globals:globs
      ~types:type_map
      ~functions:functions in

  let fn_body = ir_term env func.body in
  let fn_body = { fn_body with body = let_bindings @ fn_body.body } in
  {
    return = ret;
    type_ = fty;
    params = arg_params;
    body = fn_body;
  }

(* Resolves a WASM global to its payload: either a WASM constant,
 * or a reference to another global *)
let global_initialiser globals const =
  match List.map (fun i -> i.it) const with
    | [Libwasm.Ast.Const lit] -> Global.Constant lit.it
    | [Libwasm.Ast.GetGlobal i] ->
        let g = Int32Map.find (i.it) globals in
        Global.AnotherGlobal g
    | _ -> failwith "expected constant of length 1"

(* FIXME: This is one looooong function. It started small but has
 * grown. It should be split up. *)
let ir_module (ast_mod: Libwasm.Ast.module_) =
    let module Ast = Libwasm.Ast in
    let ast_mod = ast_mod.it in
    let types_map =
      List.fold_left (fun (i, acc) ty ->
        let acc = Int32Map.add i (ty.it) acc in
        (Int32.(add i one), acc)) (0l, Int32Map.empty) ast_mod.types |> snd in

    (* Next, handle imports. *)
    (* Validation should ensure that a table / memory is not both defined
     * _and_ imported. If the module isn't validated then we take the
     * defined (non-imported) table / memory. *)
    let (func_metadata_map, func_import_count,
         globals, global_import_count, table, memory_metadata) =
      let open Libwasm.Ast in
      List.fold_left (fun (fs, f_idx, gs, g_idx, t, m) (imp: import) ->
        let imp = imp.it in
        let decode_and_sanitise s =
          let open Util.Names in
          s |> string_of_name |> sanitise in
        let module_name = decode_and_sanitise imp.module_name in
        let import_name = decode_and_sanitise imp.item_name in

        match imp.idesc.it with
          | FuncImport ty_var ->
              let ty = Int32Map.find ty_var.it types_map in
              let func_metadata =
                Func.create_imported
                  ~module_name
                  ~function_name:import_name ty in
              let funcs = Int32Map.add f_idx func_metadata fs in
              (funcs, Int32.(add f_idx one), gs, g_idx, t, m)
          | TableImport (TableType (lims, _)) ->
              let table =
                Some (ImportedTable {
                  module_name;
                  table_name = import_name;
                  limits = lims
                }) in
              (fs, f_idx, gs, g_idx, table, m)
          | MemoryImport (MemoryType lims)->
              let memory =
                Some (ImportedMemory {
                  module_name;
                  memory_name = import_name;
                  limits = lims
                }) in
              (fs, f_idx, gs, g_idx, t, memory)
          | GlobalImport gty ->
              let g =
                Global.create_imported
                  ~module_name
                  ~global_name:import_name
                  ~ty:gty in
              let gs = Int32Map.add g_idx g gs in
              (fs, f_idx, gs, Int32.(add g_idx one), t, m)
      ) (Int32Map.empty, Int32.zero,
         Int32Map.empty, Int32.zero,
         None, None) ast_mod.imports in

    (* Next, prepare all of the defined globals *)
    let globals =
      List.fold_left (fun (i, acc) (glob: Ast.global) ->
        let glob = glob.it in
        let initialiser = global_initialiser acc glob.value.it in
        let ir_global =
          Global.create_defined
            ~name:None
            ~ty:glob.gtype
            ~initial_value:initialiser in
        let acc = Int32Map.add i ir_global acc in
        (Int32.(add i one), acc)) (global_import_count, globals) ast_mod.globals
      |> snd in

    let exports = ast_mod.exports in
    let memory_metadata =
      match ast_mod.memories with
        | [] -> memory_metadata
        | x :: _ ->
            Some (LocalMemory x.it.mtype) in

    let table =
      let open Libwasm.Types in
      match ast_mod.tables with
        | [] -> table
        | t :: _ ->
            let TableType (limits, _) = t.it.ttype in
            Some (LocalTable limits) in

    (* Update the function metadata map with defined functions,
     * starting after the indexing space of the imported functions *)
    let func_metadata_map =
      List.fold_left (fun (i, acc) (func: Libwasm.Ast.func) ->
        let ty = Int32Map.find (func.it.ftype.it) types_map in
        let md = Func.create_defined ty in
        (Int32.(add i one), Int32Map.add i md acc))
      (func_import_count, func_metadata_map) ast_mod.funcs |> snd in

    (* Given a (possibly-empty) integer constant expression, return
     * either the constant value, a global reference,
     * or the default i32 value *)
    let transform_i32_const : Libwasm.Ast.const -> expr = fun x ->
      match List.map (fun x -> x.it) x.it with
        | [] -> Const (Libwasm.Values.default_value Libwasm.Types.I32Type)
        | [Libwasm.Ast.GetGlobal i] -> GetGlobal (Int32Map.find i.it globals)
        | [Libwasm.Ast.Const lit] -> Const (lit.it)
        | _-> failwith "expected constant of length <= 1" in

    let table_elems =
      let process_segment (seg: Libwasm.Ast.table_segment) =
        let seg = seg.it in
        let offset = transform_i32_const seg.offset in
        (* Now, we can populate the elems map *)
        let vars_list = seg.init in
        let map =
          List.fold_left (fun (i, acc) var ->
            let func = Int32Map.find var.it func_metadata_map in
            let new_elems = Int32Map.add i func acc in
            (Int32.(add i one), new_elems)) (Int32.zero, Int32Map.empty) vars_list |> snd in
        (offset, map) in
      List.map (process_segment) (ast_mod.elems) in


    (* Next up, add all of the data definitions *)
    let data =
      let open Libwasm.Ast in
      List.map (fun (data_seg: string segment) ->
        let data_seg = data_seg.it in
        let offset = transform_i32_const data_seg.offset in
        { offset; contents = data_seg.init }) ast_mod.data in

    (* Now that that's all sorted, we can compile each function *)
    let function_ir =
      List.fold_left (fun (i, acc) func ->
        let md = Int32Map.find i func_metadata_map in
        let compiled_func = ir_func func_metadata_map globals func md types_map in
        let acc = Int32Map.add i compiled_func acc in
        (Int32.(add i one), acc)
      ) (func_import_count, Int32Map.empty) ast_mod.funcs
      |> snd in

    (* Grab the start function, if one exists *)
    let start =
        (Libwasm.Lib.Option.map (fun (v: Ast.var) ->
          Int32Map.find (v.it) func_metadata_map)) ast_mod.start in

    (* And for now, that should be it? *)
    { function_metadata = func_metadata_map;
      imported_function_count = Int32.to_int func_import_count;
      function_ir;
      globals;
      start;
      memory_metadata;
      exports;
      data;
      table;
      table_elems }


