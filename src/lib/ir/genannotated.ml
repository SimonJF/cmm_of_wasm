(* First IR generation pass. Strips positional information and
 * annotates blocks with metadata including the local variables
 * set in that block and any sub-blocks. *)
open Util.Sets

type env = {
  modified_locals: int32set
}

let empty_env = {
  modified_locals = Int32Set.empty
}

let merge_locals locals env =
  { modified_locals = Int32Set.union env.modified_locals locals }

let add_modified_local v env =
  { modified_locals = Int32Set.add v env.modified_locals }

let rec translate_instrs env generated (xs: Libwasm.Ast.instr list) =
  let open Libwasm.Ast in
  match xs with
    | [] -> (List.rev generated, env.modified_locals)
    | x :: xs ->
      let x = x.it in
      let go x = translate_instrs env (x :: generated) xs in
      match x with
        | Unreachable -> go Annotated.Unreachable
        | Nop -> go Annotated.Nop
        | Drop -> go Annotated.Drop
        | Select -> go Annotated.Select
        | Block (block_stack_ty, instrs) ->
            let (block_generated, block_mutated_locals) =
              translate_instrs empty_env [] instrs in
            let new_env = merge_locals block_mutated_locals env in
            let block = Annotated.Block {
              block_stack_ty;
              block_mutated_locals;
              block_instrs = block_generated
            } in
            translate_instrs new_env (block :: generated) xs
        | Loop (block_stack_ty, instrs) ->
            let (block_generated, block_mutated_locals) =
              translate_instrs empty_env [] instrs in
            let new_env = merge_locals block_mutated_locals env in
            let loop = Annotated.Loop {
              block_stack_ty;
              block_mutated_locals;
              block_instrs = block_generated
            } in
            translate_instrs new_env (loop :: generated) xs
        | If (conditional_stack_ty, t, f) ->
            let (gen_t, mutated_t) = translate_instrs empty_env [] t in
            let (gen_f, mutated_f) = translate_instrs empty_env [] f in
            let combined_mutated = Int32Set.union mutated_t mutated_f in
            let if_ = Annotated.If {
              conditional_stack_ty;
              conditional_mutated_locals = combined_mutated;
              conditional_true_instrs = gen_t;
              conditional_false_instrs = gen_f } in
            let new_env = merge_locals combined_mutated env in
            translate_instrs new_env (if_ :: generated) xs
        | Br v -> go (Annotated.Br v.it)
        | BrIf v -> go (Annotated.BrIf v.it)
        | BrTable (vs, v) ->
            let vs = List.map (fun (v: Libwasm.Ast.var) -> v.it) vs in
            go (Annotated.BrTable (vs, v.it))
        | Return -> go Annotated.Return
        | Call v -> go (Annotated.Call (v.it))
        | CallIndirect v -> go (Annotated.CallIndirect (v.it))
        | GetLocal v -> go (Annotated.GetLocal (v.it))
        | SetLocal v ->
            let v = v.it in
            let env = add_modified_local v env in
            translate_instrs env (Annotated.SetLocal v :: generated) xs
        | TeeLocal v -> go (Annotated.TeeLocal (v.it))
        | GetGlobal v -> go (Annotated.GetGlobal (v.it))
        | SetGlobal v -> go (Annotated.SetGlobal (v.it))
        | Load lop -> go (Annotated.Load lop)
        | Store sop -> go (Annotated.Store sop)
        | MemorySize -> go (Annotated.MemorySize)
        | MemoryGrow -> go (Annotated.MemoryGrow)
        | Const lit -> go (Annotated.Const (lit.it))
        | Test testop -> go (Annotated.Test testop)
        | Compare relop -> go (Annotated.Compare relop)
        | Unary unop -> go (Annotated.Unary unop)
        | Binary binop -> go (Annotated.Binary binop)
        | Convert cvtop -> go (Annotated.Convert cvtop)

let translate_module (wasm_mod: Libwasm.Ast.module_) =
  let open Annotated in
  let wasm_mod = wasm_mod.it in
  let translate_const const =
    fst (translate_instrs empty_env [] const) in

  let globals =
    List.map (fun (glob: Libwasm.Ast.global) ->
      let glob = glob.it in
      let value = translate_const glob.value.it in
      { gtype = glob.gtype ; value }) wasm_mod.globals in
  let imports =
    List.map (fun (imp: Libwasm.Ast.import) ->
      let imp = imp.it in
      let idesc =
        let open Libwasm.Ast in
        match imp.idesc.it with
          | FuncImport v -> Annotated.FuncImport v.it
          | TableImport tty -> Annotated.TableImport tty
          | MemoryImport mty -> Annotated.MemoryImport mty
          | GlobalImport gty -> Annotated.GlobalImport gty in
      { module_name = imp.module_name; item_name = imp.item_name; idesc })
      wasm_mod.imports in
  let exports =
    List.map (fun (exp: Libwasm.Ast.export) ->
      let exp = exp.it in
      let edesc =
        let open Libwasm.Ast in
        match exp.edesc.it with
          | FuncExport v -> Annotated.FuncExport v.it
          | TableExport v -> Annotated.TableExport v.it
          | MemoryExport v -> Annotated.MemoryExport v.it
          | GlobalExport v -> Annotated.GlobalExport v.it in
      { name = exp.name; edesc }) wasm_mod.exports in
  let funcs =
    List.map (fun (func: Libwasm.Ast.func) ->
      let func = func.it in
      let (body, _) = translate_instrs empty_env [] func.body in
      { ftype = func.ftype.it;
        locals = func.locals; body }) wasm_mod.funcs in
  let elems =
    List.map (fun (elem: (Libwasm.Ast.var list Libwasm.Ast.segment)) ->
      let elem = elem.it in
      let index = elem.index.it in
      let offset = translate_const elem.offset.it in
      let data = List.map (fun (x: Libwasm.Ast.var) -> x.it) elem.init in
      { index; offset; init = data }) wasm_mod.elems in
  let data =
    List.map (fun (datum: string Libwasm.Ast.segment) ->
      let datum = datum.it in
      let index = datum.index.it in
      let (offset, _) = translate_instrs empty_env [] datum.offset.it in
      { index; offset; init = datum.init }) wasm_mod.data in
  let types = List.map (fun (x: Libwasm.Ast.type_) -> x.it) wasm_mod.types in
  let tables = List.map (fun (x: Libwasm.Ast.table) ->
    { ttype = x.it.ttype } ) wasm_mod.tables in
  let memories = List.map (fun (x: Libwasm.Ast.memory) ->
    { mtype = x.it.mtype } ) wasm_mod.memories in
  let start =
    Libwasm.Lib.Option.map
      (fun (x: Libwasm.Ast.var) -> x.it) wasm_mod.start in
  {
    types;
    globals;
    tables;
    memories;
    funcs;
    start;
    elems;
    data;
    imports;
    exports
  }
