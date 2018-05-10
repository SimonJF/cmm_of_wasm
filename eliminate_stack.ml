(* Compilation pass: WASM AST -> Stackless *)
(* Compiles away the stack, meaning that there are explicit operands
 * for all WASM constructs. *)

open Util
open Libwasm
open Ir

let fresh_name = gensym "_stackless"

let bind stack stackless_instr =
  let open Stackless in
  let name = fresh_name () in
  Stack.push name stack;
  [Let (name, stackless_instr)]

let bind_literal stack lit =
  bind stack (Const lit)

let popn stack n =
  let rec go m =
    if m <= 0 then [] else
    begin
      let v = Stack.pop stack in
      v :: (go (m - 1))
    end in
  go n

let rec compile_instrs (instrs: Libwasm.Ast.instr list) : Stackless.stackless_instr list =
    let stack = Stack.create () in

    let compile_instr stack instr = 
      let open Libwasm.Ast in
      match instr with
        | Unreachable -> [Stackless.Unreachable] (* trap unconditionally *)
        | Nop -> [] (* eliminate nops *)
        | Drop ->
            let _ = Stack.pop stack in [] (* forget a value *)
        | Select ->
            let [cond; v2; v1] = popn stack 3 in
            let open Stackless in
            [Select {
              v1 = v1;
              v2 = v2;
              conditional = cond
            }]
        | Block (stack_ty, instrs) ->
            let compiled_instrs = compile_instrs instrs in
            let compiled_block = Stackless.Block compiled_instrs in
            if (List.length stack_ty = 0) then
              [compiled_block]
            else
              bind stack compiled_block
        | Loop (stack_ty, instrs) ->
            let compiled_instrs = compile_instrs instrs in
            let compiled_loop = Stackless.Loop compiled_instrs in
            if (List.length stack_ty = 0) then
              [compiled_loop]
            else
              bind stack compiled_loop
        | If (stack_ty, then_branch, else_branch) ->
            (* Currently, stack_ty is defined as a list of *output*
             * types, which may either be of length 0 or 1. *)
            let cond_var = Stack.pop stack in
            let compiled_then = compile_instrs then_branch in
            let compiled_else = compile_instrs else_branch in
            let compiled_if = Stackless.If (cond_var, compiled_then, compiled_else) in
            if (List.length stack_ty = 0) then
              [compiled_if]
            else
              bind stack compiled_if
        | Return ->
            (* TODO: To handle functions, we will need to devise a way of
             * returning multiple arguments. I think it would make sense,
             * therefore, to generalise Stackless.Return to take a list?
             * In any case, getting the compilation pipeline working is my
             * first goal. *)
            failwith "functions not yet implemented"
        | Br var -> [Stackless.Br var.it]
        | BrIf var ->
            let cond = Stack.pop stack in
            [Stackless.BrIf (var.it, cond)]
        | BrTable (vs, v) ->
            let operand = Stack.pop stack in
            let open Stackless in
            let br_ty = {
              labels = (List.map (fun (v: Ast.var) -> v.it) vs);
              default = v.it;
              operand = operand
            } in
            [Stackless.BrTable br_ty]
        | Call v -> [Stackless.Call v.it]
        | CallIndirect v ->
            let operand = Stack.pop stack in
            [Stackless.CallIndirect (v.it, operand)]
        | GetGlobal var ->
            bind stack (Stackless.GetGlobal var.it)
        | SetGlobal var ->
            let new_val = Stack.pop stack in
            [Stackless.SetGlobal (var.it, new_val)]
        | Load load_op ->
            let addr = Stack.pop stack in
            let open Stackless in
            let memory_op = {
              ty = load_op.ty;
              align = load_op.align;
              offset = load_op.offset;
              sz = load_op.sz
            } in

            let load_op = {
              memory_operation = memory_op;
              address = addr
            } in
            bind stack (Stackless.Load load_op)
        | Store store_op ->
            let open Stackless in
            let [to_store; addr] = popn stack 2 in
            let memory_op = {
              ty = store_op.ty;
              align = store_op.align;
              offset = store_op.offset;
              sz = store_op.sz
            } in
            let store_op = {
              pack_size = memory_op;
              address = addr;
              to_store = to_store
            } in
            [Stackless.Store store_op]
        | MemorySize -> bind stack Stackless.MemorySize
        | MemoryGrow ->
            let grow_amount = Stack.pop stack in
            bind stack (Stackless.MemoryGrow grow_amount)
        | GetLocal var ->
            let open Stackless in
            bind stack (Stackless.GetLocal var.it)
        | SetLocal var ->
            let new_val = Stack.pop stack in
            let open Stackless in
            [Stackless.SetLocal (var.it, new_val)]
        | TeeLocal var ->
            let new_val = Stack.top stack in
            let open Stackless in
            [Stackless.SetLocal (var.it, new_val)]
        (* read local variable *)
        | Const lit -> 
            let open Stackless in
            bind stack (Stackless.Const lit.it)
        | Test test_op ->
            (* v2 added most recently to stack *)
            let [v2; v1] = popn stack 2 in
            let open Stackless in
            bind stack (Test (test_op, v1, v2))
        | Compare rel_op ->
            let [v2; v1] = popn stack 2 in
            let open Stackless in
            bind stack (Compare (rel_op, v1, v2))
        | Unary op ->
            let v = Stack.pop stack in
            let open Stackless in
            bind stack (Unary (op, v))
        | Binary op ->
            let [v2; v1] = popn stack 2 in
            let open Stackless in
            bind stack (Binary (op, v1, v2))
        | Convert cvt ->
            let v = Stack.pop stack in
            let open Stackless in
            bind stack (Convert (cvt, v))
        | _ -> failwith "unimplemented, alas" in

    let rec go acc (instrs: Libwasm.Ast.instr list) =
      match instrs with
        | [] ->
            (* The stack may well be empty if a block is
             * merely side-effecting. In this case, we currently just
             * don't generate a return. Maybe we want to require a return
             * (and thus move to let-in instead of just let), and generalise
             * return to take an optional argument.
             *
             * Additionally, in future, WASM may support returning multiple
             * values from a block. We're currently assuming a single one. *)
            if not (Stack.is_empty stack) then
              let result = Stack.pop stack in
              (Stackless.Return result) :: acc |> List.rev
            else
              List.rev acc
        | instr :: instrs ->
            let instr' = instr.it in
            let compiled_instrs = compile_instr stack instr' in
            go (compiled_instrs @ acc) instrs in
    go [] instrs

let compile_const (const: Ast.const) = compile_instrs const.it

let compile_globals (globals: Ast.global list) : (Stackless.stackless_instr global) list =
  let compile_global (g: Ast.global') = 
    let compiled_const = compile_const g.value in
    let open Stackless in
    Ir.{ gtype = g.gtype; value = compiled_const } in
  List.map (fun (g: Ast.global) -> compile_global g.it ) globals

let compile_funcs (funcs: Ast.func list) : Stackless.stackless_instr func list =
  let compile_func (f: Ast.func') : Stackless.stackless_instr func =
    { ftype = f.ftype; locals = f.locals; body = compile_instrs f.body } in
  List.map (fun (f: Ast.func) -> compile_func f.it) funcs

let compile_segments (segments: 'a Ast.segment list) : ('a, Stackless.stackless_instr) Ir.segment list =
  let compile_segment (s: 'a Ast.segment') =
    { index = s.index; offset = compile_const s.offset; init = s.init } in
  List.map (fun (s: 'a Ast.segment) -> compile_segment s.it) segments

let compile_module (wasm_mod: Ast.module_) : Stackless.stackless_instr Ir.module_ =
  let wasm_mod = wasm_mod.it in
  let compiled_globals = compile_globals wasm_mod.globals in
  let compiled_funcs = compile_funcs wasm_mod.funcs in
  let compiled_elems = compile_segments wasm_mod.elems in
  let compiled_data = compile_segments wasm_mod.data in
  {
    types = wasm_mod.types;
    globals = compiled_globals;
    tables = wasm_mod.tables;
    memories = wasm_mod.memories;
    funcs = compiled_funcs;
    start = wasm_mod.start;
    elems = compiled_elems;
    data = compiled_data;
    imports = wasm_mod.imports;
    exports = wasm_mod.exports
  }
