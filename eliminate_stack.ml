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

let compile_instrs (instrs: Libwasm.Ast.instr list) : Stackless.stackless_instr list =
    let stack = Stack.create () in

    let compile_instr stack instr = 
      let open Libwasm.Ast in
      match instr with
        | Unreachable -> [Stackless.Unreachable] (* trap unconditionally *)
        | Nop -> [] (* eliminate nops *)
        | Drop ->
            let _ = Stack.pop stack in [] (* forget a value *)
        (* 
        | Select -> failwith "select unimplemented" (* branchless conditional *)
        | Block of stack_type * instr list  (* execute in sequence *)
        | Loop of stack_type * instr list   (* loop header *)
        | If of stack_type * instr list * instr list  (* conditional *)
        | Br of var                         (* break to n-th surrounding label *)
        | BrIf of var                       (* conditional break *)
        | BrTable of var list * var         (* indexed break *)
        | Return                            (* break from function body *)
        | Call of var                       (* call function *)
        | CallIndirect of var               (* call function through table *)
        | SetLocal of var                   (* write local variable *)
        | TeeLocal of var                   (* write local variable and keep value *)
        | GetGlobal of var                  (* read global variable *)
        | SetGlobal of var                  (* write global variable *)
        | Load of loadop                    (* read memory at address *)
        | Store of storeop                  (* write memory at address *)
        | MemorySize                        (* size of linear memory *)
        | MemoryGrow                        (* grow linear memory *)
        *)
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
            let result = Stack.pop stack in
            (Stackless.Return result) :: acc |> List.rev
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
