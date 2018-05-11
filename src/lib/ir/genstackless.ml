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

type env = {
  stack : Var.t Stack.t;
  block_continuation: Label.t;
  function_return: Label.t;
  arity: int
}


(* IR generation *)
let terminate (code: Stackless.statement list) (terminator: Stackless.terminator) = {
  body = List.rev code;
  terminator = terminator
}

let ir_term instrs env =
  let open Libwasm.Ast in
  let rec transform_instrs env (instrs: Ast.instr list) generated =
    match instrs with
      | [] -> failwith "todo"
      | x :: xs ->
          begin
          (* Binds a "pushed" variable, records on virtual stack *)
          let bind x ty =
            let v = Var.create ty in
            Stack.push v env.stack;
            transform_instrs env xs (Let (v, x) :: generated) in
          let nop = transform_instrs env xs generated in

          (* Stack manipulation *)
          let pop () = Stack.pop env.stack in

          let pop2 () =
            let v1 = Stack.pop env.stack in
            let v2 = Stack.pop env.stack in
            (v1, v2) in

          let popn n =
            let rec go n =
              if n <= 0 then
                []
              else
                let v = Stack.pop env.stack in
                v :: (go (n-1)) in
            go n in

          match x.it with
            | Unreachable -> terminate generated Stackless.Unreachable
            | Nop -> nop
            | Drop -> let _ = pop in nop
            | Select -> failwith "todo"
            | Block (ty, is) -> failwith "todo"
            | Loop (ty, is) -> failwith "todo"
            | If (sty, t, f) -> failwith "todo"
            | Br var -> failwith "todo"
            | BrIf var -> failwith "todo"
            | BrTable (vs, def) -> failwith "todo"
            | Return ->
                let returned = popn env.arity in
                terminate generated (Stackless.Br (env.function_return, returned))
            | Call var -> failwith "todo"
            | CallIndirect var -> failwith "todo"
            | GetLocal var -> failwith "todo"
            | SetLocal var -> failwith "todo"
            | TeeLocal var -> failwith "todo"
            | GetGlobal var -> failwith "todo"
            | SetGlobal var -> failwith "todo"
            | Load loadop -> failwith "todo"
            | Store storeop -> failwith "todo"
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

