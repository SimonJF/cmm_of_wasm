open Libwasm

type virtual_var = string
type literal = Values.value

type stackless_instr =
  | Let of (virtual_var * stackless_instr)
  | Return of virtual_var
  | Unreachable                       (* trap unconditionally *)
  (*
  | Select                            (* branchless conditional *)
  | Block of stack_type * stackless_instr list  (* execute in sequence *)
  | Loop of stack_type * stackless_instr list   (* loop header *)
  *)
  | If of virtual_var * stackless_instr list * stackless_instr list  (* conditional *)
  (*
  | Br of var                         (* break to n-th surrounding label *)
  | BrIf of var                       (* conditional break *)
  | BrTable of var list * var         (* indexed break *)
  | Return                            (* break from function body *)
  | Call of var                       (* call function *)
  | CallIndirect of var               (* call function through table *)
  | GetLocal of var                   (* read local variable *)
  | SetLocal of var                   (* write local variable *)
  | TeeLocal of var                   (* write local variable and keep value *)
  | GetGlobal of var                  (* read global variable *)
  | SetGlobal of var                  (* write global variable *)
  | Load of loadop                    (* read memory at address *)
  | Store of storeop                  (* write memory at address *)
  | MemorySize                        (* size of linear memory *)
  | MemoryGrow                        (* grow linear memory *)
  *)
  | Const of literal                  (* constant *)
  | Test of (Ast.testop * virtual_var * virtual_var)
                                      (* numeric test *)
  | Compare of (Ast.relop * virtual_var * virtual_var)                 (* numeric comparison *)
  | Unary of (Ast.unop * virtual_var)                    (* unary numeric operator *)
  | Binary of (Ast.binop * virtual_var * virtual_var)                  (* binary numeric operator *)
  | Convert of (Ast.cvtop * virtual_var)                 (* conversion *)
  [@@deriving show]

module ToSexpr = struct
    open Sexpr

    let vvar v = Atom ("vvar " ^ v)

    let list f xs = List.map f xs
    let constop v = Types.string_of_value_type (Values.type_of v) ^ ".const"

    let rec instr e =
      let head, inner =
        match e with
        | Unreachable -> "unreachable", []
        | Let (bnd, e) -> "let " ^ bnd, [instr e]
        | Return var -> "return " ^ var, []
        (*
        | Select -> "select", []
        | Block (ts, es) -> "block", stack_type ts @ list instr es
        | Loop (ts, es) -> "loop", stack_type ts @ list instr es
        *)
        | If (var, es1, es2) ->
            "if", (vvar var) ::
                [Node ("then", list instr es1); Node ("else", list instr es2)]
            (*
        | Br x -> "br " ^ var x, []
        | BrIf x -> "br_if " ^ var x, []
        | BrTable (xs, x) ->
          "br_table " ^ String.concat " " (list var (xs @ [x])), []
        | Return -> "return", []
        | Call x -> "call " ^ var x, []
        | CallIndirect x -> "call_indirect", [Node ("type " ^ var x, [])]
        | GetLocal x -> "get_local " ^ var x, []
        | SetLocal x -> "set_local " ^ var x, []
        | TeeLocal x -> "tee_local " ^ var x, []
        | GetGlobal x -> "get_global " ^ var x, []
        | SetGlobal x -> "set_global " ^ var x, []
        | Load op -> loadop op, []
        | Store op -> storeop op, []
        | MemorySize -> "memory.size", []
        | MemoryGrow -> "memory.grow", []
        *)
        | Const lit -> constop lit ^ " " ^ Values.string_of_value lit, []
        | Test (op, v1, v2) -> Arrange.testop op, [vvar v1; vvar v2]
        | Compare (op, v1, v2) -> Arrange.relop op, [vvar v1; vvar v2]
        | Unary (op, v1) -> Arrange.unop op, [vvar v1]
        | Binary (op, v1, v2) -> Arrange.binop op, [vvar v1; vvar v2]
        | Convert (op, v1) -> Arrange.cvtop op, [vvar v1]
      in Node (head, inner)
end

