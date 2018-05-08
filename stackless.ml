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

