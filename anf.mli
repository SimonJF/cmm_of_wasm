open Libwasm

type var = Ast.var

type anf_value =
  | ANFVar of var
  | WasmVal of Values.literal
 

type anf_term =
  | Let of var * anf_term * anf_term  (* let x = e1 in e2 *)
  | Lift of anf_value                 (* return v *)
  | Unreachable                       (* trap unconditionally *)
  (*
  | Select                            (* branchless conditional: TODO *)
  | Block of ssa                      (* scoped inner SSA: TODO? *)
  | Loop of stack_type * instr glist   (* loop header: TODO?  *)
  *)
  | If of anf_value * anf_term * anf_term (* conditional *)
  (*
  | Br of var                         (* break to n-th surrounding label *)
  | BrIf of var                       (* conditional break *)
  | BrTable of var list * var         (* indexed break *)
  | Return                            (* break from function body *)
  *)
  | Call of var * anf_value list      (* call function *)
  | CallIndirect of var * anf_value list (* call function through table *)
  | GetLocal of var                   (* read local variable *)
  | SetLocal of var                   (* write local variable *)
  | TeeLocal of var                   (* write local variable and keep value *)
  | GetGlobal of var                  (* read global variable *)
  | SetGlobal of var                  (* write global variable *)
  (*
  | Load of loadop                    (* read memory at address *)
  | Store of storeop                  (* write memory at address *)
  *)
  | MemorySize                        (* size of linear memory *)
  | MemoryGrow                        (* grow linear memory *)
  | Test of Ast.testop * anf_value * anf_value (* numeric test *)
  | Compare of Ast.relop * anf_value * anf_value (* numeric comparison *)
  | Unary of Ast.unop * anf_value         (* unary numeric operator *)
  | Binary of Ast.binop * anf_value * anf_value (* binary numeric operator *)
  | Convert of Ast.cvtop * anf_value                  (* conversion *)
