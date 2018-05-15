open Libwasm

type is_rec = bool

type binder = Var.t

type term = {
  body : statement list;
  terminator : terminator
}

and statement =
  | Cont of Label.t * binder list * is_rec * term
  | Let of Var.t * expr
  | Effect of effect

and terminator =
  | Unreachable
  | Br of Branch.t
  | BrTable of { index : Var.t; es : Branch.t list; default : Branch.t }
  | If of { cond : Var.t; ifso : Branch.t; ifnot : Branch.t }
  | Call of { func : Func_id.t; args : Var.t list; arg_type : Types.stack_type;
              ret_type : Types.stack_type; cont : Branch.t }
  | CallIndirect of { type_ : Types.func_type; func : Var.t; args : Var.t list; cont : Branch.t }

and expr =
  | Select of { cond : Var.t; ifso : Var.t; ifnot : Var.t }
  | GetGlobal of Global.t
  | Load of Ast.loadop * Var.t
  | MemorySize
  | MemoryGrow of Var.t
  | Const of Values.value
  | Test of Ast.testop * Var.t
  | Compare of Ast.relop * Var.t * Var.t
  | Unary of Ast.unop * Var.t
  | Binary of Ast.binop * Var.t * Var.t
  | Convert of Ast.cvtop * Var.t

and effect =
  | SetGlobal of Global.t * Var.t
  | Store of { op : Ast.storeop; index : Var.t; value : Var.t }

type func = {
  return : Label.t;
  type_ : Types.func_type;
  args : binder list;
  body : term;
}
