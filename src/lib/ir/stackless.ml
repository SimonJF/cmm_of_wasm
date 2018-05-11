open Libwasm

type branch = Label.t * Var.t list

type is_rec = bool

type term = {
  body : statement list;
  terminator : terminator
}

and statement =
  | Cont of Label.t * Var.t list * is_rec * term
  | Let of Var.t * expr
  | Effect of effect

and terminator =
  | Unreachable
  | Br of branch
  | BrTable of { cond : Var.t; branches : branch list; default : branch }
  | If of { cond : Var.t; ifso : branch; ifnot : branch }
  | Call of { func : Func_id.t; args : Var.t list; arg_type : Types.stack_type;
              ret_type : Types.stack_type; cont : branch }
  | CallIndirect of { type_ : Types.func_type; func : Var.t; args : Var.t list; cont : branch }

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
  args : Var.t list;
  body : term;
}
