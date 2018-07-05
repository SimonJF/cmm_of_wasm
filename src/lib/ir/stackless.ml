open Libwasm.Ast
open Libwasm.Types

type is_rec = bool

type binder = Var.t

type store_op = {
    op : storeop; index : Var.t; value : Var.t
}

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
  | Call of { func : Func.t; args : Var.t list; cont : Branch.t }
  | CallIndirect of { type_ : func_type; func : Var.t; args : Var.t list; cont : Branch.t }

and expr =
  | Select of { cond : Var.t; ifso : Var.t; ifnot : Var.t }
  | GetGlobal of Global.t
  | Load of loadop * Var.t
  | MemorySize
  | MemoryGrow of Var.t
  | Const of Libwasm.Values.value
  | Test of testop * Var.t
  | Compare of relop * Var.t * Var.t
  | Unary of unop * Var.t
  | Binary of binop * Var.t * Var.t
  | Convert of cvtop * Var.t

and effect =
  | SetGlobal of Global.t * Var.t
  | Store of store_op

type func = {
  return : Label.t;
  type_ : func_type;
  params : binder list;
  body : term;
}

type data = {
  offset: expr;
  contents: string
}

type table =
  | LocalTable of (Int32.t Libwasm.Types.limits)
  | ImportedTable of {
      module_name: string;
      table_name: string;
      limits: Int32.t Libwasm.Types.limits
    }

type memory =
  | LocalMemory of Libwasm.Types.memory_type
  | ImportedMemory of {
      module_name: string;
      memory_name: string;
      limits: Int32.t Libwasm.Types.limits
  }

type module_ = {
    funcs : (func * Func.t) Util.Maps.Int32Map.t;
    globals: Global.t Util.Maps.Int32Map.t;
    start : Func.t option;
    exports : Libwasm.Ast.export list;
    memory_metadata: memory option;
    data : data list;
    table: table option;
    table_elems : (expr * Func.t Util.Maps.Int32Map.t) list (* Expr: offset *)
}
