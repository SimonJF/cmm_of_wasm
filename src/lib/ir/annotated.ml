(* First IR, where each block is annotated with the local
 * variables modified in the block (and any sub-blocks).
 *
 * Also strips out positional information (hurrah!)
 *
 * Used for generating efficient SSA. *)

open Libwasm.Types
open Util.Sets

type var = int32
type literal = Libwasm.Values.value
type name = int list

type block_info = {
  block_stack_ty: stack_type;
  block_mutated_locals: int32set;
  block_instrs: instr list
}

and conditional_info = {
  conditional_stack_ty: stack_type;
  conditional_mutated_locals: int32set;
  conditional_true_instrs: instr list;
  conditional_false_instrs: instr list
}

and instr =
  | Unreachable
  | Nop
  | Drop
  | Select
  | Block of block_info
  | Loop of block_info
  | If of conditional_info
  | Br of var
  | BrIf of var
  | BrTable of var list * var
  | Return
  | Call of var
  | CallIndirect of var
  | GetLocal of var
  | SetLocal of var
  | TeeLocal of var
  | GetGlobal of var
  | SetGlobal of var
  | Load of Libwasm.Ast.loadop
  | Store of Libwasm.Ast.storeop
  | MemorySize
  | MemoryGrow
  | Const of literal
  | Test of Libwasm.Ast.testop
  | Compare of Libwasm.Ast.relop
  | Unary of Libwasm.Ast.unop
  | Binary of Libwasm.Ast.binop
  | Convert of Libwasm.Ast.cvtop


(* Globals & Functions *)

type const = instr list

and global =
{
  gtype : global_type;
  value : const;
}

and func =
{
  ftype : var;
  locals : value_type list;
  body : instr list;
}


(* Tables & Memories *)

type table =
{
  ttype : table_type;
}

type memory =
{
  mtype : memory_type;
}

type 'data segment =
{
  index : var;
  offset : const;
  init : 'data;
}

type table_segment = var list segment
type memory_segment = string segment


(* Modules *)

type type_ = func_type

type export_desc =
  | FuncExport of var
  | TableExport of var
  | MemoryExport of var
  | GlobalExport of var

type export =
{
  name : name;
  edesc : export_desc;
}

type import_desc =
  | FuncImport of var
  | TableImport of table_type
  | MemoryImport of memory_type
  | GlobalImport of global_type

type import =
{
  module_name : name;
  item_name : name;
  idesc : import_desc;
}

type module_ =
{
  types : type_ list;
  globals : global list;
  tables : table list;
  memories : memory list;
  funcs : func list;
  start : var option;
  elems : var list segment list;
  data : string segment list;
  imports : import list;
  exports : export list;
}

