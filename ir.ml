(* Common format for all IRs. Parameterise WASM modules
 * over the instruction type.
 *)
open Libwasm
open Libwasm.Types

type 'instr const = 'instr list
type var = Ast.var

type 'instr global =
{
  gtype : global_type;
  value : 'instr const;
}

type 'instr func =
{
  ftype : var;
  locals : value_type list;
  body : 'instr list;
}

type ('data, 'instr) segment =
{
  index : var;
  offset : 'instr const;
  init : 'data;
}

type 'instr module_ =
{
  types : Ast.type_ list;
  globals : ('instr global) list;
  tables : Ast.table list;
  memories : Ast.memory list;
  funcs : 'instr func list;
  start : var option;
  elems : ((var list), 'instr) segment list;
  data : (string, 'instr) segment list;
  imports : Ast.import list;
  exports : Ast.export list;
}

