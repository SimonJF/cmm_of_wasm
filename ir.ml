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


module Show = struct

    module type INSTRTYPE = sig
        type t
        val sexpr_of_instr : t -> Libwasm.Sexpr.sexpr
    end

    module Make (X: INSTRTYPE) = struct
        type t = X.t

        let sexpr_of_instr = X.sexpr_of_instr
        let sexpr_of_module m = failwith "todo"
        let string_of_module m = failwith "todo"
    end
end
