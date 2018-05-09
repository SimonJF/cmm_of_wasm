(* Common format for all IRs. Parameterise WASM modules
 * over the instruction type.
 *
 * Also provide printing functionality with things tediously
 * mapped to functions in Arrange.ml (WASM)
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
        open Sexpr
        type t = X.t

        let var = Arrange.var (* Libwasm.I32.to_string_u *)
        let listi = Arrange.listi
        let list = Arrange.list

        let sexpr_of_instr = X.sexpr_of_instr

        let const c =
          list sexpr_of_instr c


        let global = failwith "TODO"
        let func_with_index = failwith "TODO"

        let segment head dat seg =
          let {index; offset; init} = seg in
          Node (head, 
            Arrange.atom var index ::
                Node ("offset", const offset) :: dat init)

        let elems seg =
          segment "elem" (list (Arrange.atom var)) seg

        let break_bytes s =
          let ss = Lib.String.breakup s 16 in
          list (Arrange.atom Arrange.bytes) ss

        let data seg =
          segment "data" break_bytes seg


        let module_ m =
          let func_imports = List.filter Arrange.is_func_import m.imports in
          let table_imports = List.filter Arrange.is_table_import m.imports in
          let memory_imports = List.filter Arrange.is_memory_import m.imports in
          let global_imports = List.filter Arrange.is_global_import m.imports in
          Node ("module",
            listi Arrange.typedef m.types @
            listi Arrange.import table_imports @
            listi Arrange.import memory_imports @
            listi Arrange.import global_imports @
            listi Arrange.import func_imports @
            listi (Arrange.table (List.length table_imports)) m.tables @
            listi (Arrange.memory (List.length memory_imports)) m.memories @
            listi (global (List.length global_imports)) m.globals @
            listi (func_with_index (List.length func_imports)) m.funcs @
            list Arrange.export m.exports @
            Arrange.opt Arrange.start m.start @
            list elems m.elems @
            list data m.data
          )

        let sexpr_of_module m = module_ m

        let string_of_module m =
            sexpr_of_module m
            |> Sexpr.to_string !Flags.width
    end
end
