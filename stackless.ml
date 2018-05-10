open Libwasm

type virtual_var = string
type var = int32
type literal = Values.value

type select_type = {
    v1 : virtual_var;
    v2 : virtual_var;
    conditional : virtual_var
}

type br_table_type = {
    labels : var list;
    default : var;
    operand : virtual_var
}

type 'a memop =
  {ty : Types.value_type; align : int; offset : Memory.offset; sz : 'a option}

type loadop = {
    memory_operation : (Memory.pack_size * Memory.extension) memop;
    address : virtual_var
}
type storeop = {
    pack_size : Memory.pack_size memop;
    address : virtual_var;
    to_store : virtual_var
}

type stackless_instr =
  | Let of (virtual_var * stackless_instr)
  | Return of virtual_var
  | Unreachable
  | Select of select_type
  | Block of stackless_instr list  (* execute in sequence *)
  | Loop of stackless_instr list   (* loop header *)
  | If of virtual_var * stackless_instr list * stackless_instr list  (* conditional *)
  (* TODO: I'm still not entirely sure of the effects Br has on the stack, from the
   * operational semantics. I'll need to look at it a bit more, although the text
   * gives me a reasonable idea. How much of this needs to be encoded in the instruction? *)
  | Br of var
  | BrIf of (var * virtual_var)
  | BrTable of br_table_type
  | Call of var
  | CallIndirect of (var * virtual_var)
  | Load of loadop
  | Store of storeop
  | MemoryGrow of virtual_var 
  | MemorySize
  | GetGlobal of var
  | SetGlobal of (var * virtual_var)
  | GetLocal of var
  | SetLocal of (var * virtual_var)
  | Const of literal
  | Test of (Ast.testop * virtual_var * virtual_var)
  | Compare of (Ast.relop * virtual_var * virtual_var)
  | Unary of (Ast.unop * virtual_var)
  | Binary of (Ast.binop * virtual_var * virtual_var)
  | Convert of (Ast.cvtop * virtual_var)

module ToSexpr = struct
    open Sexpr

    let vvar v = Atom ("vvar " ^ v)

    let list f xs = List.map f xs
    let constop v = Types.string_of_value_type (Values.type_of v) ^ ".const"

    let var = Libwasm.I32.to_string_u

    let memop name {ty; align; offset; _} =
      Arrange.value_type ty ^ "." ^ name ^
      (if offset = 0l then "" else " offset=" ^ Arrange.nat32 offset) ^
      (if 1 lsl align = Types.size ty then "" else " align=" ^ Arrange.nat (1 lsl align))

    let loadop op =
      match op.sz with
      | None -> memop "load" op
      | Some (sz, ext) ->
          memop ("load" ^ 
                 Arrange.pack_size sz ^
                 Arrange.extension ext) op

    let storeop op =
      match op.sz with
      | None -> memop "store" op
      | Some sz -> memop ("store" ^ Arrange.pack_size sz) op


    let rec instr e =
      let head, inner =
        match e with
        | Unreachable -> "unreachable", []
        | Let (bnd, e) -> "let " ^ bnd, [instr e]
        | Return var -> "return " ^ var, []
        | Select sty ->
            "select", [vvar sty.v1; vvar sty.v2; vvar sty.conditional]
        | Block es -> "block", list instr es
        | Loop es -> "loop", list instr es
        | If (var, es1, es2) ->
            "if", (vvar var) ::
                [Node ("then", list instr es1); Node ("else", list instr es2)]
        | Br x -> "br " ^ var x, []
        | BrIf (x, v) -> "br_if " ^ var x ^ ", " ^ v, []
        | BrTable brt ->
            let formatted_labels =
              String.concat " " (list var (brt.labels @ [brt.default])) in
            let labels_node = Node ("labels", [Atom formatted_labels]) in
            let operand_node = Node ("operand", [vvar brt.operand]) in
          "br_table ", [labels_node; operand_node]
        | Load op -> 
            "loadop ",
            [ (Node ("memop", [Atom (loadop op.memory_operation)]));
              (Node ("address", [vvar op.address])) ]
        | Store op ->
            "storeop ",
            [ (Node ("memop", [Atom (storeop op.pack_size)]));
              (Node ("address", [vvar op.address])) ]
        | Call x -> "call " ^ var x, []
        | CallIndirect (x, vvar) ->
                "call_indirect",
                [Node ("type " ^ var x, []); Node ("vvar " ^ vvar, [])]
        | GetGlobal x -> "get_global " ^ var x, []
        | SetGlobal (x, vvar) -> "set_global " ^ var x ^ ", " ^ vvar, []
        | MemorySize -> "memory.size", []
        | MemoryGrow v -> "memory.grow " ^ v, []
        | GetLocal x -> "get_local " ^ var x, []
        | SetLocal (x, vvar) -> "set_local " ^ var x ^ ", " ^ vvar, []
        | Const lit -> constop lit ^ " " ^ Values.string_of_value lit, []
        | Test (op, v1, v2) -> Arrange.testop op, [vvar v1; vvar v2]
        | Compare (op, v1, v2) -> Arrange.relop op, [vvar v1; vvar v2]
        | Unary (op, v1) -> Arrange.unop op, [vvar v1]
        | Binary (op, v1, v2) -> Arrange.binop op, [vvar v1; vvar v2]
        | Convert (op, v1) -> Arrange.cvtop op, [vvar v1]
      in Node (head, inner)
end

module ShowStackless =
    Ir.Show.Make(
        struct
            type t = stackless_instr
            let sexpr_of_instr = ToSexpr.instr
        end)

