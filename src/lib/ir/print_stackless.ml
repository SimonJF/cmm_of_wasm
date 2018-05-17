open Libwasm.Sexpr
let var = Libwasm.Arrange.var
let listi = Libwasm.Arrange.listi
let list = Libwasm.Arrange.list
let var_atom v = Atom ("var " ^ (Var.to_string v))
let concat_args xs =
  List.map (Var.to_string) xs |> String.concat " "

module Memops = struct
  open Libwasm
  (* open Libwasm.Ast *)

  let memop name (mem_op: 'a Libwasm.Ast.memop) =
    Arrange.value_type mem_op.ty ^ "." ^ name ^
    (if (mem_op.offset) = 0l then "" else " offset=" ^ Arrange.nat32 (mem_op.offset)) ^
    (if 1 lsl (mem_op.align) = Types.size mem_op.ty then "" else " align=" ^ Arrange.nat (1 lsl mem_op.align))

  let loadop (op: Libwasm.Ast.loadop) =
    match op.sz with
    | None -> memop "load" op
    | Some (sz, ext) -> memop ("load" ^ Arrange.pack_size sz ^ Arrange.extension ext) op

  let storeop (op: Libwasm.Ast.storeop) : string =
    match op.sz with
    | None -> memop "store" op
    | Some sz -> memop ("store" ^ Arrange.pack_size sz) op
end

let rec sexpr_of_term = failwith "TODO"

and sexpr_of_statement = failwith "TODO"

and sexpr_of_terminator term =
  let head, inner =
    let open Stackless in
    match term with
      | Unreachable -> "unreachable", []
      | Br br -> "br", [Branch.to_sexpr br]
      | BrTable { index; es; default} ->
          "br_table",
          [ var_atom index;
            Node ("branches ", list Branch.to_sexpr es);
            Node ("default " , [Branch.to_sexpr default]) ]
      | If { cond; ifso; ifnot } ->
          "if",
          [var_atom cond; Branch.to_sexpr ifso; Branch.to_sexpr ifnot]
      | Call { func; args; cont } ->
          "call",
          [ Func.to_sexpr func;
            Atom ("args " ^ concat_args args);
            Branch.to_sexpr cont ]
      | CallIndirect { type_; func; args; cont } -> 
          "call_indirect",
          [Atom ("ty " ^ Libwasm.Types.string_of_func_type type_);
           Atom ("id " ^ Var.to_string func);
           Atom ("args " ^ (concat_args args));
           Node ("cont ", [Branch.to_sexpr cont]) ]
          in
  Node (head, inner)

and sexpr_of_expr = failwith "TODO"

and sexpr_of_effect e =
  let open Libwasm.Ast in
  let open Stackless in
  let head, inner =
    match e with
      | SetGlobal (glob, v) ->
          "set_global", 
          [Global.to_sexpr glob; var_atom v]
      | Store op -> 
          "store_op", []
          (* I GIVE UP!!!! 
           * [Atom (Libwasm.Arrange.storeop op)] *)in
  Node (head, inner)

and sexpr_of_func func name = failwith "TODO"



let const c = list sexpr_of_expr c

let sexpr_of_module m = failwith "TODO"

let string_of_module m =
  let open Libwasm in
  sexpr_of_module m
  |> Sexpr.to_string !Flags.width
