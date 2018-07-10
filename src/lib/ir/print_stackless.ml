open Libwasm.Sexpr
let var = Libwasm.Arrange.var
let listi = Libwasm.Arrange.listi
let list = Libwasm.Arrange.list
let var_atom v = Atom ("var " ^ (Var.to_string v))
let concat_args xs =
  List.map (Var.to_string) xs |> String.concat " "

let sexpr_of_effect e =
  let open Stackless in
  let head, inner =
    match e with
      | Stackless.SetGlobal (glob, v) ->
          "set_global",
          [Global.to_sexpr glob; var_atom v]
      | Stackless.Store store_op ->
          let open Libwasm in
          let op = store_op.op in
          let idx = Var.to_string store_op.index in
          let value = Var.to_string store_op.value in
          Arrange.storeop op ^ ", idx: " ^ idx ^ " = " ^ value, [] in
  Node (head, inner)


let rec sexpr_of_term (term: Stackless.term) =
  let body = list sexpr_of_statement term.body in
  let terminator = [sexpr_of_terminator term.terminator] in
  Node ("term", body @ terminator)

and sexpr_of_statement stmt =
  let head, inner =
  let open Stackless in
    match stmt with
      | Cont (lbl, binders, is_rec, term) ->
          let txt =
            Printf.sprintf "%s(%s) %s "
              (Label.to_string lbl)
              (concat_args binders)
              (if is_rec then "rec" else "") in
          txt,
            [sexpr_of_term term]
      | Let (v, e) ->
          "let " ^ (Var.to_string v) ^ "=", [sexpr_of_expr e]
      | Effect e -> "effect", [sexpr_of_effect e] in
  Node (head, inner)

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

and sexpr_of_expr expr =
  let head, inner =
    match expr with
      | Select { cond; ifso; ifnot } ->
          let str = Var.to_string in
          "select", [
            (Atom ("cond: " ^ (str cond)));
            (Atom ("ifso: " ^ (str ifso)));
            (Atom ("ifnot: " ^ (str ifnot)))]
      | GetGlobal glob -> "get_global", [Global.to_sexpr glob]
      | Load (op, v) -> (Libwasm.Arrange.loadop op) ^ ": " ^ (Var.to_string v), []
      | MemorySize -> "memory_size", []
      | MemoryGrow v -> "memory_grow: " ^ (Var.to_string v), []
      | Const v -> "const " ^ (Libwasm.Values.string_of_value v), []
      | Test (op, v) ->
          let v = Var.to_string v in
          "(" ^ (Libwasm.Arrange.testop op) ^ ") " ^ v, []
      | Compare (op, v1, v2) ->
          let v1 = Var.to_string v1 in
          let v2 = Var.to_string v2 in
          "(" ^ (Libwasm.Arrange.relop op) ^ ") " ^
          v1 ^ " " ^ v2, []
      | Unary (op, v) ->
          let v = Var.to_string v in
          "(" ^ (Libwasm.Arrange.unop op) ^ ") " ^ v, []
      | Binary (op, v1, v2) ->
          let v1 = Var.to_string v1 in
          let v2 = Var.to_string v2 in
          "(" ^ (Libwasm.Arrange.binop op) ^ ") " ^
          v1 ^ " " ^ v2, []
      | Convert (op, v) ->
          let v = Var.to_string v in
          "(" ^ (Libwasm.Arrange.cvtop op) ^ ") " ^ v, [] in
  Node (head, inner)

and sexpr_of_func (func: Stackless.func) name =
  Node ("func",
    [Label.to_sexpr func.return;
     Atom (": " ^ Libwasm.Types.string_of_func_type func.type_);
     Atom (concat_args func.params);
     Node ("body", [sexpr_of_term func.body])])

let const c = list sexpr_of_expr c

let sexpr_of_module (m: Stackless.module_) =
  let open Util.Maps in
  let funcs = Int32Map.bindings m.function_metadata in
  let globals = Int32Map.bindings m.globals in

  let sexpr_of_func (i, md) =
    let body =
      match Int32Map.find_opt i m.function_ir with
        | Some ir ->
            [ Func.to_sexpr md; sexpr_of_func ir (Func.to_string md) ]
        | None -> [Func.to_sexpr md] in
    Node ("func", body) in

  let funcs = Node ("funcs", List.map sexpr_of_func funcs) in

  let globals =
    Node ("globals", List.map (fun (_, x) -> Global.to_sexpr x) globals) in
  Node ("module", [funcs; globals])


let string_of_module m =
  let open Libwasm in
  sexpr_of_module m
  |> Sexpr.to_string !Flags.width
