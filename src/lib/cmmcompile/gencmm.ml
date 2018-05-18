(* Compile Stackless into CMM *)
open Ir
open Cmm

let lv env x = Compile_env.lookup_var x env

let nodbg = Debuginfo.none
let compile_terminator t = failwith "TODO"

(* We're making the assumption here that we're on a 64-bit architecture at the moment.
 * FIXME: We're (erroneously) treating 32-bit integers as 64-bit integers here.
 * FIXME: We're not supporting floats yet. (represented as 64-bit ints in WASM,
 * and I'm not quite sure of the mapping to machine floats yet).
 *)
let compile_value =
  let open Libwasm.Values in
  function
  | I32 x -> Cconst_int (Int32.to_int x)
  | I64 x -> Cconst_natint (Int64.to_nativeint x)
  | F32 x -> failwith "32-bit floats not yet supported"
  | F64 x -> failwith "TODO" (* Cconst_float x *)

let compile_testop _ = failwith "TODO"
let compile_relop _ = failwith "TODO"
let compile_unop _ = failwith "TODO"
let compile_binop _ = failwith "TODO"
let compile_cvtop _ = failwith "TODO"

let compile_expression env =
  let open Ir.Stackless in
  function
  | Select { cond ; ifso ; ifnot } ->
      Cifthenelse (Cvar (lv env cond), Cvar(lv env ifso), Cvar(lv env ifnot))
  | GetGlobal _ -> failwith "TODO"
  | Load (loadop, v) -> failwith "TODO"
  | MemorySize -> failwith "TODO" (* TODO: write RTS, hook in here *)
  | MemoryGrow v -> failwith "TODO"
  | Const value -> compile_value value
  | Test (test, v) ->
      let op = compile_testop test in
      let i = lv env v in
      Cop (op, [Cvar i], nodbg)
  | Compare (rel, v1, v2) ->
      let op = compile_relop rel in
      let (i1, i2) = (lv env v1, lv env v2) in
      Cop (op, [Cvar i1; Cvar i2], nodbg)
  | Unary (un, v) ->
      let op = compile_unop un in
      Cop (op, [Cvar (lv env v)], nodbg)
  | Binary (bin, v1, v2) ->
      let op = compile_binop bin in
      let (i1, i2) = (lv env v1, lv env v2) in
      Cop (op, [Cvar i1; Cvar i2], nodbg)
  | Convert (cvt, v) ->
      let op = compile_cvtop cvt in
      Cop (op, [Cvar (lv env v)], nodbg)

(* compile_body: env -> W.terminator -> W.statement list -> Cmm.expression *) 
let rec compile_body env terminator = function
  | [] -> compile_terminator terminator
  | x :: xs ->
      let open Stackless in
      begin
      match x with
        | Cont (lbl, xs, is_rec, body) -> failwith "TODO"
        | Let (v, e) ->
            let ident = Ident.create (Var.to_string v) in
            let env = Compile_env.bind_var v ident env in
            let e1 = compile_expression env e in
            Clet (ident, e1, compile_body env terminator xs)
        | Effect eff -> failwith "TODO"
      end


