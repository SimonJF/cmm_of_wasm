(* Compile Stackless into CMM *)
open Ir
open Cmm

let lv env x = Compile_env.lookup_var x env

let bind_var v env =
  let ident = Ident.create (Var.to_string v) in
  let env = Compile_env.bind_var v ident env in
  (ident, env)

let nodbg = Debuginfo.none

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
 
let compile_relop =
  let open Libwasm.Ast in
  let open Libwasm.Values in
    let compile_float_op =
    let open Libwasm.Ast.FloatOp in
    function
      | Eq -> CFeq
      | Ne -> CFneq
      | Lt -> CFlt
      | Gt -> CFgt
      | Le -> CFle
      | Ge -> CFge in
    (* FIXME: ignoring signedness for the time being, just getting skeletons up and running *)
    let compile_int_op =
    let open Libwasm.Ast.IntOp in
    function
      | Eq -> Ceq
      | Ne -> Cne
      | LtS -> Clt
      | LtU -> Clt
      | GtS -> Cgt
      | GtU -> Cgt
      | LeS -> Cle
      | LeU -> Cle
      | GeS -> Cge
      | GeU -> Cge in
    function
      | I32 i32op -> Ccmpi (compile_int_op i32op)
      | I64 i64op -> Ccmpi (compile_int_op i64op)
      | F64 f64op -> Ccmpf (compile_float_op f64op)
      | _ -> failwith "F32s not supported yet"

let compile_unop _ = failwith "TODO: I have no idea what any of these mean yet"
let compile_binop op = 
  let open Libwasm.Ast in
  let open Libwasm.Values in
  (* FIXME: Signedness for division / modulo *)
  let compile_int_op =
    let open Libwasm.Ast.IntOp in
    function
    | Add -> Caddi
    | Sub -> Csubi
    | Mul -> Cmuli
    | DivS -> Cdivi
    | DivU -> Cdivi
    | RemS -> Cmodi
    | RemU -> Cmodi
    (* TODO: Ensure that these are actually bitwise
     * operators and not just binary ones...*)
    | And -> Cand
    | Or -> Cor
    | Xor -> Cxor
    (* TODO: Shift right operators ignore signedness right now... *)
    | Shl -> Clsl
    | ShrS -> Clsr
    | ShrU -> Clsr
    | Rotl -> failwith "Rotation unimplemented"
    | Rotr -> failwith "Rotation unimplemented" in
  let compile_float_op = failwith "TODO" in
  match op with
    | I32 i32op -> compile_int_op i32op
    | I64 i64op -> compile_int_op i64op
    | F32 f32op -> compile_float_op f32op
    | F64 f64op -> compile_float_op f64op
  
  
let compile_cvtop _ = failwith "Conversion operators not done yet"

let compile_type : Libwasm.Types.value_type -> machtype = 
  let open Libwasm.Types in
  function
    | I32Type -> typ_int
    | I64Type -> typ_int
    | F32Type -> typ_float
    | F64Type -> typ_float


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
      let i = lv env v in
      let cmp = 
        let open Libwasm.Types in
        match Var.type_ v with
          | I32Type -> Cconst_int 0
          | I64Type -> Cconst_natint Nativeint.zero
          | _ -> failwith "Eqz not implemented on floats"
        in
      Cop (Ccmpi Ceq, [Cvar i; cmp], nodbg)
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

module TrapReasons : sig
  val unreachable : int
end = struct
  let unreachable = 0
end

let trap reason =
  Cop (Cextcall ("__Cmmwasm_trap", typ_int, false, None), 
    [Cconst_int reason], nodbg)

let compile_terminator env = 
  let open Stackless in
  let branch b =
    let br_id = Compile_env.lookup_label (Branch.label b) env in
    let args =
      List.map (fun v ->
        Cvar (lv env v)) (Branch.arguments b) in
    Cexit (br_id, args) in
  function
    | Unreachable -> trap (TrapReasons.unreachable)
    | Br b -> branch b
    | BrTable { index ; es ; default } -> failwith "TODO"
    | If { cond; ifso; ifnot } ->
        let cond_var = Cvar (lv env cond) in
        let test = 
          Cop (Ccmpi Ceq, [cond_var; Cconst_natint Nativeint.zero],
            nodbg) in
        let true_branch = branch ifso in
        let false_branch = branch ifnot in
        Cifthenelse (test, true_branch, false_branch)
    | Call { func ; args; cont } ->
        (* Alright. We have a function, set of args, and a
         * continuation branch, pre-populated with some args. 
         * In addition, the continuation will take the return
         * type(s) of the function, so should be prepended. 
         * ...I think? *)
        let (FuncType (_arg_tys, ret_tys)) = Func.type_ func in
        (* We'll need to do some fun with tuples to properly handle
         * functions which return more than one value *)
        let args_cvars = List.map (fun v -> Cvar (lv env v)) args in
        let symbol_name = Compile_env.lookup_func_symbol func env in
        let fn_symbol = Cconst_symbol symbol_name in
        (* Cop (Capply <return type>, <args>, dbg) *)
        let br_id = Compile_env.lookup_label (Branch.label cont) env in
        let branch_args = Branch.arguments cont in
        let cont_args_cvars =
          List.map (fun v -> Cvar (lv env v)) branch_args in
        (* No return values: typ_void in call, csequence, and branch args *)
        let compile_noreturn =
          let call = Cop (Capply typ_void, fn_symbol :: args_cvars, nodbg) in
          Csequence (call, Cexit (br_id, cont_args_cvars)) in
        (* One return value: let-composition, result goes on head of args *)
        let compile_return1 ty =
          let call =
            Cop (Capply (compile_type ty), fn_symbol :: args_cvars, nodbg) in
          let fresh_id = Ident.create ("_call" ^ symbol_name) in
          Clet (fresh_id, call,
            Cexit (br_id, (Cvar fresh_id) :: cont_args_cvars)) in
        begin
          match ret_tys with
            | [] -> compile_noreturn
            | [ty] -> compile_return1 ty
            | _ -> 
                failwith ("Can't currently compile functions with " 
                  ^ "more than one return type")
        end
    | CallIndirect { type_; func; args; cont } -> failwith "TODO"

(* compile_body: env -> W.terminator -> W.statement list -> Cmm.expression *) 
let rec compile_body env terminator = function
  | [] -> compile_terminator env terminator
  | x :: xs ->
      let open Stackless in
      begin
      match x with
        | Cont (lbl, binders, is_rec, body) ->
            let rec_flag = if is_rec then Recursive else Nonrecursive in
            (* TODO: Maybe this is goofy API design -- should flip parameters on
             * lv etc to avoid having to name args here *)
            let binders_idents = List.map (lv env) binders in
            let (lbl_id, env) = Compile_env.bind_label lbl env in
            let catch_clause =
              (lbl_id, binders_idents, compile_term env body) in
            let cont = compile_body env terminator xs in
            Ccatch (rec_flag, [catch_clause], cont)
        | Let (v, e) ->
            let (ident, env) = bind_var v env in
            let e1 = compile_expression env e in
            Clet (ident, e1, compile_body env terminator xs)
        | Effect eff -> failwith "TODO"
      end
and compile_term env term = compile_body env (term.terminator) (term.body)


