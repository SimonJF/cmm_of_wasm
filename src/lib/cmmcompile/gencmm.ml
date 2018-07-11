(* Compile Stackless into CMM *)
open Ir
open Cmm
open Cmm_trap
open Util.Maps

(* Record type to hold information about exports *)
type export_info = {
  func_symbols: string list Int32Map.t;
  global_symbols: string list Int32Map.t;
  memory_symbols: string list;
  table_symbols: string list
}

(* Environment access helper functions *)
let lv env x = Compile_env.lookup_var x env

let ll env lbl = Compile_env.lookup_label lbl env
let llb env br = ll env (Branch.label br)

let bind_var v env =
  let ident = Ident.create (Var.to_string v) in
  let env = Compile_env.bind_var v ident env in
  (ident, env)

let bind_vars vs env =
  let (acc, env) =
    List.fold_left (fun (acc, env) v ->
      let (ident, env) = bind_var v env in
      (ident :: acc, env)
    ) ([], env) vs in
  (List.rev acc, env)

let nodbg = Debuginfo.none

(* Recursive call depth must be bounded. We use a notion of "fuel" for this. *)
let fuel_ident = Ident.create "FUEL"

(* Preamble: Check if we're out of fuel. If so, trap. *)
let add_exhaustion_check return_ty func_body =
  Cifthenelse (
    Cop (Ccmpi Cle, [Cvar fuel_ident; Cconst_int 0], nodbg),
    trap return_ty TrapExhaustion,
    func_body)

(* Integer operations *)

let u32_max = Nativeint.of_string "0x00000000FFFFFFFF"
let u64_max = Nativeint.of_string "0xFFFFFFFFFFFFFFFF"

let sign_extend_i32 n =
  Cop(Casr, [Cop(Clsl, [n; Cconst_int 32], nodbg); Cconst_int 32], nodbg)

let unset_high_32 n = Cop(Cand, [n; Cconst_natint u32_max], nodbg)

let f32_of_f64 x = Cop (Cf32off64, [x], nodbg)
let f64_of_f32 x = Cop (Cf64off32, [x], nodbg)

(* We perform addition, subtraction, and multiplication on 32-bit integers
 * without sign-extending them beforehand. This is safe, because the high-
 * bits just wrap around anyway. Nonetheless, we need to sign extend before
 * returning or storing them. *)
let normalise_signed ty n =
  let open Libwasm.Types in
  match ty with
    | I32Type -> sign_extend_i32 n
    | _ -> n

let normalise_unsigned ty x =
  let open Libwasm.Types in
  match ty with
    | I32Type -> unset_high_32 x
    | _ -> x

let normalise_function return_ty body =
  match return_ty with
    | [] -> body
    | [ty] -> normalise_unsigned ty body
    | _ -> assert false

(* Some handy functions for floats *)

let is_nan x  =
  Cop (
    Ccmpf CFneq, [x ; x],
    nodbg
  )

(* Compilation *)

(* Cmm_of_wasm assumes a 64-bit host and target architecture. I do not intend
 * to support 32-bit architectures any time soon. *)
let compile_value =
  let open Libwasm.Values in
  function
  | I32 x -> Cconst_int (Int32.to_int x)
  | I64 x -> Cconst_natint (Int64.to_nativeint x)
  | F32 x ->
      (* Evil unsafe typecasting voodoo. *)
      (* The idea is this. We have an exact f32 bit pattern we want to store
       * in a register. OCaml's desperately trying to extend it to an f64 one.
       *
       * We convert this bit pattern to an int64. Then, we circumvent the
       * extension by just calling Int64.float_of_bits. Evil genius.
       *)
      Cconst_float (Libwasm.F32.to_bits x |> Int64.of_int32 |> Int64.float_of_bits)
  | F64 x -> Cconst_float (Libwasm.F64.to_float x)


let compile_op_simple env cmmop v1 v2 =
  let (i1, i2) = (lv env v1, lv env v2) in
  Cop (cmmop, [Cvar i1; Cvar i2], nodbg)

let compile_f32_op env cmmop v1 v2 =
  let (i1, i2) = (lv env v1, lv env v2) in
  f32_of_f64 (Cop (cmmop, [f64_of_f32 (Cvar i1); f64_of_f32 (Cvar i2)], nodbg))


(* Some operations (division, modulo, comparison) require sign extension
 * in the case of unsigned 32-bit integers. *)
let compile_op_normalised signed env op v1 v2 =
  let open Libwasm.Types in
  let normalise = if signed then normalise_signed else normalise_unsigned in
  let (v1ty, v2ty) = (Var.type_ v1, Var.type_ v2) in
  let _ = assert (v1ty = v2ty) in
  let (i1, i2) = (lv env v1, lv env v2) in
  let compile_op arg1 arg2 = Cop (op, [arg1; arg2], nodbg) in
  match v1ty with
    | I32Type ->
        compile_op (normalise v1ty (Cvar i1)) (normalise v2ty (Cvar i2))
    | _ -> compile_op (Cvar i1) (Cvar i2)

let compile_relop env op v1 v2 =
  let open Libwasm.Ast in
  let open Libwasm.Values in

  let cf op ~is_32 =
    let (i1, i2) = (lv env v1, lv env v2) in
    if is_32 then
      Cop (op, [f64_of_f32 @@ Cvar i1; f64_of_f32 @@ Cvar i2], nodbg)
    else
      Cop (op, [Cvar i1; Cvar i2], nodbg) in

  let cn op b = compile_op_normalised b env op v1 v2 in

  let compile_float_op ~is_32 =
  let open Libwasm.Ast.FloatOp in
  function
    | Eq -> cf (Ccmpf CFeq) is_32
    | Ne -> cf (Ccmpf CFneq) is_32
    | Lt -> cf (Ccmpf CFlt) is_32
    | Gt -> cf (Ccmpf CFgt) is_32
    | Le -> cf (Ccmpf CFle) is_32
    | Ge -> cf (Ccmpf CFge) is_32 in
  let compile_int_op =
  let open Libwasm.Ast.IntOp in
  function
    | Eq -> cn (Ccmpi Ceq) false
    | Ne -> cn (Ccmpi Cne) false
    | LtS -> cn (Ccmpi Clt) true
    | LtU -> cn (Ccmpa Clt) false
    | GtS -> cn (Ccmpi Cgt) true
    | GtU -> cn (Ccmpa Cgt) false
    | LeS -> cn (Ccmpi Cle) true
    | LeU -> cn (Ccmpa Cle) false
    | GeS -> cn (Ccmpi Cge) true
    | GeU -> cn (Ccmpa Cge) false in
  match op with
    | I32 i32op -> compile_int_op i32op
    | I64 i64op -> compile_int_op i64op
    | F32 f32op -> compile_float_op f32op ~is_32:true
    | F64 f64op -> compile_float_op f64op ~is_32:false

let compile_binop env op v1 v2 =
  let open Libwasm.Ast in
  let open Libwasm.Values in
  let is_32 = match Var.type_ v1 with | I32Type -> true | _ -> false in
  let (i1, i2) = (lv env v1, lv env v2) in

  let division_operation normalise div_f =
    let ty = Var.type_ v2 in
    let cmp =
       match ty with
          | I32Type -> Cconst_int 0
          | I64Type -> Cconst_natint Nativeint.zero
          | _ -> assert false in
    let normal_i1 = normalise ty (Cvar i1) in
    let normal_i2 = normalise ty (Cvar i2) in
    let norm_i1 = Ident.create "_normi1" in
    let norm_i2 = Ident.create "_normi2" in
    Clet (norm_i1, normal_i1,
      Clet (norm_i2, normal_i2,
        Cifthenelse (
          Cop (Ccmpi Ceq, [Cvar norm_i2; cmp], nodbg),
          trap_int TrapDivZero,
          div_f norm_i1 norm_i2
        )
      )
    ) in

  let div_overflow_check signed norm_i1 norm_i2 operation overflow =
    let cmp_norm1 =
      if is_32 then
        Cconst_natint (Nativeint.of_string "0xFFFFFFFF80000000")
      else
        Cconst_natint (Nativeint.of_string "0x8000000000000000") in
    let cmp_norm2, cmp_zero =
      (Cconst_natint Nativeint.minus_one, Cconst_natint Nativeint.zero) in
    if signed then
      Cifthenelse (
        Cop (Ccmpi Cne,
          [(Cop (Cand, [
              Cop (Ccmpi Ceq, [Cvar norm_i1; cmp_norm1], nodbg);
              Cop (Ccmpi Ceq, [Cvar norm_i2; cmp_norm2], nodbg)], nodbg));
           cmp_zero], nodbg),
        overflow,
        operation)
    else
      operation in


  let divide signed =
    let norm_fn = if signed then normalise_signed else normalise_unsigned in
    let div_op = if signed then Cdivi else Cdiviu in

    division_operation
      norm_fn
      (fun norm_i1 norm_i2 ->
        let div = Cop (div_op, [Cvar norm_i1; Cvar norm_i2], nodbg) in
        div_overflow_check signed norm_i1 norm_i2 div (trap_int TrapIntOverflow)) in

  let rem signed =
    let norm_fn = if signed then normalise_signed else normalise_unsigned in
    let div_op = if signed then Cdivi else Cdiviu in
    division_operation
      norm_fn
      (fun norm_i1 norm_i2 ->
        let op =
          Cop (Csubi, [Cvar norm_i1;
            Cop (Cmuli, [
              Cop (div_op, [Cvar norm_i1; Cvar norm_i2], nodbg);
              Cvar norm_i2], nodbg)], nodbg) in
        div_overflow_check signed norm_i1 norm_i2 op (Cconst_int 0)) in

  (* shift_left: need to normalise and mod RHS, but not LHS *)
  let shift_left =
    let ty = Var.type_ v1 in
    (* Must normalise i2 and then get i2 % int width before doing the shift *)
    let normalised_rhs = normalise_unsigned ty (Cvar i2) in
    let mod_base = if is_32 then Cconst_int 32 else Cconst_int 64 in
    (Cop (Clsl, ([Cvar i1;
      Cop (Cmodi, [normalised_rhs; mod_base], nodbg)]), nodbg)) in

  (* shift_right: need to normalise LHS, and both normalise and mod RHS. *)
  let shift_right signed =
    let ty = Var.type_ v1 in
    (* Normalise LHS *)
    let normalised_lhs =
      if signed then
        normalise_signed ty (Cvar i1)
      else
        normalise_unsigned ty (Cvar i1) in
    (* Must normalise i2 and then get i2 % int width before doing the shift *)
    let normalised_rhs = normalise_unsigned ty (Cvar i2) in
    let mod_base = if is_32 then Cconst_int 32 else Cconst_int 64 in
    let op = if signed then Casr else Clsr in
    (Cop (op, ([normalised_lhs;
      Cop (Cmodi, [normalised_rhs; mod_base], nodbg)]), nodbg)) in


  (* Rotation as two shifts and an or, Hacker's delight, p.37 *)
  (* We get junk in the high bits in the i32 case, but that's fine. *)
  let rotate is_left to_shift distance width =
    let (op1, op2) =
      if is_left then (Clsl, Clsr) else (Clsr, Clsl) in
    let shifted_l_n =
      Cop (op1, [to_shift; distance], nodbg) in
    let shifted_r_n =
      Cop (op2, [to_shift;
        Cop (Csubi, [Cconst_int width; distance], nodbg)], nodbg) in
    Cop (Cor, [shifted_l_n; shifted_r_n], nodbg) in

  let rotate_left = rotate true in
  let rotate_right = rotate false in

  let rotate_left_i32 =
    let width = 32 in
    let low_set_ident = Ident.create "rol32_low_set" in
    let low_set = unset_high_32 (Cvar i1) in
    (* Next, normalise rotate length *)
    let distance_ident = Ident.create "dist_id" in
    let distance =
      Cop (Cmodi, [unset_high_32 (Cvar i2); Cconst_int width], nodbg) in
    Clet (low_set_ident, low_set,
      Clet (distance_ident, distance,
        rotate_left (Cvar low_set_ident) (Cvar distance_ident) width)) in

  let rotate_left_i64 =
    let width = 64 in
    let distance_ident = Ident.create "dist_id" in
    let distance =
      Cop (Cmodi, [Cvar i2; Cconst_int 64], nodbg) in
    Clet (distance_ident, distance,
      rotate_left (Cvar i1) (Cvar distance_ident) width) in

    let rotate_right_i32 =
    let width = 32 in
    let low_set_ident = Ident.create "ror32_low_set" in
    let low_set = unset_high_32 (Cvar i1) in
    (* Next, normalise rotate length *)
    let distance_ident = Ident.create "dist_id" in
    let distance =
      Cop (Cmodi, [unset_high_32 (Cvar i2); Cconst_int 32], nodbg) in
    Clet (low_set_ident, low_set,
      Clet (distance_ident, distance,
        rotate_right (Cvar low_set_ident) (Cvar distance_ident) width)) in

  let rotate_right_i64 =
    let width = 64 in
    let distance_ident = Ident.create "dist_id" in
    let distance =
      Cop (Cmodi, [Cvar i2; Cconst_int width], nodbg) in
    Clet (distance_ident, distance,
      rotate_right (Cvar i1) (Cvar distance_ident) width) in


  let rotate_left = if is_32 then rotate_left_i32 else rotate_left_i64 in
  let rotate_right = if is_32 then rotate_right_i32 else rotate_right_i64 in

  let cs op = compile_op_simple env op v1 v2 in
  let cf32 op = compile_f32_op env op v1 v2 in
  (* let cn op signed = compile_op_normalised signed env op v1 v2 in *)

  let min_or_max ~is_f32 ~is_min =
    let (i1, i2) = (lv env v1, lv env v2) in
    let op = if is_min then CFle else CFge in
    let (arg_f, result_f) =
      if is_f32 then (f64_of_f32, f32_of_f64)
      else ((fun x -> x), (fun x -> x)) in

    let zero_min_f =
      "wasm_rt_zero_min_" ^ (if is_f32 then "f32" else "f64") in
    let zero_max_f =
      "wasm_rt_zero_max_" ^ (if is_f32 then "f32" else "f64") in

    (* Check whether either operand is a NaN *)
    (* It so turns out that Pervasives.nan ain't good enough. *)
    let wasm_nan =
        result_f @@ Cconst_float (Libwasm.F64.pos_nan |> Libwasm.F64.to_float) in

    let float_eq f1 f2 =
      Cop (Ccmpf CFeq, [f1 ; f2], nodbg) in

    let float_eq_zero f = float_eq f (Cconst_float 0.0) in

    (* Check if both are zeros *)
    (* Check NaN special case *)
    Cifthenelse (
      Cop (Cor, [is_nan (arg_f @@ Cvar i1); is_nan (arg_f @@ Cvar i2)], nodbg),
      wasm_nan,
      Cifthenelse (
        Cop (Cand, [float_eq_zero (arg_f @@ Cvar i1); float_eq_zero (arg_f @@ Cvar i2)], nodbg),
        (* TODO: I really can't do better than this at the moment. The WASM spec requires
         * us to check the sign bit properly when comparing zeros, and there's no easy way
         * to do so from CMM that I can see. *)
        begin
          let call name =
            Cop (Cextcall (name, typ_float, false, None),
                [Cvar i1; Cvar i2], nodbg) in
           if is_min then call zero_min_f else call zero_max_f
        end,
        Cifthenelse (
            Cop (Ccmpf op, [arg_f @@ Cvar i1; arg_f @@ Cvar i2], nodbg),
            Cvar i1, Cvar i2))) in

  let compile_int_op =
    let open Libwasm.Ast.IntOp in
    function
    | Add -> cs Caddi
    | Sub -> cs Csubi
    | Mul -> cs Cmuli
    | DivS -> divide true
    | DivU -> divide false
    | RemS -> rem true
    | RemU -> rem false
    | And -> cs Cand
    | Or -> cs Cor
    | Xor -> cs Cxor
    | Shl -> shift_left
    | ShrS -> shift_right true
    | ShrU -> shift_right false
    | Rotl -> rotate_left
    | Rotr -> rotate_right in
  let compile_float_op =
    let open Libwasm.Ast.FloatOp in
    function
    | Add -> cs Caddf
    | Sub -> cs Csubf
    | Mul -> cs Cmulf
    | Div -> cs Cdivf
    | Min -> min_or_max ~is_f32:false ~is_min:true
    | Max -> min_or_max ~is_f32:false ~is_min:false
    | CopySign ->
        Cop (Cextcall ("copysign", typ_float, false, None),
          [Cvar i1; Cvar i2], nodbg) in
  let compile_float32_op =
    let open Libwasm.Ast.FloatOp in
    function
    | Add -> cf32 Caddf
    | Sub -> cf32 Csubf
    | Mul -> cf32 Cmulf
    | Div -> cf32 Cdivf
    | Min -> min_or_max ~is_f32:true ~is_min:true
    | Max -> min_or_max ~is_f32:true ~is_min:false
    | CopySign ->
        Cop (Cextcall ("copysignf", typ_float, false, None),
          [Cvar i1; Cvar i2], nodbg) in
  match op with
    | I32 i32op -> compile_int_op i32op
    | I64 i64op -> compile_int_op i64op
    | F32 f32op -> compile_float32_op f32op
    | F64 f64op -> compile_float_op f64op

let compile_unop env op v =
  let open Libwasm.Values in

  let compile_int_op ~is_32 =
    let open Libwasm.Ast.IntOp in
    let arg = Cvar (lv env v) in
    let add_suffix s = s ^ (if is_32 then "_u32" else "_u64") in
    let call name = Cextcall (add_suffix name, typ_int, false, None) in
    function
      | Clz -> Cop (call "wasm_rt_clz", [arg], nodbg)
      | Ctz -> Cop (call "wasm_rt_ctz", [arg], nodbg)
      | Popcnt -> Cop (call "wasm_rt_popcount", [arg], nodbg) in

  let compile_float_op ~is_32 =
    let open Libwasm.Ast.FloatOp in

    let i = lv env v in
    let call name =
      Cop (Cextcall (name, typ_float, false, None), [Cvar i], nodbg) in

    let call_floats f32_name f64_name =
      if is_32 then call f32_name
      else call f64_name in

    function
      (* Unfortunately, in 32-bit mode, we need to do an RTS call since
       * the cast-to-64-bit trick loses information when combined with
       * neg and abs, it seems. Nonsense to do with NaNs. *)
      | Neg ->
          if is_32 then call "wasm_rt_neg_f32"
          else Cop (Cnegf, [Cvar i], nodbg)
      | Abs ->
          if is_32 then call "fabsf"
          else Cop (Cabsf, [Cvar i], nodbg)
      | Ceil ->
          if is_32 then call "ceilf"
          else call "ceil"
      | Floor -> call_floats "floorf" "floor"
      | Trunc -> call_floats "truncf" "trunc"
      | Nearest -> call_floats "nearbyintf" "nearbyint"
      | Sqrt -> call_floats "sqrtf" "sqrt" in

  match op with
    | I32 i32op -> compile_int_op ~is_32:true i32op
    | I64 i64op -> compile_int_op ~is_32:false i64op
    | F32 f32op -> compile_float_op ~is_32:true f32op
    | F64 f64op -> compile_float_op ~is_32:false f64op

let compile_cvtop env op v =
  let open Libwasm.Values in
  let var = Cvar (lv env v) in

  let call_float name args =
    Cop (Cextcall (name, typ_float, false, None), args, nodbg) in

  let call_int name args =
    Cop (Cextcall (name, typ_int, false, None), args, nodbg) in

  let float_of_int x =
    Cop (Cfloatofint, [x], nodbg) in
  let int_of_float x =
    Cop (Cintoffloat, [x], nodbg) in

  let compile_int_op ~is_32 =

    let truncate_signed_float is_float_32 =
      (* Checks needed:
       * - NaNs should trap
       * - Infinity and negative infinity should trap
       * - Any integer greater than i32 max and less than i64 max should trap *)

      let result_ident = Ident.create "result" in
      let ri_var = Cvar result_ident in

      let cmp_ident = Ident.create "cmp" in
      let cmp_var = Cvar cmp_ident in

      let (min, max) =
        (* If integer (destination) is 32-bits, min and max need to be 32-bit
         * MIN/MAX_INT *)
        if is_32 then
          let min = Cconst_float (Int32.to_float Int32.min_int) in
          let max = Cconst_float (Int32.to_float Int32.max_int) in
          (min, max)
        else
          let min = Cconst_float (Int64.to_float Int64.min_int) in
          let max = Cconst_float (Int64.to_float Int64.max_int) in
          (min, max) in
      (* If float is 32 bits, we need to first promote it to 64 bits *)
      let cmp =
        if is_float_32 then f64_of_f32 var else var in
      let max_op =
        if is_32 && (not is_float_32) then CFgt else CFge in

      Clet (cmp_ident, cmp,
        Cifthenelse (is_nan cmp_var,
          trap_int TrapInvalidConversion,
          Clet (result_ident, int_of_float cmp_var,
          Cifthenelse (
              Cop (Cor, [
                Cop (Ccmpf CFlt, [cmp_var; min], nodbg);
                Cop (Ccmpf max_op, [cmp_var; max], nodbg)
              ], nodbg),
            trap_int TrapIntOverflow,
            ri_var)))) in

    let truncate_unsigned_float is_float_32 =
      (* We have to do an RTS call since CMM doesn't support unsigned
       * conversions. Hacker's Delight has some bit hackery, but they
       * don't work for the full range of values. *)
      let cvt_to = if is_32 then "u32" else "u64" in
      let cvt_from = if is_float_32 then "f32" else "f64" in
      let call_name = Printf.sprintf "wasm_rt_trunc_%s_%s" cvt_to cvt_from in
      Cop (Cextcall (call_name, typ_int, false, None),
        [var], nodbg) in

    let open Libwasm.Ast.IntOp in
    function
      | ExtendSI32 -> sign_extend_i32 var
      | ExtendUI32 -> unset_high_32 var
      | WrapI64 ->
          (* Amusingly, since we're emulating i32s using i64s
           * anyway, we actually don't need to do anything for
           * this case at all :) *)
          var
      | TruncSF32 -> truncate_signed_float true
      | TruncUF32 -> truncate_unsigned_float true
      | TruncSF64 -> truncate_signed_float false
      | TruncUF64 -> truncate_unsigned_float false
      | ReinterpretFloat ->
          (* It would be nice to be able to use the CMM load / store
           * mechanism for copying between registers. For now, we can
           * emulate reinterpret operations using a memcpy in the RTS. *)
          let suffix = if is_32 then "_u32" else "_u64" in
          call_int ("wasm_rt_reinterpret" ^ suffix) [var] in

  let compile_float_op ~is_32 =
    let open Libwasm.Ast.FloatOp in

    let convert_unsigned_int is_int_32 =
      (* Again, we need to call the RTS to do these conversions :( *)
      let cvt = if is_int_32 then unset_high_32 var else var in
      let cvt_to = if is_32 then "f32" else "f64" in
      let cvt_from = if is_int_32 then "u32" else "u64" in
      let call_name = Printf.sprintf "wasm_rt_convert_%s_%s" cvt_to cvt_from in
      call_float call_name [cvt] in

    function
      | ConvertSI32 ->
          let cvt = float_of_int (sign_extend_i32 var) in
          if is_32 then f32_of_f64 cvt else cvt
      | ConvertUI32 -> convert_unsigned_int true
      | ConvertSI64 ->
          let cvt = float_of_int var in
          if is_32 then f32_of_f64 cvt else cvt
      | ConvertUI64 -> convert_unsigned_int false
      | PromoteF32 -> f64_of_f32 var
      | DemoteF64 -> f32_of_f64 var
      | ReinterpretInt ->
          let suffix = if is_32 then "_f32" else "_f64" in
          call_float ("wasm_rt_reinterpret" ^ suffix) [var] in

  match op with
    | I32 i32op -> compile_int_op ~is_32:true i32op
    | I64 i64op -> compile_int_op ~is_32:false i64op
    | F32 f32op -> compile_float_op ~is_32:true f32op
    | F64 f64op -> compile_float_op ~is_32:false f64op

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
  | GetGlobal g ->
      begin
        let load_global g =
          let symbol =
            Cconst_symbol (Compile_env.global_symbol g env) in
          Cmm_rts.Globals.get ~symbol ~ty:(Global.type_ g) in

        match Global.data g with
          | DefinedGlobal { initial_value = Constant v} ->
            (* If global is mutable, then we need to read from
             * its associated symbol. *)
            if Global.is_mutable g then load_global g
            else
              (* If it's immutable, we may simply return its (compiled) initial
               * value. *)
              compile_value v
          | DefinedGlobal { initial_value = AnotherGlobal g } ->
              (* If we're referencing another global, then we know by
               * the specification that the other global *must* be an
               * imported global. *)
              load_global g
          | ImportedGlobal _ ->
              (* If we're referencing an imported global, we load that. *)
              load_global g
      end
  | Load (loadop, v) ->
      Cmm_rts.Memory.load
        ~root:(Cconst_symbol (Compile_env.memory_symbol env))
        ~dynamic_pointer:(unset_high_32 @@ Cvar (lv env v))
        ~op:loadop
  | MemorySize ->
      Cmm_rts.Memory.size (Cconst_symbol (Compile_env.memory_symbol env))
  | MemoryGrow v ->
      Cmm_rts.Memory.grow
        (Cconst_symbol (Compile_env.memory_symbol env))
        (Cvar (lv env v))
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
      compile_relop env rel v1 v2
  | Unary (un, v) ->
      compile_unop env un v
  | Binary (bin, v1, v2) ->
      compile_binop env bin v1 v2
  | Convert (cvt, v) ->
      compile_cvtop env cvt v

let compile_terminator env =
  let open Stackless in
  let branch b =
    let lbl_id = Branch.label b |> Label.id in
    let args =
      List.map (fun v ->
        Cvar (lv env v)) (Branch.arguments b) in

    if Label.Id.is_return lbl_id then
      begin
        match args with
          | [] ->
              Ctuple []
          | [x] -> x
          | _ -> failwith "Can only currently return a single argument from a function"
      end
    else
      let br_id = Compile_env.lookup_label (Branch.label b) env in
      Cexit (br_id, args) in

  let local_call_noreturn fn_addr =
      (* No return values: typ_void in call, csequence, and branch args *)
    fun args_cvars br_id cont_args_cvars ->
      let call = Cop (Capply typ_void, fn_addr :: args_cvars, nodbg) in
      Csequence (call, Cexit (br_id, cont_args_cvars)) in

  let local_call_return1 fn_addr =
    fun args_cvars br_id cont_args_cvars ty ->
      let call =
        Cop (Capply (compile_type ty), fn_addr :: args_cvars, nodbg) in
      Cexit (br_id, call :: cont_args_cvars) in

  let import_call_noreturn name =
    fun args_cvars br_id cont_args_cvars ->
      let call =
        Cop (Cextcall (name, typ_void, false, None),
          args_cvars, nodbg) in
      Csequence (call, Cexit (br_id, cont_args_cvars)) in

  let import_call_return1 name =
    fun args_cvars br_id cont_args_cvars ty ->
      let call =
        Cop (Cextcall (name, compile_type ty, false, None),
          args_cvars, nodbg) in
      Cexit (br_id, call :: cont_args_cvars) in


  let call fn_noreturn fn_return1 ret_tys args cont =
    (* Calculate new fuel *)
      let new_fuel = Cop (Csubi, [Cvar fuel_ident; Cconst_int 1], nodbg) in
      let args_cvars = (List.map (fun v -> Cvar (lv env v)) args) @ [new_fuel] in
      let br_id = Compile_env.lookup_label (Branch.label cont) env in
      let branch_args = Branch.arguments cont in
      let cont_args_cvars =
        (List.map (fun v -> Cvar (lv env v)) branch_args) in
      (* One return value: let-composition, result goes on head of args *)
      match ret_tys with
        | [] -> fn_noreturn args_cvars br_id cont_args_cvars
        | [ty] -> fn_return1 args_cvars br_id cont_args_cvars ty
        | _ -> assert false in
  function
    | Unreachable ->
        (* FIXME: Dodgy. We'll need some more info to decide whether to
         * trap_int or trap_float. *)
        trap_int TrapUnreachable
    | Br b -> branch b
    | BrTable { index ; es ; default } ->
        (* Bounds check needs to be done at runtime. *)
        let lbl_id = Ident.create "br_table_lbl" in
        let branches = es @ [default] in
        let default_id = (List.length branches) - 1 in
        let branches_exprs =
          List.map branch branches
          |> Array.of_list in
        let switch_ids = Array.make (List.length branches) 0 in
        let () = Array.iteri (fun i _ -> switch_ids.(i) <- i) switch_ids in
        let idx = lv env index in

        Clet (lbl_id,
          Cifthenelse (
            (* Test whether index exceeds bounds *)
            Cop (Ccmpa Cge,
              [Cvar idx; Cconst_int (List.length es)], nodbg),
            (* If so, return the default branch *)
            Cconst_int (default_id),
            (* Otherwise, return the index *)
            Cvar idx),
            (* Implement br_table using Cswitch *)
            Cswitch (Cvar lbl_id, switch_ids, branches_exprs, nodbg))
    | If { cond; ifso; ifnot } ->
        let cond_var = Cvar (lv env cond) in
        let test =
          Cop (Ccmpi Cne, [cond_var; Cconst_natint Nativeint.zero],
            nodbg) in
        let true_branch = branch ifso in
        let false_branch = branch ifnot in
        Cifthenelse (test, true_branch, false_branch)
    | Call { func ; args; cont } ->
        let (FuncType (_arg_tys, ret_tys)) = Func.type_ func in
        let symb = Compile_env.func_symbol func env in
        let (noreturn, return1) =
          (* We need slightly different functions based on whether or
           * not the symbol is imported or not (imported calls must
           * use an extcall) *)
          if Func.is_imported func then
            (import_call_noreturn symb),
            (import_call_return1 symb)
          else
            (local_call_noreturn (Cconst_symbol symb)),
            (local_call_return1 (Cconst_symbol symb)) in
        call (noreturn) (return1) ret_tys args cont
    | CallIndirect { type_; func; args; cont } ->
        (* Normalise function index *)
        let func_ident = Ident.create "func_id" in
        let func_var = Cvar func_ident in
        let func = unset_high_32 (Cvar (lv env func)) in
        let FuncType (_arg_tys, ret_tys) = type_ in

        let in_bounds =
          let table_size = Cmm_rts.Tables.count env in
          Cop (Ccmpa Clt, [func_var; table_size], nodbg) in

        let hashes_match =
          let this_hash = Util.Type_hashing.hash_function_type type_ in
          let that_hash = Cmm_rts.Tables.function_hash env func in
          Cop (Ccmpa Ceq, [Cconst_natint this_hash; that_hash], nodbg) in

        Clet (func_ident, func,
          Cifthenelse (
            in_bounds,
            Cifthenelse (hashes_match,
              call
                (local_call_noreturn @@
                  Cmm_rts.Tables.function_pointer env func_var)
                (local_call_return1 @@
                  Cmm_rts.Tables.function_pointer env func_var)
                ret_tys
                args
                cont,
              trap_function_ty ret_tys TrapCallIndirect),
            trap_function_ty ret_tys TrapCallIndirect
          )
        )

(* compile_body: env -> W.terminator -> W.statement list -> Cmm.expression *)
let rec compile_body env terminator = function
  | [] -> compile_terminator env terminator
  | x :: xs ->
      let open Stackless in
      begin
      match x with
        | Cont (lbl, binders, is_rec, body) ->
            let rec_flag = if is_rec then Recursive else Nonrecursive in
            (* Bind vars, and build up (ident, machtype) pairs *)
            let (idents_rev, env) =
              List.fold_left (fun (acc, env) v ->
                let (ident, env) = bind_var v env in
                let mty = compile_type (Var.type_ v) in
                ((ident, mty) :: acc, env)
              ) ([], env) binders in
            let idents = List.rev idents_rev in
            let (lbl_id, env) = Compile_env.bind_label lbl env in
            let catch_clause =
              (lbl_id, idents, compile_term env body) in
            let cont = compile_body env terminator xs in
            Ccatch (rec_flag, [catch_clause], cont)
        | Let (v, e) ->
            let (ident, env) = bind_var v env in
            let e1 = compile_expression env e in
            Clet (ident, e1, compile_body env terminator xs)
        | Effect (SetGlobal (g, v)) ->
            let symbol = Cconst_symbol (Compile_env.global_symbol g env) in
            Csequence (
              Cmm_rts.Globals.set
                ~symbol
                ~ty:(Global.type_ g)
                ~to_store:(Cvar (lv env v)),
              compile_body env terminator xs
            )
        | Effect (Store { op; index; value }) ->
            Csequence (
              Cmm_rts.Memory.store
                ~root:(Cconst_symbol (Compile_env.memory_symbol env))
                ~dynamic_pointer:(unset_high_32 @@ Cvar (lv env index))
                ~op
                ~to_store:(Cvar (lv env value)),
            compile_body env terminator xs)

      end
and compile_term env term = compile_body env (term.terminator) (term.body)


(* IR function to CMM function *)
let compile_function (ir_func: Stackless.func) func_md env =
  (* Name *)
  let name = Compile_env.func_symbol func_md env in
  (* Arguments: Need to bind each param in the env we will
   * use to compile the function, and pair with machtype *)
  let FuncType (arg_tys, ret_tys) = Func.type_ func_md in
  let tty =
    match ret_tys with
      | [] -> typ_void
      | [x] -> trap_ty x
      | _ -> assert false in
  let zipped =
    List.combine ir_func.params arg_tys in
  let (args_rev, env) =
    List.fold_left (fun (acc, env) (param, ty) ->
      let (ident, env) = bind_var param env in
      let cmm_ty = compile_type ty in
      ((ident, cmm_ty) :: acc, env)) ([], env) zipped in
  (* With updated env, compile function body *)
  let body =
    compile_term env ir_func.body
    |> normalise_function ret_tys
    |> add_exhaustion_check tty in
  (* Finally, we can put it all together... *)
  {
    fun_name = name;
    fun_args = List.rev args_rev @ [(fuel_ident, typ_int)];
    fun_body = body;
    fun_codegen_options = [No_CSE];
    fun_dbg = nodbg
  }

let compile_functions env (ir_mod: Stackless.module_) =
  Int32Map.bindings ir_mod.function_ir
  |> List.map (fun (id, func_ir) ->
      let md = Int32Map.find id (ir_mod.function_metadata) in
      Cfunction (compile_function func_ir md env))

let init_function module_name env (ir_mod: Stackless.module_) data_info =
  let name_prefix = (Util.Names.sanitise module_name) ^ "_" in
  let call_seq =
    List.fold_left (fun acc x -> Csequence (acc, x)) (Ctuple []) in

  let global_body =
    let memcpy (_, g) =
      match Global.data g with
        | DefinedGlobal { initial_value = Constant _ } ->
            (* Defined at symbol export time, we don't need to do anything here. *)
            Ctuple []
        | DefinedGlobal { initial_value = AnotherGlobal g2 } ->
            let symb = Compile_env.global_symbol g env in
            Cop (Cextcall ("memcpy", typ_void, false, None),
              [ Cconst_symbol symb;
                Cconst_symbol (Compile_env.global_symbol g2 env);
                Cconst_int Arch.size_int], nodbg)
        | ImportedGlobal _ ->
            (* Loads will happen directly from the other symbol,
             * nothing to copy here *)
            Ctuple [] in
    call_seq (List.map (memcpy) (Int32Map.bindings ir_mod.globals)) in

  let memory_body =
    let open Libwasm.Types in
    let root_ident = Ident.create "data_root" in
    let init_data lims =
      (* Generate memcpy calls to initialise memory *)
      let memcpy (symb, offset, size) =
        let addr = Cop (Caddi,
          [Cvar root_ident; compile_expression env offset], nodbg) in
        Cop (Cextcall ("memcpy", typ_void, false, None),
          [addr; Cconst_symbol symb; Cconst_int size], nodbg) in
        call_seq (List.map (memcpy) data_info) in

    let memory_symbol = Cconst_symbol (Compile_env.memory_symbol env) in

    match ir_mod.memory_metadata with
      | None -> Ctuple []
      | Some (ImportedMemory { limits }) ->
          let root =
            Cop (Cload (Word_int, Mutable),
              [memory_symbol], nodbg) in
          Clet (root_ident, root, init_data limits)
      | Some (LocalMemory (MemoryType lims)) ->
        (* Initialise struct representing this module's memory *)
        let root =
          Cop (Cload (Word_int, Mutable),
            [memory_symbol], nodbg) in
        (* Set up memory page limits *)
        let max_addressable_pages = 65535 in
        let min_pages = Cconst_natint (Nativeint.of_int32 lims.min) in
        let max_pages =
          begin
            match lims.max with
              | Some x -> Cconst_natint (Nativeint.of_int32 x)
              | None -> Cconst_natint (Nativeint.of_int max_addressable_pages)
          end in
        (* Perform allocation, then initialise data *)
          Csequence (
            Cop (
              Cextcall ("wasm_rt_allocate_memory", typ_void, false, None),
              [memory_symbol; min_pages; max_pages],
              nodbg
            ),
            Clet (root_ident, root, init_data lims)) in

  (* Initialise the table with elements *)
  let table_body =
    let compile_elem (offset, elem_map) =
      Int32Map.fold (fun idx func acc ->
        let hash = Util.Type_hashing.hash_function_type (Func.type_ func) in
        let symb = Compile_env.func_symbol func env in
        let cmm_idx =
          Cop (Caddi, [compile_expression env offset;
            Cconst_natint (Nativeint.of_int32 idx)], nodbg) in
        Csequence (Cmm_rts.Tables.set_table_entry env cmm_idx hash symb, acc)
      ) elem_map (Ctuple []) in
    List.map (compile_elem) ir_mod.table_elems |> call_seq in

  let start_body =
    match ir_mod.start with
      | Some md ->
          let symbol_name = Compile_env.func_symbol md env in
          let fn_symbol = Cconst_symbol symbol_name in
          let fuel = Cconst_int (Util.Command_line.initial_fuel ()) in
          Cop (Capply typ_void, [fn_symbol; fuel], nodbg)
      | _ -> Ctuple [] in

  (* Sequence all instructions. Later passes perform the `() ; M ~~> M`
   * and `M ; () ~~> M` translations, so we don't have to worry about unit
   * values floating around. *)
  let init_instrs = [
    global_body;
    memory_body;
    table_body;
    start_body
  ] in

  let body = call_seq init_instrs in

  Cfunction {
    fun_name = name_prefix ^ "initinternal";
    fun_args = [];
    fun_body = body;
    fun_codegen_options = [No_CSE];
    fun_dbg = nodbg
  }

(* Returns a Cmm.phrase list, and a (symbol, offset, size) list.
 * The (symbol, offset, size) list is a list of data symbols along with
 * their respective sizes. *)
let module_data name (ir_mod: Stackless.module_) =
  let name_prefix = (Util.Names.sanitise name) ^ "_data_" in

  let emit_string_constant symb s =
    let n = Arch.size_int - 1 - (String.length s) mod Arch.size_int in
    [Cdefine_symbol symb; Cstring s; Cskip n; Cint8 n] in

  let (_, data_rev, info_rev) =
    List.fold_left (fun (i, cmm_data, symbol_info) (data: Stackless.data) ->
      let symb = name_prefix ^ (string_of_int i) in
      let size = String.length data.contents in
      let list_entry = (symb, data.offset, size) in
      let data = emit_string_constant symb data.contents in
      (i + 1, data :: cmm_data, list_entry :: symbol_info))
    (0, [], []) ir_mod.data in
  (List.rev data_rev |> List.concat, List.rev info_rev)


let module_globals env (ir_mod: Stackless.module_) export_info =
  let open Libwasm.Values in
  let global_bindings = Int32Map.bindings ir_mod.globals in

  let compile_data_value = function
    | I32 x -> Cint32 (Nativeint.of_int32 x)
    | I64 x -> Cint (Int64.to_nativeint x)
    | F32 x -> Cint32 (Libwasm.F32.to_bits x |> Nativeint.of_int32)
    | F64 x -> Cint (Libwasm.F64.to_bits x |> Int64.to_nativeint) in

  (* We can initialise certain variables (i.e., those initialised with
   * a constant expression) at compile-time to reduce the number of
   * memcpy operations we need *)
  List.map (fun (wasm_id, g) ->

    let exported_symbols =
      (match Int32Map.find_opt wasm_id export_info.global_symbols with
        | Some symbs ->
            List.map (fun x ->
              let name =
                Printf.sprintf "%s_global_%s" (Compile_env.module_name env) x in
              [Cglobal_symbol name; Cdefine_symbol name]) symbs
        | None -> []) |> List.concat in

    let make_symbol dat =
      let symb = Compile_env.global_symbol g env in
      exported_symbols @
        [Cglobal_symbol symb; Cdefine_symbol symb; dat] in

    match Global.data g with
      | DefinedGlobal { initial_value = Constant lit } ->
          make_symbol (compile_data_value lit)
      | DefinedGlobal _ ->
          (* We need a symbol, but don't have the global initialisation
           * information available at compile-time, so we set to 0 *)
          make_symbol (Cint Nativeint.zero)
      | ImportedGlobal _ ->
          (* Don't need to generate a symbol for an imported global,
           * as this will be generated by the module exporting the
           * global *)
          []
  ) global_bindings
  |> List.concat

let module_function_table env (ir_mod: Stackless.module_) export_info =
  let ir_table = ir_mod.table in
  let table_exports =
    List.map (fun x ->
      let name = Printf.sprintf "%s_table_%s"
        (Compile_env.module_name env) x in
      [Cglobal_symbol name; Cdefine_symbol name])
      export_info.table_symbols |> List.concat in

  match ir_table with
    | Some (LocalTable limits) ->
        let table_size =
          (Arch.size_int * 2 * (Int32.to_int limits.min)) in
        let max =
          match limits.max with
            | Some m -> m | None -> Int32.zero in
        table_exports @
        [Cdefine_symbol (Compile_env.table_symbol env);
         Cint (Nativeint.of_int32 limits.min);
         Cint (Nativeint.of_int32 max);
         Cskip (table_size)]
    | Some (ImportedTable { limits }) ->
        (* If we're importing a table instead of defining one,
         * then we don't need to do define any table symbols
         * since we'll use symbols from the other modules *)
        []
    | None -> []

let module_memory env (ir_mod: Stackless.module_) export_info =
  let memory_exports =
    List.map (fun x ->
      let name = Printf.sprintf "%s_memory_%s" (Compile_env.module_name env) x in
      [Cglobal_symbol name; Cdefine_symbol name])
      export_info.memory_symbols |> List.concat in

  (* Memory symbol: needs 3 words of space to store struct created by RTS *)
  (* Must only define a memory symbol for an internal memory symbol! *)
  match ir_mod.memory_metadata with
    | Some (LocalMemory _) ->
      let struct_size = Arch.size_int * 3 in
      let memory_symb =
        [Cdefine_symbol (Compile_env.memory_symbol env); Cskip struct_size] in
      memory_exports @ memory_symb
    | Some (ImportedMemory { module_name; memory_name }) ->
        memory_exports @ [Csymbol_address (Compile_env.memory_symbol env)]
    | None -> []

let split_exports : Stackless.module_ -> export_info = fun ir_mod ->
  let open Libwasm.Ast in
  let empty =
    { func_symbols = Int32Map.empty;
      global_symbols = Int32Map.empty;
      memory_symbols = []; table_symbols = [] } in

  List.fold_left (fun acc (x: Libwasm.Ast.export) ->
    let x = x.it in
    let name = Util.Names.sanitise (Util.Names.name_to_string x.name) in
    let add_or_update k v m =
      Int32Map.update k (fun xs_opt ->
        match xs_opt with
          | Some xs -> Some (v :: xs)
          | None -> Some [v]
      ) m in

    match x.edesc.it with
      | FuncExport v ->
          { acc with func_symbols = add_or_update v.it name acc.func_symbols }
      | GlobalExport v ->
          { acc with global_symbols = add_or_update v.it name acc.global_symbols }
      | TableExport _ ->
          { acc with table_symbols = name :: acc.table_symbols }
      | MemoryExport _ ->
          { acc with memory_symbols = name :: acc.memory_symbols }) empty ir_mod.exports


(* Exported function symbols have to be handled slightly differently, since CMM
 * distinguishes between functions and data items. Nonetheless, each function symbol
 * can straightforwardly refer to a function pointer, which can be used directly
 * by Capply calls. *)
let module_function_exports env (ir_mod: Stackless.module_) =
  let open Libwasm.Ast in
  let global_symbol name dat =
    [Cglobal_symbol name; Cdefine_symbol name; dat] in
  let export_symbol name =
    Printf.sprintf "%s_func_%s" (Compile_env.module_name env) name in
  let sanitise name = Util.Names.(name_to_string name |> sanitise) in

  List.map (fun (x: Libwasm.Ast.export) ->
    let x = x.it in
    let name = sanitise x.name in
    match x.edesc.it with
      | FuncExport v ->
          let func = Int32Map.find v.it (ir_mod.function_metadata) in
          let symbol = export_symbol name in
          let internal_symbol = Compile_env.func_symbol func env in
          global_symbol symbol (Csymbol_address internal_symbol)
      | _ -> []
  ) ir_mod.exports
  |> List.rev
  |> List.concat

  (* IR function to CMM phrase list *)
let compile_module name (ir_mod: Stackless.module_) =
  let sanitised_name = (Util.Names.sanitise name) in
  let env =
    Compile_env.empty
      ~module_name:sanitised_name
      ~memory:ir_mod.memory_metadata
      ~table:ir_mod.table in
  let (elem_data, data_info) = module_data name ir_mod in
  let init = init_function name env ir_mod data_info in

  let export_info = split_exports ir_mod in
  let func_exports = module_function_exports env ir_mod in
  let memory = module_memory env ir_mod export_info in
  let global_data = module_globals env ir_mod export_info in
  let table = module_function_table env ir_mod export_info in
  let data =
    Cdata
      ([func_exports;
        elem_data;
        memory;
        global_data;
        table
      ] |> List.concat) in

  let funcs = compile_functions env ir_mod @ [init] in
  funcs @ [data]

