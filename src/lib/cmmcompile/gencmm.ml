(* Compile Stackless into CMM *)
open Ir
open Cmm
open Cmm_trap
open Symbol

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

let is_infinity x =
  Cop (Cor,
      [ Cop (Ccmpf CFeq, [x; Cconst_float (infinity)], nodbg);
        Cop (Ccmpf CFeq, [x; Cconst_float (neg_infinity)], nodbg)], nodbg)


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
        f32_of_f64 @@ Cop (Cextcall ("copysign", typ_float, false, None),
          [f64_of_f32 @@ Cvar i1; f64_of_f32 @@ Cvar i2], nodbg) in
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

  let compile_float_op arg_f result_f =
    let open Libwasm.Ast.FloatOp in
    let i = lv env v in
    let call name =
      Cop (Cextcall (name, typ_float, false, None), [arg_f @@ Cvar i], nodbg)
      |> result_f in
    function
      | Neg -> Cop (Cnegf, [arg_f @@ Cvar i], nodbg) |> result_f
      | Abs -> Cop (Cabsf, [arg_f @@ Cvar i], nodbg) |> result_f
      | Ceil -> call "ceil"
      | Floor -> call "floor"
      | Trunc -> call "trunc"
      | Nearest ->
          (* Nearest is *frustratingly close* to C's round function, but alas special-cases
           * the numbers between -1 and 0, and 0 and 1. Since we have to do a C call to
           * round anyway, it's easiest to just implement nearest in the RTS. *)
          call "wasm_rt_nearest_f64"
      | Sqrt -> call "sqrt" in

  let compile_float64_op = compile_float_op (fun x -> x) (fun x -> x) in
  let compile_float32_op = compile_float_op f64_of_f32 f32_of_f64 in

  match op with
    | I32 i32op -> compile_int_op ~is_32:true i32op
    | I64 i64op -> compile_int_op ~is_32:false i64op
    | F32 f32op -> compile_float32_op f32op
    | F64 f64op -> compile_float64_op f64op

let compile_cvtop env op v =
  let open Libwasm.Values in
  let var = Cvar (lv env v) in
  let unimplemented_float = Cconst_float 0.0 in
  let unimplemented_int = Cconst_int 0 in
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
      let call_name =
        (* There must be a prettier way of doing this... *)
        match (is_32, is_float_32) with
          | (true, true) -> "wasm_rt_trunc_u32_f32"
          | (true, false)-> "wasm_rt_trunc_u32_f64"
          | (false, true) -> "wasm_rt_trunc_u64_f32"
          | (false, false) -> "wasm_rt_trunc_u64_f64" in
      (* We have to do an RTS call since CMM doesn't support unsigned
       * conversions. Hacker's Delight has some bit hackery, but they
       * don't work for the full range of values. *)
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
(*
    let convert_signed_float is_float_32 =
      (* Checks needed:
       * - NaNs should trap
       * - Any float greater than i32 max and less than i64 max should trap *)

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
  *)



    let open Libwasm.Ast.FloatOp in

    function
      | ConvertSI32 ->
          let cvt = float_of_int (sign_extend_i32 var) in
          if is_32 then f32_of_f64 cvt else cvt
      | ConvertUI32 -> unimplemented_float
      | ConvertSI64 ->
          let cvt = float_of_int var in
          if is_32 then f32_of_f64 cvt else cvt
      | ConvertUI64 -> unimplemented_float
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
      (* If global is mutable, then we need to read from
       * its associated symbol. *)
      if Global.is_mutable g then
        let symbol =
          Cconst_symbol (Compile_env.global_symbol env g) in
        Cmm_rts.Globals.get ~symbol ~ty:(Global.type_ g)
      else
        (* If it's immutable, we may simply return its (compiled) initial
         * value. *)
        compile_value (Global.initial_value g)
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
            Cop (Ccmpi Cge,
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
        (* Alright. We have a function, set of args, and a
         * continuation branch, pre-populated with some args.
         * In addition, the continuation will take the return
         * type(s) of the function, so should be prepended.
         * ...I think? *)
        let (FuncType (_arg_tys, ret_tys)) = Func.type_ func in
        (* Calculate new fuel *)
        let new_fuel = Cop (Csubi, [Cvar fuel_ident; Cconst_int 1], nodbg) in
        let args_cvars = (List.map (fun v -> Cvar (lv env v)) args) @ [new_fuel] in
        (* Lookup function symbol from table *)
        let symbol_name = Compile_env.lookup_func_symbol func env |> Symbol.name in
        let fn_symbol = Cconst_symbol symbol_name in
        (* Cop (Capply <return type>, <args>, dbg) *)
        let br_id = Compile_env.lookup_label (Branch.label cont) env in
        let branch_args = Branch.arguments cont in
        let cont_args_cvars =
          (List.map (fun v -> Cvar (lv env v)) branch_args) in
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
    | CallIndirect { type_; func; args; cont } ->
        (* TODO: implement *)
        Ctuple []

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
            let symbol = Cconst_symbol (Compile_env.global_symbol env g) in
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
  let name =
    Compile_env.lookup_func_symbol func_md env
    |> Symbol.name in
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

(* Now we've named the functions in the IR pass, we can simplify this. *)
let rec populate_symbols module_name env (ir_module: Stackless.module_) : Compile_env.t =
  let open Libwasm.Ast in
  let name_prefix = (Util.Names.sanitise module_name) ^ "_" in
  Util.Maps.Int32Map.fold (fun _ (_, md) acc ->
    match Func.name md with
      | Some name ->
          let name =
            if Util.Command_line.generate_c () then
              Util.Names.internal_name (name_prefix ^ name)
            else
              name_prefix ^ name in
          (* Internal name if we're generating CMM; standard name if not *)
          Compile_env.bind_global_func_symbol md name acc
      | None -> Compile_env.bind_internal_func_symbol name_prefix md acc |> snd
  ) (ir_module.funcs) env

let compile_functions env (ir_mod: Stackless.module_) =
  let open Util.Maps in
  Int32Map.bindings ir_mod.funcs
  |> List.map (fun (_, (func, md)) ->
      Cfunction (compile_function func md env))

let init_function module_name env (ir_mod: Stackless.module_) data_info =
  let name_prefix = (Util.Names.sanitise module_name) ^ "_" in
  let memory_body =
    match ir_mod.memory_metadata with
      | Some (MemoryType lims) ->
        (* Initialise struct representing this module's memory *)
        let root_ident = Ident.create "data_root" in
        let root =
          Cop (Cload (Word_int, Mutable),
            [Cconst_symbol (Compile_env.memory_symbol env)], nodbg) in

        (* Generate memcpy calls to initialise memory *)
        let init_data =
          let memcpy (symb, offset, size) =
            let addr = Cop (Caddi,
              [Cvar root_ident;
               Cconst_natint (Int64.to_nativeint offset)], nodbg) in
            Cop (Cextcall ("memcpy", typ_void, false, None),
              [addr; Cconst_symbol symb; Cconst_int size], nodbg) in

          let rec call_seq calls =
            match calls with
              | [] -> Ctuple []
              | x :: xs -> Csequence (x, call_seq xs) in
          call_seq (List.map (memcpy) data_info) in

        (* Set up memory page limits *)
        let max_addressable_pages = 65535 in
        let min_pages = Cconst_natint (Nativeint.of_int32 lims.min) in
        let max_pages =
          begin
            match lims.max with
              | Some x -> Cconst_natint (Nativeint.of_int32 x)
              | None -> Cconst_natint (Nativeint.of_int max_addressable_pages)
          end in
        let memory_symbol = Cconst_symbol (Compile_env.memory_symbol env) in
        (* Perform allocation, then initialise data *)
          Csequence (
            Cop (
              Cextcall ("wasm_rt_allocate_memory", typ_void, false, None),
              [memory_symbol; min_pages; max_pages],
              nodbg
            ),
            Clet (root_ident, root, init_data))
      | None -> Ctuple [] in

  let start_body =
    match ir_mod.start with
      | Some md ->
          let symbol_name =
            Compile_env.lookup_func_symbol md env
            |> Symbol.name in
          let fn_symbol = Cconst_symbol symbol_name in
          Cop (Capply typ_void, [fn_symbol], nodbg)
      | _ -> Ctuple [] in

  let body = Csequence (memory_body, start_body) in
  Cfunction {
    fun_name = name_prefix ^ "init";
    fun_args = [];
    fun_body = body;
    fun_codegen_options = [No_CSE];
    fun_dbg = nodbg
  }

(* Returns a Cmm.phrase list, and a (symbol, offset, size) list.
 * The (symbol, offset, size) list is a list of data symbols along with
 * their respective sizes. *)
let module_data name (ir_mod: Stackless.module_) =
  let name_prefix = (Util.Names.sanitise name) ^ "_Data_" in

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


(* TODO: Handle exported globals *)
let module_globals env (ir_mod: Stackless.module_) =
  let open Libwasm.Values in
  let open Util.Maps in
  let global_bindings = Int32Map.bindings ir_mod.globals in

  let compile_data_value = function
    | I32 x -> Cint32 (Nativeint.of_int32 x)
    | I64 x -> Cint (Int64.to_nativeint x)
    | F32 x -> Cint32 (Libwasm.F32.to_bits x |> Nativeint.of_int32)
    | F64 x -> Cint (Libwasm.F64.to_bits x |> Int64.to_nativeint) in

  List.map (fun (_, g) ->
    let dat = compile_data_value (Global.initial_value g) in
    let symb_name = Compile_env.global_symbol env g in
    [Cdefine_symbol symb_name; dat]) global_bindings
  |> List.concat


(* IR function to CMM phrase list *)
let compile_module name (ir_mod: Stackless.module_) =
  let sanitised_name = (Util.Names.sanitise name) in
  let env =
    populate_symbols name (Compile_env.empty sanitised_name) ir_mod in
  let memory_symbol_name = Compile_env.memory_symbol env in
  let (data, data_info) = module_data name ir_mod in
  let init = init_function name env ir_mod data_info in
  let global_data = module_globals env ir_mod in
  (* Memory symbol: needs 3 words of space to store struct created by RTS *)
  let data =
    Cdata ([Cdefine_symbol memory_symbol_name; Cskip 24] @ data @ global_data) in
  let funcs = compile_functions env ir_mod @ [init] in
  funcs @ [data]

