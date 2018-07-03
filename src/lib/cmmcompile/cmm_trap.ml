open Cmm

let nodbg = Debuginfo.none

type trap_reason =
  | TrapNone
  | TrapOOB
  | TrapIntOverflow
  | TrapDivZero
  | TrapInvalidConversion
  | TrapUnreachable
  | TrapCallIndirect
  | TrapExhaustion

(* Provides an index to be given to the C trap call, corresponding
 * to the `wasm_rt_trap_t` enumeration index *)
let trap_id = function
  | TrapNone -> 0
  | TrapOOB -> 1
  | TrapIntOverflow -> 2
  | TrapDivZero -> 3
  | TrapInvalidConversion -> 4
  | TrapUnreachable -> 5
  | TrapCallIndirect -> 6
  | TrapExhaustion -> 7

let trap_ty =
  let open Libwasm.Types in
  function
    | I32Type | I64Type -> typ_int
    | F32Type | F64Type -> typ_float

(* Traps don't return. But to successfully create join points
 * where one join ends in a trap (for example, memory bounds checks),
 * we need to ensure that the right register type is specified in the
 * call to trap. *)
let trap ty reason =
  Cop (Cextcall ("wasm_rt_trap", ty, false, None),
    [Cconst_int (trap_id reason)], nodbg)

let trap_int = trap typ_int
let trap_float = trap typ_int

let trap_function_ty tys reason =
  let trap_ty =
    match tys with
      | [] -> typ_void
      | [ty] -> trap_ty ty
      | _ -> assert false in
  trap trap_ty reason

