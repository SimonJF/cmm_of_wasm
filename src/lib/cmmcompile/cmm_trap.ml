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

let trap reason =
  Cop (Craise Raise_notrace, [Cconst_int (trap_id reason)], nodbg)

(* We require a toplevel return type annotation to ensure registers
 * match up. *)
let with_toplevel_handler toplevel_type expr =
  let reason_ident = Ident.create "reason" in
  let handler =
    Cop (Cextcall ("wasm_rt_trap", toplevel_type, false, None),
      [Cvar reason_ident], nodbg) in
  Ctrywith (expr, reason_ident, handler)

