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

let trap reason =
  Cop (Cextcall ("wasm_rt_trap", typ_int, false, None),
    [Cconst_int (trap_id reason)], nodbg)

