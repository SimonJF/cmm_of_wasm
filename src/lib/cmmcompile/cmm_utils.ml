open Cmm

let is_value = function
  | Cconst_int _ | Cconst_natint _ | Cconst_float _ | Cconst_symbol _
  | Cconst_pointer _ | Cconst_natpointer _ | Cvar _ -> true
  | _ -> false

let is_int = function
  | Cconst_int _ | Cconst_natint _ -> true
  | _ -> false
