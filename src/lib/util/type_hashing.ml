open Libwasm.Types
exception Unhashable

let hash_type = function
  | I32Type -> Nativeint.of_int 1
  | I64Type -> Nativeint.of_int 2
  | F32Type -> Nativeint.of_int 3
  | F64Type -> Nativeint.of_int 4

let hash_function_type fn_ty =
  let FuncType (arg_tys, ret_tys) = fn_ty in

  (* Each type requires 3 bits to store.
   * On a 64-bit machine, this results in (64 bits / 3) = 20 arguments and the result type  *)
  (* bit_size: number of bits required to losslessly hash a WASM type. *)
  let bit_size = 3 in
  let max_args = (Nativeint.size - bit_size) / bit_size in
  let () =
    if (List.length arg_tys > max_args) then raise Unhashable else () in

  (* Lowest 3 bits are result type *)
  let x =
    match ret_tys with
      | [] -> Nativeint.zero
      | [ret_ty] -> hash_type ret_ty
      | _ -> assert false in

  let rec go n x = function
    | [] -> x
    | ty :: tys ->
        let () = if (n > max_args) then assert false else () in
        let shift_size = bit_size * n in
        let x = Nativeint.logor x (Nativeint.shift_left (hash_type ty) shift_size) in
        go (n + 1) x tys in

  (* Starting at position 1 (i.e., << 3) since we need to account for recording
   * the return type *)
  let x = go 1 x arg_tys in
  (* Finally, we set the high bit to indicate that this slot in the function
   * table has been initialised, distinguishing the hash from that for
   * void -> void *)
  Nativeint.logor x (Nativeint.of_string "0x8000000000000000")


