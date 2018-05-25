(* Generates C header files and stubs for a compiled module *)
open Ir

type c_type = | Uint32 | Uint64 | Float | Double | Void

type cfunc = {
  external_name : string; (* External symbol name *)
  internal_name : string; (* Internally-compiled symbol name *)
  args : (string * c_type) list;
  ret_ty : c_type
}

let ctype_of_wasm_type =
  let open Libwasm.Types in
  function
  | I32Type -> Uint32
  | I64Type -> Uint64
  | F32Type -> Float
  | F64Type -> Double

let string_of_ctype = function
  | Uint32 -> "uint32_t"
  | Uint64 -> "uint64_t"
  | Float -> "float"
  | Double -> "double"
  | Void -> "void"

let signature func =
  let args =
    List.map (fun (n, ty) -> 
      Printf.sprintf "%s %s" (string_of_ctype ty) n) func.args
    |> String.concat ", " in
  Printf.sprintf "%s %s(%s)"
    (string_of_ctype func.ret_ty) func.external_name args

let cfunc_of_func' func =
  let arg_name i = function
    | Uint32 | Uint64 -> "i" ^ (string_of_int i)
    | Float | Double -> "f" ^ (string_of_int i)
    | Void -> assert false in
  match Func.name func with
    | None -> []
    | Some name -> 
        let open Libwasm.Types in
        let FuncType (args, rets) = Func.type_ func in
        (* Still only supporting 0 or 1-return functions *)
        let ret =
          begin
            match rets with
              | [] -> Void
              | [ty] -> ctype_of_wasm_type ty
              | _ -> assert false
          end in
        let c_args =
          List.mapi (fun i ty ->
            let cty = ctype_of_wasm_type ty in
            let name = arg_name i cty in
            (name, cty)) args in
        [{ external_name = name; 
           internal_name = Util.Names.internal_name name;
           args = c_args; ret_ty = ret }]


let cfunc_of_func func =
  match cfunc_of_func' func with
    | [] -> None
    | [x] -> Some x
    | _ -> assert false

let cfuncs_of_funcs funcs = List.concat (List.map cfunc_of_func' funcs)

let integer_register i =
  (* OCaml calling conventions, you be crazy.. *)
  let registers =
    ["a"; "b"; "D"; "S"; "d"; "c"; "q8"; "q9";
     "q12"; "q13"; "q10"; "q11"; "Rbp"] in
  if i > List.length registers - 1 then 
    failwith
      ("Error: Cannot generate C stubs for functions " ^
       "with greater than 13 integer arguments")
  else
    List.nth registers i

let float_register i =
  if i > 16 then
    failwith
      ("Error: cannot generate C stubs for functions " ^
       "with greater than 16 float arguments")
  else
    "v" ^ (string_of_int i)

let generate_cstub func =
  (* C stub must:
    * 1) Define variable to hold result, of result type
    * 2) Generate volatile ASM stub:
      *  a) If result is an integer, it goes into a
      *  b) If result is a float, it goes into %xmm0 = v0
      *  c) Each integer argument goes into abc...
      *  d) Each float goes into xmm...
    * 3) C return of result type *)
  let rec assign_registers i_count f_count = function
    | [] -> []
    | (name, Uint32) :: xs | (name, Uint64) :: xs ->
        let reg = integer_register i_count in
        (name, reg) :: assign_registers (i_count + 1) f_count xs
    | (name, Float) :: xs | (name, Double) :: xs ->
        let reg = float_register f_count in
        (name, reg) :: assign_registers i_count (f_count + 1) xs
    | (_, Void) :: _ -> assert false in
  let register_map = assign_registers 0 0 func.args in
  let formatted_register_map =
    List.map (fun (name, reg) ->
      Printf.sprintf "\"%s\" (%s)" reg name) register_map
    |> String.concat ", " 
  in
  let result_register =
    match func.ret_ty with
      | Uint32 | Uint64 -> "\"=a\""
      | Float | Double -> "\"=v0\""
      | Void -> assert false in
  signature func ^ " {\n" ^
    (Printf.sprintf "  %s result;\n" (string_of_ctype func.ret_ty)) ^
    (Printf.sprintf "  asm volatile(\"call %s\" : %s (result) : "
      (func.internal_name) result_register) ^
    formatted_register_map ^ " : \"memory\", \"cc\");\n" ^
    "  return result;\n}"

let header ~module_name ~c_funcs =
  (* TODO: This is limited to functions for now; we'll need to extend it for
   * arbitrary globals later *)
  let header_exports =
    List.map (fun func ->
      Printf.sprintf "%s;" (signature func)) c_funcs
    |> String.concat "\n" in

  let header_name =
    Printf.sprintf "__CMMOFWASM_%s_H" (String.uppercase_ascii module_name) in

  Printf.sprintf "#ifndef %s\n#define %s\n#include %s\n%s\n#endif"
    header_name header_name "<stdint.h>" header_exports

let stub_file ~header_filename ~c_funcs =
  let func_stubs = List.map generate_cstub c_funcs |> String.concat "\n\n" in
  Printf.sprintf "#include %s\n#include \"%s\"\n\n%s\n"
    "<stdint.h>" header_filename  func_stubs
  
