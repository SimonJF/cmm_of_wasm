(* Generates C header files and stubs for a compiled module *)
open Ir

type c_type = | U32 | U64 | F32 | F64 | Void

type cfunc = {
  external_name : string; (* External symbol name *)
  internal_name : string; (* Internally-compiled symbol name *)
  args : (string * c_type) list;
  ret_ty : c_type
}

let ctype_of_wasm_type =
  let open Libwasm.Types in
  function
  | I32Type -> U32
  | I64Type -> U64
  | F32Type -> F32
  | F64Type -> F64

let string_of_ctype = function
  | U32 -> "u32"
  | U64 -> "u64"
  | F32 -> "f32"
  | F64 -> "f64"
  | Void -> "void"

let signature func =
  let args =
    List.map (fun (n, ty) -> 
      Printf.sprintf "%s %s" (string_of_ctype ty) n) func.args
    |> String.concat ", " in
  Printf.sprintf "%s %s(%s)"
    (string_of_ctype func.ret_ty) func.external_name args

let cfunc_of_func' module_name func =
  let arg_name i = function
    | U32 | U64 -> "i" ^ (string_of_int i)
    | F32 | F64 -> "f" ^ (string_of_int i)
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
        (* First, need to sanitise the module name and method name *)
        let name_prefix = (Util.Names.sanitise module_name) ^ "_" in
        let name = name_prefix ^ name in
        [{ external_name = name_prefix ^ name; 
           internal_name = Util.Names.internal_name name;
           args = c_args; ret_ty = ret }]


let cfunc_of_func ~module_name func =
  match cfunc_of_func' module_name func with
    | [] -> None
    | [x] -> Some x
    | _ -> assert false

let cfuncs_of_funcs ~module_name funcs =
  List.concat (List.map (cfunc_of_func' module_name) funcs)

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
    | (name, U32) :: xs | (name, U64) :: xs ->
        let reg = integer_register i_count in
        (name, reg) :: assign_registers (i_count + 1) f_count xs
    | (name, F32) :: xs | (name, F64) :: xs ->
        let reg = float_register f_count in
        (name, reg) :: assign_registers i_count (f_count + 1) xs
    | (_, Void) :: _ -> assert false in
  let register_map = assign_registers 0 0 func.args in
  let formatted_register_map =
    List.map (fun (name, reg) ->
      Printf.sprintf "\"%s\" (%s)" reg name) register_map
    |> String.concat ", " 
  in

    let result_decl = 
    if func.ret_ty = Void then
      ""
    else
      Printf.sprintf "  %s result;\n" (string_of_ctype func.ret_ty) in

  let result_return =
    if func.ret_ty = Void then
      "return;\n"
    else
      "return result;\n" in

  let result_assignment = 
    if func.ret_ty = Void then
      ""
    else
      let result_register =
        match func.ret_ty with
          | U32 | U64 -> "\"=a\""
          | F32 | F64 -> "\"=v0\""
          | Void -> assert false in
      Printf.sprintf "%s (result)" result_register in


  signature func ^ " {\n" ^
    result_decl ^
    (Printf.sprintf "  asm volatile(\"call %s\" : %s : "
      (func.internal_name) result_assignment) ^
    formatted_register_map ^ " : \"memory\", \"cc\");\n" ^
    result_return ^ "}"

let header ~module_name ~c_funcs =
  let header_prefix =
    let header_prefix_path = Util.Command_line.header_prefix_path () in
    Util.Files.read_text_file header_prefix_path in

  (* TODO: This is limited to functions for now; we'll need to extend it for
   * arbitrary globals later *)
  let header_exports =
    List.map (fun func ->
      Printf.sprintf "%s;" (signature func)) c_funcs
    |> String.concat "\n" in

  let header_name =
    Printf.sprintf "__CMMOFWASM_%s_H" (String.uppercase_ascii module_name) in
  let rts_basename = Filename.basename (Util.Command_line.rts_header ()) in

  "#ifndef " ^ header_name ^ "\n" ^
  "#define " ^ header_name ^ "\n" ^
  "#include \"" ^ rts_basename ^ "\"\n" ^
  header_prefix ^ "\n" ^
  header_exports ^ "\n" ^
  "#endif"

let stub_file ~header_filename ~c_funcs =
  let func_stubs = List.map generate_cstub c_funcs |> String.concat "\n\n" in
  Printf.sprintf "#include \"%s\"\n\n%s\n" header_filename func_stubs
  
