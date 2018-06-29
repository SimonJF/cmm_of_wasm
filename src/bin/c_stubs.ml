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
        [{ external_name = name;
           internal_name = Util.Names.internal_name name;
           args = c_args; ret_ty = ret }]


let cfunc_of_func ~module_name func =
  match cfunc_of_func' module_name func with
    | [] -> None
    | [x] -> Some x
    | _ -> assert false

let cfuncs_of_funcs ~module_name funcs =
  List.concat (List.map (cfunc_of_func' module_name) funcs)

let int_registers =
  (* OCaml calling conventions, you be crazy.. *)
    ["rax"; "rbx"; "rdi"; "rsi"; "rdx"; "rcx"; "r8"; "r9";
     "r12"; "r13"; "r10"; "r11"; "rbp"]

let float_register i =
  if i > 14 then
    failwith
      ("Error: cannot generate C stubs for functions " ^
       "with greater than 15 float arguments")
  else
    "xmm" ^ (string_of_int i)

let generate_cstub func =
  (* Helpers *)
  let try_get_register = function
    | [] -> failwith @@
        "Exhausted registers: we only support " ^
        "stub generation for functions with up to 13 integer and 16 float arguments."
    | x :: xs -> (x, xs) in
  let is_int_type = function
    | U32 | U64 -> true | _ -> false in
  let is_float_type = function
    | F32 | F64 -> true | _ -> false in

  (* C stub must:
    * 1) Define variable to hold result, of result type
    * 2) Generate volatile ASM stub:
      *  a) If result is an integer, it goes into a
      *  b) If result is a float, it goes into Yz
      *  c) Each integer argument goes into abc...
      *  d) Each float goes into xmm...
    * 3) C return of result type *)
  let rec assign_registers int_registers float_count = function
    | [] -> []
    | (name, ty) :: xs when is_int_type ty ->
        let (reg, rest) = try_get_register int_registers in
        (name, ty, reg) :: assign_registers rest float_count xs
    | (name, ty) :: xs when is_float_type ty ->
        let reg = float_register float_count in
        (name, ty, reg) :: assign_registers int_registers (float_count + 1) xs
    | _ :: _ -> assert false in

  let fuel_name = "__cmmWasmFuel" in

  let register_map =
    let args = func.args @ [(fuel_name, U32)] in
    assign_registers int_registers 0 args in

  let defined_registers =
    let define_register (name, ty, reg) =
      (* Assign to given parameter, unless it's the fuel parameter,
       * in which case assign the initial fuel value *)
      let assignment =
        if name = fuel_name then
          string_of_int @@ Util.Command_line.initial_fuel ()
        else
          name in
      Printf.sprintf "  register %s r%s asm (\"%s\") = %s;\n"
        (string_of_ctype ty) name reg assignment in
    List.map define_register register_map
    |> String.concat "" in

  let formatted_register_map =
    List.map (fun (name, ty, _reg) ->
      let reg_constraint = if is_int_type ty then "q" else "x" in
      Printf.sprintf "\"%s\" (r%s)" reg_constraint name) register_map
    |> String.concat ", "
  in

    let result_decl =
    if func.ret_ty = Void then
      ""
    else
      let result_register =
        match func.ret_ty with
          | U32 | U64 -> "rax"
          | F32 | F64 -> "xmm0"
          | Void -> assert false in
      Printf.sprintf "  register %s result_asm asm(\"%s\");\n" (string_of_ctype func.ret_ty) result_register in

  let stable_result =
    if func.ret_ty = Void then "" else
      Printf.sprintf "  %s result = result_asm;\n" (string_of_ctype func.ret_ty) in

  let result_return =
    if func.ret_ty = Void then
      "  return;\n"
    else
      "  return result;\n" in

  let result_assignment =
    if func.ret_ty = Void then
      ""
    else
      let result_register =
        match func.ret_ty with
          | U32 | U64 -> "\"=a\""
          | F32 | F64 -> "\"=x\""
          | Void -> assert false in
      Printf.sprintf "%s (result_asm)" result_register in

  (*
   *rax         0
    rbx         1
    rdi         2
    rsi         3
    rdx         4
    rcx         5
    r8          6
    r9          7
    r12         8
    r13         9
    r10         10
    r11         11
    rbp         12
    r14         trap pointer
    r15         allocation pointer *)
  (* GCC moans about RBP being clobbered -- we need to handle it manually *)
  let clobbered_registers =
    ["rax"; "rbx"; "rdi"; "rsi"; "rdx"; "r8"; "r9"; "r12"; "r13"; "r10";
     "r11"; "r14"; "r15"
    ]
    |> List.map (Printf.sprintf "\"%s\"")
    |> String.concat ", " in

  let push_registers =
    "  asm (\"push %%rbp\\n\\t\" \"push %%rsi\" : );\n" in
  (*
  let pop_registers =
    "  asm volatile(\"pop rbp\n\t\" \"pop rsi\");\n" in
*)


  signature func ^ " {\n" ^
    defined_registers ^
    result_decl ^
    push_registers ^
    (Printf.sprintf "  asm volatile(\"call %s\\n\\t\" \"pop %s\\n\\t\" \"pop %s\" : %s : "
      (func.internal_name) "%%rsi" "%%rbp" result_assignment) ^
    formatted_register_map ^ " : \"memory\", \"cc\");\n" ^
    stable_result ^
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
    let sanitised = Util.Names.sanitise module_name in
    Printf.sprintf "__CMMOFWASM_%s_H" (String.uppercase_ascii sanitised) in
  let rts_basename = Filename.basename (Util.Command_line.rts_header ()) in

  "#ifndef " ^ header_name ^ "\n" ^
  "#define " ^ header_name ^ "\n" ^
  "#include <stdio.h>" ^ "\n" ^
  "#include \"" ^ rts_basename ^ "\"\n" ^
  header_prefix ^ "\n" ^
  header_exports ^ "\n" ^
  "#endif"

let stub_file ~header_filename ~c_funcs =
  let func_stubs = List.map generate_cstub c_funcs |> String.concat "\n\n" in
  Printf.sprintf "#include \"%s\"\n\n%s\n" header_filename func_stubs

