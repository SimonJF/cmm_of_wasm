(* Generates C header files and stubs for a compiled module *)
open Ir

type c_type = | U32 | U64 | F32 | F64 | Void

type c_func = {
  name : string; (* Export name *)
  args : (string * c_type) list;
  ret_ty : c_type;
  ir_func : Func.t (* Ir function metadata *)
}

type c_export =
  | Cfunc of c_func
  | Cglobal of { name: string; type_: c_type }
  | Cmemory of { name: string }
  | Ctable of { name: string }


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
    (string_of_ctype func.ret_ty) func.name args

let export_func ~function_name func =
  let arg_name i = function
    | U32 | U64 -> "i" ^ (string_of_int i)
    | F32 | F64 -> "f" ^ (string_of_int i)
    | Void -> assert false in
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
    Cfunc { name = function_name; args = c_args;
      ret_ty = ret; ir_func = func }

let c_exports ~module_name (ir_mod: Ir.Stackless.module_) : c_export list  =
  let open Util.Maps in
  List.map (fun (x: Libwasm.Ast.export) ->
    let x = x.it in
    let export_name =
      Util.Names.(name_to_string x.name |> sanitise) in

    match x.edesc.it with
      | FuncExport v ->
          let (_func, md) = Int32Map.find v.it ir_mod.funcs in
          let name = Printf.sprintf "%s_cfunc_%s" module_name export_name in
          export_func ~function_name:name md
      | TableExport _ ->
          let name = Printf.sprintf "%s_table_%s" module_name export_name in
          Ctable { name }
      | MemoryExport _ ->
          let name = Printf.sprintf "%s_memory_%s" module_name export_name in
          Cmemory { name }
      | GlobalExport v ->
          let g = Int32Map.find v.it ir_mod.globals in
          let cty = ctype_of_wasm_type (Global.type_ g) in
          let name = Printf.sprintf "%s_global_%s" module_name export_name in
          Cglobal { name; type_ = cty }
  ) ir_mod.exports

let int_registers =
  (* OCaml calling conventions, you be crazy.. *)
    ["rax"; "rbx"; "rdi"; "rsi"; "rdx"; "rcx"; "r8"; "r9";
     "r12"; "r13"; "r10"; "r11"; "rbp"]

let max_float_registers = 16

let float_register i =
  if i > max_float_registers then
    failwith
      ("Error: cannot generate C stubs for functions " ^
       "with greater than 15 float arguments")
  else
    "xmm" ^ (string_of_int i)

let generate_cstub module_name export =
  let generate_func_stub func =
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
    (* Being slightly cheeky with mutability here -- it means we
     * can still be tail-recursive and not have to do a state-passing
     * transformation, but we can still get the unassigned int registers
     * and float registers to mark them as clobbered *)
    let unassigned_int_registers = ref [] in
    let final_float_count = ref (-1) in
    let rec assign_registers int_registers float_count = function
      | [] ->
          unassigned_int_registers := int_registers;
          final_float_count := float_count;
          []
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

    let clobbered_registers =
      let result_register =
        match func.ret_ty with
          | U32 | U64 -> "rax" | F32 | F64 -> "xmm0" | _ -> "" in
      (* RBP has to be handled specially. Additionally, the result register
       * shouldn't appear in here either. *)
      let include_in_clobber x =
        if x = "rbp" || x = result_register then false else true in
      let rec clobbered_float_registers i =
        if i >= max_float_registers then []
        else ("xmm" ^ (string_of_int i)) :: (clobbered_float_registers (i + 1)) in
      !unassigned_int_registers @
      (clobbered_float_registers !final_float_count) @ ["memory"; "cc"]
      |> List.map (fun x -> if include_in_clobber x then [Printf.sprintf "\"%s\"" x] else [])
      |> List.concat
      |> String.concat "," in


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
      if func.ret_ty = Void then "  return;\n"
      else "  return result;\n" in

    let result_assignment =
      if func.ret_ty = Void then ""
      else
        let result_register =
          match func.ret_ty with
            | U32 | U64 -> "\"=a\""
            | F32 | F64 -> "\"=x\""
            | Void -> assert false in
        Printf.sprintf "%s (result_asm)" result_register in

    let push_registers =
      "  asm (\"push %%rbp\\n\\t\" \"push %%rsi\" : );\n" in

    let internal_name = Func.symbol module_name func.ir_func in

    signature func ^ " {\n" ^
      defined_registers ^
      result_decl ^
      push_registers ^
      (Printf.sprintf "  asm volatile(\"call %s\\n\\t\" \"pop %s\\n\\t\" \"pop %s\" : %s : "
        internal_name "%%rsi" "%%rbp" result_assignment) ^
      formatted_register_map ^ " : " ^
      clobbered_registers ^ ");\n" ^
      stable_result ^
      result_return ^ "}" in

  match export with
    | Cfunc func -> [generate_func_stub func]
    | _ -> []

let header ~module_name ~exports =
  let header_prefix =
    let header_prefix_path = Util.Command_line.header_prefix_path () in
    Util.Files.read_text_file header_prefix_path in

  let header_exports =
    List.map (fun exp ->
      match exp with
        | Cfunc func -> Printf.sprintf "%s;" (signature func)
        | Ctable { name } ->
            Printf.sprintf "wasm_rt_table_t* %s;" name
        | Cmemory { name } -> Printf.sprintf "wasm_rt_memory_t* %s;" name
        | Cglobal { name; type_ } ->
            Printf.sprintf "%s* %s;" name (string_of_ctype type_)
    ) exports
    |> String.concat "\n" in

  let header_name =
    Printf.sprintf "__CMMOFWASM_%s_H" (String.uppercase_ascii module_name) in
  let rts_basename = Filename.basename (Util.Command_line.rts_header ()) in

  "#ifndef " ^ header_name ^ "\n" ^
  "#define " ^ header_name ^ "\n" ^
  "#include <stdio.h>" ^ "\n" ^
  "#include \"" ^ rts_basename ^ "\"\n" ^
  header_prefix ^ "\n" ^
  header_exports ^ "\n" ^
  "#endif"

let stub_file ~header_filename ~module_name ~exports =
  let func_stubs =
    List.map (generate_cstub module_name) exports
      |> List.concat
      |> String.concat "\n\n" in
  Printf.sprintf "#include \"%s\"\n\n%s\n" header_filename func_stubs

