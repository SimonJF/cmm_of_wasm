(* Generates C header files and stubs for a compiled module *)
open Ir
open Util.Maps

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

let ctype_of_function_type fty =
  let open Libwasm.Types in
  let (FuncType (args, rets)) = fty in
  let ret =
    begin
      match rets with
        | [] -> Void
        | [ty] -> ctype_of_wasm_type ty
        | _ -> assert false
    end in
  let c_args = List.map (ctype_of_wasm_type) args in
  (c_args, ret)

let c_function_pointer func_md =
  let str = string_of_ctype in
  let (arg_tys, ret) = ctype_of_function_type (Func.type_ func_md) in
  let arg_ty_str =
    List.map (str) arg_tys
    |> String.concat ", " in
  (* Perhaps this is goofy API design. We know the module_name will never
   * be used since the function must be imported, yet still have to supply
   * it. Could we represent this better? *)
  let name = Func.symbol ~module_name:"" func_md in
  Printf.sprintf "%s(*%s)(%s)" (str ret) name arg_ty_str

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
  let (arg_tys, ret) = ctype_of_function_type (Func.type_ func) in
  let c_args =
    List.mapi (fun i cty ->
      let name = arg_name i cty in
      (name, cty)) arg_tys in
  Cfunc { name = function_name; args = c_args;
    ret_ty = ret; ir_func = func }

let c_exports ~prefix (ir_mod: Ir.Stackless.module_) : c_export list  =
  List.map (fun (x: Libwasm.Ast.export) ->
    let x = x.it in
    let export_name =
      Util.Names.(name_to_string x.name |> sanitise) in

    match x.edesc.it with
      | FuncExport v ->
          let md = Int32Map.find v.it ir_mod.function_metadata in
          let name = Printf.sprintf "%s_cfunc_%s" prefix export_name in
          export_func ~function_name:name md
      | TableExport _ ->
          let name = Printf.sprintf "%s_table_%s" prefix export_name in
          Ctable { name }
      | MemoryExport _ ->
          let name = Printf.sprintf "%s_memory_%s" prefix export_name in
          Cmemory { name }
      | GlobalExport v ->
          let g = Int32Map.find v.it ir_mod.globals in
          let cty = ctype_of_wasm_type (Global.type_ g) in
          let name = Printf.sprintf "%s_global_%s" prefix export_name in
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

let float_registers =
  Array.init max_float_registers
    (fun x -> "xmm" ^ (string_of_int x)) |> Array.to_list

let generate_cstub prefix export =
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

    let internal_name = Func.symbol prefix func.ir_func in

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

let header ~prefix ~exports ~(ir_mod: Stackless.module_) =
  let header_prefix =
    let header_prefix_path = Util.Command_line.header_prefix_path () in
    Util.Files.read_text_file header_prefix_path in

  let header_imports =
    let imported_functions =
      let function_extern (_, f) =
        if Func.is_imported f then
          let ptr = c_function_pointer f in
          [Printf.sprintf "extern %s;" ptr]
        else [] in
      List.map (function_extern) (Int32Map.bindings ir_mod.function_metadata)
      |> List.concat in
    let imported_globals =
      let global_extern (_, g) =
        if Global.is_imported g then
          let g_symbol = Global.symbol ~module_name:"" g in
          let c_ty = ctype_of_wasm_type (Global.type_ g) |> string_of_ctype in
          [Printf.sprintf "extern %s* %s;" c_ty g_symbol]
        else [] in
      List.map (global_extern) (Int32Map.bindings ir_mod.globals)
      |> List.concat in
    let imported_memory =
      match ir_mod.memory_metadata with
        | Some (ImportedMemory { module_name; memory_name }) ->
            [Printf.sprintf
              "extern wasm_rt_memory_t* %s_memory_%s;" module_name memory_name]
        | _ -> [] in
    let imported_table =
      match ir_mod.table with
        | Some (ImportedTable { module_name; table_name }) ->
            [Printf.sprintf
              "extern wasm_rt_table_t* %s_table_%s;" module_name table_name]
        | _ -> [] in
    [imported_functions;
     imported_globals;
     imported_memory;
     imported_table]
    |> List.concat
    |> String.concat "\n" in

  let header_exports =
    List.map (fun exp ->
      match exp with
        | Cfunc func -> Printf.sprintf "%s;" (signature func)
        | Ctable { name } ->
            Printf.sprintf "wasm_rt_table_t* %s;" name
        | Cmemory { name } -> Printf.sprintf "wasm_rt_memory_t* %s;" name
        | Cglobal { name; type_ } ->
            Printf.sprintf "%s* %s;" (string_of_ctype type_) name
    ) exports
    |> String.concat "\n" in

  let header_name =
    Printf.sprintf "__CMMOFWASM_%s_H" (String.uppercase_ascii prefix) in
  let rts_basename = Filename.basename (Util.Command_line.rts_header ()) in

  "#ifndef " ^ header_name ^ "\n" ^
  "#define " ^ header_name ^ "\n" ^
  "#include <stdio.h>" ^ "\n" ^
  "#include \"" ^ rts_basename ^ "\"\n" ^
  header_prefix ^ "\n" ^
  (* header_imports ^ "\n" ^ -- These are unneeded, as they are declared in other headers... *)
  header_exports ^ "\n" ^
  "#endif"

let stub_file ~header_filename ~prefix ~exports =
  let init_func_stub =
    let signature = Printf.sprintf "void %s_init()" prefix in
    let all_registers = int_registers @ float_registers in
    let clobbered_registers =
      List.filter (fun x -> x <> "rsi" && x <> "rbp") all_registers
      |> List.map (Printf.sprintf "\"%s\"") in
    let clobbered_registers = ["\"memory\""; "\"cc\""] @ clobbered_registers in
    let clobbered_registers_str = String.concat ", " clobbered_registers in

    let push_registers =
      "  asm (\"push %%rbp\\n\\t\" \"push %%rsi\" : );" in

    let asm_call =
      (Printf.sprintf "  asm volatile(\"call %s_initinternal\\n\\t\" \"pop %s\\n\\t\" \"pop %s\" : : : %s );"
      prefix "%%rsi" "%%rbp" clobbered_registers_str) in

    signature ^ " {" ^ "\n" ^
    push_registers ^ "\n" ^
    asm_call ^ "\n" ^
    "}" in

  let func_stubs =
    let stubs =
      init_func_stub :: (List.map (generate_cstub prefix) exports |> List.concat) in
    String.concat "\n\n" stubs in
  Printf.sprintf "#include \"%s\"\n\n%s\n" header_filename func_stubs

