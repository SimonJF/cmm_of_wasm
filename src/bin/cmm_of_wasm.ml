open Libwasm.Ast
open Ir.Stackless
open Cmm
open C_stubs

(* Entry point of the compiler *)
let trace s =
  if Command_line.verbose () then print_endline s else ()

let print = print_endline

let load_wasm_script filename =
  trace ("Loading (" ^ filename ^ ")...");
  let ic = open_in filename in
  try
    let lexbuf = Lexing.from_channel ic in
    trace "Parsing...";
    let script = Libwasm.(Parse.parse filename lexbuf Parse.Script) in
    close_in ic;
    script
  with | e -> close_in ic; raise e

let rec collect_modules commands =
  (* UGH. *)
  List.fold_left (fun acc (x: Libwasm.Script.command) -> 
    match x.it with
      | Module (name_opt, def) ->
          begin
            match def.it with
              | Textual module_ -> (name_opt, module_) :: acc
              | _ -> acc
          end
      | _ -> acc) [] commands |> List.rev

let print_module ast_mod =
  if Command_line.dump_wasm () then
    Libwasm.Print.module_ stdout !(Libwasm.Flags.width) ast_mod

let dump_cmm cmm_phrases =
  if Command_line.dump_cmm () then
    begin
      print "CMM Phrases: ";
      List.iter (Printcmm.phrase Format.std_formatter) cmm_phrases
    end

let dump_stackless ir =
  if Command_line.dump_stackless () then
    begin
      print "IR Module: ";
      print (Ir.Print_stackless.string_of_module ir)
    end

let generate_c_stubs ir =
  let open Util.Maps in
  let funcs = Int32Map.bindings ir.funcs |> List.map (fun (_, (_, x)) -> x) in
  let module_name =
    Filename.remove_extension (Command_line.output_filename ()) in
  let c_funcs = C_stubs.cfuncs_of_funcs funcs in
  let header = C_stubs.header ~module_name ~c_funcs in
  let stub_file = C_stubs.stub_file ~module_name ~c_funcs in
  Printf.printf "Header: %s\n" header;
  Printf.printf "Stub file: %s\n" stub_file


let compile_module filename (_name_opt, module_) =
  print_module module_;
  (* Validate the module *)
  Libwasm.Valid.check_module module_;
  (* Generate stackless IR representation *)
  let ir = Ir.Genstackless.ir_module module_ in
  dump_stackless ir;
  (* Compile to CMM *)
  let cmm_phrases = Cmmcompile.Gencmm.compile_module ir in
  dump_cmm cmm_phrases;
  (* Generate C stubs *)
  generate_c_stubs ir;
  (* Compile to ASM *)
  let out_dir = Filename.dirname filename in
  let name = Filename.basename filename in
  Build_utils.build ~name ~out_dir cmm_phrases

let compile_modules output_filename = function
    | [] -> ()
    | [x] -> compile_module output_filename x
    | xs ->
      List.iteri (fun i -> compile_module (output_filename ^ (string_of_int i)) ) xs

let frontend filename =
  load_wasm_script filename
  |> collect_modules
  |> compile_modules 
      (Filename.remove_extension 
        (Command_line.output_filename ())) 

let () =
  Command_line.setup ();
  let filename = Command_line.filename () in
  frontend filename

