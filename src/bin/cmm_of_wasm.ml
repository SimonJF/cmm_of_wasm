open Libwasm.Ast
open Ir.Stackless
open Cmm
open C_stubs
open Util

(* Entry point of the compiler *)
let print = print_endline

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

let compile_module output_file (_name_opt, module_) =
  print_module module_;
  (* Validate the module *)
  Libwasm.Valid.check_module module_;
  (* Generate stackless IR representation *)
  let ir = Ir.Genstackless.ir_module module_ in
  dump_stackless ir;
  (* Compile to CMM *)
  let cmm_phrases = Cmmcompile.Gencmm.compile_module ir in
  dump_cmm cmm_phrases;
  (* Compile to ASM *)
  let output_dir = Filename.dirname output_file in
  let output_base = Filename.basename output_file in
  Build_utils.build ~name:output_base ~out_dir:output_dir ~ir ~cmm:cmm_phrases

let frontend filename =
  let name = Filename.basename filename in
  Build_utils.parse_file name filename
  |> compile_module
      (Filename.remove_extension 
        (Command_line.output_filename ())) 

let () =
  Command_line.setup ();
  let filename = Command_line.input_filename () in
  frontend filename

