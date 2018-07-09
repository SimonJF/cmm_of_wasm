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

let compile_module ~output_file ~prefix module_ =
  print_module module_;
  (* Validate the module *)
  Libwasm.Valid.check_module module_;
  (* Generate stackless IR representation *)
  let ir = Ir.Genstackless.ir_module module_ in
  dump_stackless ir;
  (* Compile to CMM *)
  let cmm_phrases = Cmmcompile.Gencmm.compile_module prefix ir in
  dump_cmm cmm_phrases;
  (* Compile to ASM *)
  let output_dir = Filename.dirname output_file in
  let output_base = Filename.basename output_file in
  Build_utils.build
    ~output_name:output_base
    ~prefix
    ~out_dir:output_dir
    ~ir
    ~cmm:cmm_phrases

let frontend filename =
  let (_, parsed_module) = Build_utils.parse_file filename in
  let output_file = Command_line.output_filename () in
  let prefix =
    begin
      match Command_line.prefix () with
        | Some prefix -> prefix
        | None -> output_file
      end
    |> Util.Names.sanitise in
  compile_module
    ~output_file
    ~prefix
    parsed_module

let () =
  Command_line.setup ();
  let filename = Command_line.input_filename () in
  frontend filename

