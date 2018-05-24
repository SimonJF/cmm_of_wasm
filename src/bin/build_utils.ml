(* Pierre Chambart's work, repurposed for cmm_of_wasm *)
open Libwasm

let gen_asm out name cmm =
  Clflags.dump_cmm := Command_line.dump_cmm ();
  Clflags.dump_linear := Command_line.dump_linear ();
  Emitaux.create_asm_file := true;
  Emitaux.output_channel := open_out out;
  Compilenv.reset
    ?packname:None name;
  Emit.begin_assembly ();
  List.iter (Asmgen.compile_phrase Format.std_formatter) cmm;
  Emit.end_assembly ();
  close_out !Emitaux.output_channel

let run_command command =
  if Command_line.verbose () then
    Printf.eprintf "+ %s\n%!" command;
  let ret = Sys.command command in
  if ret <> 0 then
    failwith (Printf.sprintf "Command failed: %s" command)

let call_compiler in_file out_file =
  let cc = Command_line.cc () in
  let command =
    Printf.sprintf "%s -c %s -o %s"
      cc in_file out_file
  in
  run_command command

let shared_file obj_files out_file =
  let cc = Command_line.cc () in
  let command =
    Printf.sprintf "%s -shared %s -o %s"
      cc (String.concat " " obj_files) out_file
  in
  run_command command

let build ~name ~out_dir cmm =
  let assembly_filename = Filename.temp_file name ".s" in
  let verbose = Command_line.verbose () in
  if Command_line.shared () then
    let obj_file = (Filename.chop_suffix assembly_filename ".s") ^ ".o" in
    let so_file = Filename.concat out_dir (name ^ ".so") in
    if verbose then begin
      Printf.eprintf "@ asm: %s\n%!" assembly_filename;
      Printf.eprintf "@ obj: %s\n%!" obj_file;
    end;
    gen_asm assembly_filename name cmm;
    call_compiler assembly_filename obj_file;
    shared_file [obj_file] so_file
  else
    let obj_file = Filename.concat out_dir (name ^ ".o") in
    if verbose then begin
      Printf.eprintf "@ asm: %s\n%!" assembly_filename;
    end;
    gen_asm assembly_filename name cmm;
    call_compiler assembly_filename obj_file


let parse_file kind name file =
  let ic = open_in file in
  try
    let lexbuf = Lexing.from_channel ic in
    let e = Parse.parse name lexbuf kind in
    close_in ic;
    e
  with e ->
    close_in ic;
    raise e

let parse_module name file =
  let var, script = parse_file Parse.Module name file in
  let rec depack def =
    match def.Source.it with
    | Script.Textual m -> m
    | Script.Encoded (name, bs) ->
      Decode.decode name bs
    | Script.Quoted (_, s) ->
      let def' = Parse.string_to_module s in
      depack def'
  in
  let modul_ = depack script in
  let () =
    try Valid.check_module modul_
    with (Valid.Invalid (at, msg)) as e ->
      Format.eprintf "%s : %s : %s@."
        (Source.string_of_region at)
        "invalid module"
        msg;
      raise e
  in
  var, modul_
(*
let get_start (modul: Ast.module_) =
  match modul.start with
  | None -> failwith "No start"
  | Some start ->
    let _, func =
      List.find (fun (id, _) -> id = start) modul.functions
    in
    func

let get_func (modul:module_) name =
  let _, func =
    List.find (fun (id, _) -> Func_id.is_named id name) modul.functions
  in
  func
  *)
