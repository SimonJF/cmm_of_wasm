(* Pierre Chambart's work, repurposed for cmm_of_wasm *)
open Libwasm
open Util
open Util.Trace

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

let call_compiler opts in_file out_file =
  let cc = Command_line.cc () in
  let opts = String.concat " " opts in
  let command =
    Printf.sprintf "%s %s %s -o %s"
      cc opts in_file out_file
  in
  run_command command

let shared_file obj_files out_file =
  let cc = Command_line.cc () in
  let command =
    Printf.sprintf "%s -shared %s -o %s"
      cc (String.concat " " obj_files) out_file
  in
  run_command command

let link_files obj_files out_file =
  let command =
    Printf.sprintf "ld -r %s -o %s"
      (String.concat " " obj_files) out_file in
  run_command command

let cleanup_temp_files files = List.iter (Sys.remove) files

let build_without_c ~output_name ~out_dir cmm =
  let assembly_filename = Filename.temp_file output_name ".s" in
  let verbose = Command_line.verbose () in
  let obj_file = Filename.concat out_dir (output_name ^ ".o") in
    if verbose then begin
      Printf.eprintf "@ asm: %s\n%!" assembly_filename;
    end;
  gen_asm assembly_filename obj_file cmm;
  call_compiler ["-c"] assembly_filename obj_file;
  cleanup_temp_files [assembly_filename]

let write_file (filename: string) (contents: string) =
  let oc = open_out filename in
  Printf.fprintf oc "%s\n" contents;
  close_out oc

let generate_c_stubs ~header_filename (ir:Ir.Stackless.module_) =
  let open Util.Maps in
  let funcs = Int32Map.bindings ir.funcs |> List.map (fun (_, (_, x)) -> x) in
  let module_name =
    Command_line.output_filename ()
      |> Filename.basename
      |> Filename.remove_extension in
  let c_funcs = C_stubs.cfuncs_of_funcs ~module_name funcs in
  let header = C_stubs.header ~module_name ~c_funcs in
  let stub = C_stubs.stub_file ~header_filename ~c_funcs in
  (header, stub)

let write_tmp_c_stubs ~header ~header_filename ~stub ~stub_filename =
  write_file header_filename header ;
  write_file stub_filename stub 

(* Default: build with C stubs *)
let build ~output_name ~out_dir ~ir ~cmm =
  (* Filename of generated assembly *)
  let assembly_filename = Filename.temp_file output_name ".s" in
  (* Filename of compiled CMM object *)
  let tmp_obj_filename = Filename.temp_file output_name ".o" in
  (* Filename of generated C stub *)
  let stub_c_filename = Filename.temp_file output_name ".c" in
  (* Filename of generated C stub object *)
  let stub_o_filename = Filename.temp_file output_name ".o" in
  (* Filename of temporary C header *)
  let stub_h_filename = Filename.temp_file output_name ".h" in
  (* Filename of temporary RTS header *)
  let rts_header_path = Command_line.rts_header () in
  let rts_h_basename = Filename.basename rts_header_path in
  let rts_h_tmp_filename =
    Filename.concat
      (Filename.get_temp_dir_name ()) rts_h_basename in
  Unix.symlink rts_header_path rts_h_tmp_filename;
  let verbose = Command_line.verbose () in
  let obj_file_name = Filename.concat out_dir (output_name ^ ".o") in
  let header_file_name = Filename.concat out_dir (output_name ^ ".h") in
  if verbose then begin
    Printf.eprintf "@ asm: %s\n%!" assembly_filename;
    Printf.eprintf "@ cmm o: %s\n%!" tmp_obj_filename;
    Printf.eprintf "@ stub c: %s\n%!" stub_c_filename;
    Printf.eprintf "@ stub h: %s\n%!" stub_h_filename;
    Printf.eprintf "@ stub o: %s\n%!" stub_o_filename;
    Printf.eprintf "@ output o: %s\n%!" obj_file_name;
    Printf.eprintf "@ output h: %s\n%!" header_file_name;
  end;
  (* Generate C stubs and headers *)
  let (header, stub) =
    generate_c_stubs 
      ~header_filename:stub_h_filename
      ir in
  write_tmp_c_stubs
    ~header ~header_filename:stub_h_filename
    ~stub ~stub_filename:stub_c_filename;
  (* Compile C stub *)
  call_compiler ["-c"] stub_c_filename stub_o_filename;

  (* Generate WASM .o *)
  gen_asm assembly_filename output_name cmm;
  call_compiler ["-c"] assembly_filename tmp_obj_filename;
  (* Link stub .o and WASM .o to generate final .o file *)
  link_files [tmp_obj_filename; stub_o_filename] obj_file_name;
  write_file header_file_name header;
  cleanup_temp_files
    [assembly_filename; tmp_obj_filename;
     stub_c_filename; stub_h_filename; stub_o_filename]


let parse_module (var, script) =
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

let parse_sexpr name file =
  let ic = open_in file in
  try
    let lexbuf = Lexing.from_channel ic in
    let e = Parse.parse name lexbuf (Parse.Module) in
    close_in ic;
    parse_module e
  with e ->
    close_in ic;
    raise e

let parse_binary name file =
  let input_binary_file name file =
    let open Ast in
    let open Source in
    trace ("Loading (" ^ file ^ ")...");
    let ic = open_in_bin file in
    try
      let len = in_channel_length ic in
      let buf = Bytes.make len '\x00' in
      really_input ic buf 0 len;
      trace "Decoding...";
      let res =
        (None, Script.Encoded (name, (Bytes.to_string buf)) @@ no_region) in
      close_in ic;
      res
    with exn -> close_in ic; raise exn in
  input_binary_file name file
  |> parse_module

let parse_file name file =
  let unsupported _ =
    failwith (file ^ ": unsupported file type (try .wat or .wasm)") in
  Libwasm.Run.dispatch_file_ext
    (parse_binary name)
    (parse_sexpr name)
    unsupported
    unsupported
    unsupported
    file

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
