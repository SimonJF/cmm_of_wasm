(* Pierre Chambart's work, repurposed for cmm_of_wasm *)
open Libwasm
open Util
open Util.Trace


(* We have to copy this across from asmgen, unfortunately, since without
 * memory checks, the OCaml compiler does unsafe DCE on loads / stores.
 * This is against the WASM spec, and causes the tests to fail. *)


let liveness ppf phrase =
  Liveness.fundecl ppf phrase; phrase

let dump_if ppf flag message phrase =
  if !flag then Printmach.phase message ppf phrase

let pass_dump_if ppf flag message phrase =
  dump_if ppf flag message phrase; phrase

let pass_dump_linear_if ppf flag message phrase =
  if !flag then Format.fprintf ppf "*** %s@.%a@." message Printlinear.fundecl phrase;
  phrase

let rec regalloc ppf round fd =
  let open Clflags in
  if round > 50 then
    Misc.fatal_error(fd.Mach.fun_name ^
                ": function too complex, cannot complete register allocation");
  dump_if ppf dump_live "Liveness analysis" fd;
  if !use_linscan then begin
    (* Linear Scan *)
    Interval.build_intervals fd;
    if !dump_interval then Printmach.intervals ppf ();
    Linscan.allocate_registers()
  end else begin
    (* Graph Coloring *)
    Interf.build_graph fd;
    if !dump_interf then Printmach.interferences ppf ();
    if !dump_prefer then Printmach.preferences ppf ();
    Coloring.allocate_registers()
  end;
  dump_if ppf dump_regalloc "After register allocation" fd;
  let (newfd, redo_regalloc) = Reload.fundecl fd in
  dump_if ppf dump_reload "After insertion of reloading code" newfd;
  if redo_regalloc then begin
    Reg.reinit(); Liveness.fundecl ppf newfd; regalloc ppf (round + 1) newfd
  end else newfd

let (++) x f = f x

let compile_fundecl (ppf : Format.formatter) fd_cmm =
  let open Clflags in
  Proc.init ();
  Reg.reset();
  fd_cmm
  ++ Profile.record ~accumulate:true "selection" Selection.fundecl
  ++ pass_dump_if ppf dump_selection "After instruction selection"
  (*
  ++ Profile.record ~accumulate:true "comballoc" Comballoc.fundecl
  ++ pass_dump_if ppf dump_combine "After allocation combining"
  ++ Profile.record ~accumulate:true "cse" CSE.fundecl
  ++ pass_dump_if ppf dump_cse "After CSE"
  *)
  ++ Profile.record ~accumulate:true "liveness" (liveness ppf)
  (* ++ Profile.record ~accumulate:true "deadcode" Deadcode.fundecl *)
  ++ pass_dump_if ppf dump_live "Liveness analysis"
  ++ Profile.record ~accumulate:true "spill" Spill.fundecl
  ++ Profile.record ~accumulate:true "liveness" (liveness ppf)
  ++ pass_dump_if ppf dump_spill "After spilling"
  ++ Profile.record ~accumulate:true "split" Split.fundecl
  ++ pass_dump_if ppf dump_split "After live range splitting"
  ++ Profile.record ~accumulate:true "liveness" (liveness ppf)
  ++ Profile.record ~accumulate:true "regalloc" (regalloc ppf 1)
  (* ++ Profile.record ~accumulate:true "available_regs" Available_regs.fundecl *)
  ++ Profile.record ~accumulate:true "linearize" Linearize.fundecl
  ++ pass_dump_linear_if ppf dump_linear "Linearized code"
  ++ Profile.record ~accumulate:true "scheduling" Scheduling.fundecl
  ++ pass_dump_linear_if ppf dump_scheduling "After instruction scheduling"
  ++ Profile.record ~accumulate:true "emit" Emit.fundecl

let compile_phrase ppf p =
  let open Clflags in
  if !dump_cmm then Format.fprintf ppf "%a@." Printcmm.phrase p;
  match p with
  | Cfunction fd -> compile_fundecl ppf fd
  | Cdata dl -> Emit.data dl

(* End of stuff copied from Asmgen. *)

let gen_asm out name cmm =
  Clflags.dump_linear := Command_line.dump_linear ();
  Clflags.use_linscan := (not (Command_line.colouring_allocator ()));
  Emitaux.create_asm_file := true;
  Emitaux.output_channel := open_out out;
  Compilenv.reset ?packname:None name;
  Emit.begin_assembly ();
  List.iter (compile_phrase Format.std_formatter) cmm;
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

let cleanup_temp_files files =
  if not (Command_line.keep_temp ()) then
    List.iter (Sys.remove) files

let write_file (filename: string) (contents: string) =
  let oc = open_out filename in
  Printf.fprintf oc "%s\n" contents;
  close_out oc

let generate_c_stubs ~header_filename ~prefix (ir_mod:Ir.Stackless.module_) =
  let exports = C_stubs.c_exports ~prefix ir_mod in
  let header = C_stubs.header ~prefix ~exports in
  let stub = C_stubs.stub_file ~header_filename ~prefix ~exports in
  (header, stub)

let write_tmp_c_stubs ~header ~header_filename ~stub ~stub_filename =
  write_file header_filename header ;
  write_file stub_filename stub

(* Default: build with C stubs *)
let build ~output_name ~prefix ~out_dir ~ir ~cmm =
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
  try
    Unix.symlink rts_header_path rts_h_tmp_filename
  with _ -> ();
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
      ~prefix
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

let parse_sexpr filename =
  let ic = open_in filename in
  try
    let lexbuf = Lexing.from_channel ic in
    let e = Parse.parse filename lexbuf (Parse.Module) in
    close_in ic;
    parse_module e
  with e ->
    close_in ic;
    raise e

let parse_binary filename =
  let input_binary_file =
    let open Source in
    trace ("Loading (" ^ filename ^ ")...");
    let ic = open_in_bin filename in
    try
      let len = in_channel_length ic in
      let buf = Bytes.make len '\x00' in
      really_input ic buf 0 len;
      trace "Decoding...";
      let res =
        (None, Script.Encoded (filename, (Bytes.to_string buf)) @@ no_region) in
      close_in ic;
      res
    with exn -> close_in ic; raise exn in
  parse_module input_binary_file

let parse_file filename =
  let unsupported _ =
    failwith (filename ^ ": unsupported file type (try .wat or .wasm)") in
  Libwasm.Run.dispatch_file_ext
    (parse_binary)
    (parse_sexpr)
    unsupported
    unsupported
    unsupported
    filename
