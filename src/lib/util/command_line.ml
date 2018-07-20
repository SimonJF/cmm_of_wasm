
module Refs = struct
  let filename = ref ""
  let generate_c = ref true
  let dump_cmm = ref false
  let dump_stackless = ref false
  let dump_linear = ref false
  let verbose = ref false
  let cc = ref "cc"
  let dump_wasm = ref false
  let output_filename = ref None
  let header_prefix_path = ref None
  let rts_path = ref None
  let initial_fuel = ref 500
  let keep_temp = ref false
  let prefix = ref None
  let colouring_allocator = ref false
end

let options =
  let open Getopt in
  [ ('n', "nocgen", Some (fun () -> Refs.generate_c := false), None);
    (noshort, "dcmm", Some (fun () -> Refs.dump_cmm := true), None);
    (noshort, "dlinear", Some (fun () -> Refs.dump_linear := true), None);
    (noshort, "dstackless", Some (fun () -> Refs.dump_stackless := true), None);
    (noshort, "dwasm", Some (fun () -> Refs.dump_wasm := true), None);
    ('v', "verbose", Some (fun () -> Refs.verbose := true), None);
    (noshort, "cc", None, Some (fun s -> Refs.cc := s));
    ('o', nolong, None, Some (fun s -> Refs.output_filename := (Some s)));
    (noshort, "rts", None, Some (fun s -> Refs.rts_path := (Some s)));
    (noshort, "header-prefix", None, Some (fun s -> Refs.header_prefix_path := (Some s)));
    (noshort, "fuel", None, Some (fun i -> Refs.initial_fuel := (int_of_string i)));
    ('t', "keep-temp", Some (fun () -> Refs.keep_temp := true), None);
    ('p', "prefix", None, Some (fun s -> Refs.prefix := (Some s)));
    (noshort, "colouring", Some (fun () -> Refs.colouring_allocator := true), None)
  ]

let set_filename fn = Refs.filename := fn

let print_syntax () =
  Printf.printf
    "Syntax: cmm_of_wasm [-s -n -v -o <name>] \
     [--nocgen --dstackless --dcmm --dlinear \
     --verbose --fuel=FUEL --header-prefix=PREFIX \
     -rts=RTS --cc=CC] filename\n";
  exit (-1)

let setup () =
  if Array.length Sys.argv <= 1 then print_syntax () else
  try
    Getopt.parse_cmdline options set_filename
   with | _ -> print_syntax ()

let generate_c () = !(Refs.generate_c)
let dump_stackless () = !(Refs.dump_stackless)
let dump_cmm () = !(Refs.dump_cmm)
let dump_linear () = !(Refs.dump_linear)
let dump_wasm () = !(Refs.dump_wasm)
let verbose () = !(Refs.verbose)
let cc () = !(Refs.cc)

let input_filename () = !(Refs.filename)

let output_filename () =
  match !(Refs.output_filename) with
    | Some fn -> fn
    | None ->
        Filename.basename !Refs.filename
        |> Filename.chop_extension

(* Eh, it'll work for now; we'll want to do something better
 * when we're packaging on opam later *)
let default_includes_dir =
  let exe_dir = Filename.dirname Sys.executable_name in
  Filename.concat exe_dir "includes"

let header_filename = "header-prefix.h"

let header_prefix_path () =
  match !Refs.header_prefix_path with
    | Some fn -> fn
    | None -> Filename.concat default_includes_dir header_filename

let default_rts_path =
  Filename.concat default_includes_dir "rts"

let rts_path () =
  match !Refs.rts_path with
    | Some fn -> fn
    | None -> default_rts_path

let rts_header () = Filename.concat (rts_path ()) "wasm-rt.h"

let initial_fuel () = !Refs.initial_fuel

let keep_temp () = !Refs.keep_temp

let prefix () = !Refs.prefix

let colouring_allocator () = !Refs.colouring_allocator
