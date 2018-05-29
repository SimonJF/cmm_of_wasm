
module Refs = struct
  let filename = ref ""
  let shared = ref false
  let generate_c = ref true
  let dump_cmm = ref false
  let dump_stackless = ref false
  let dump_linear = ref false
  let verbose = ref false
  let cc = ref "cc"
  let dump_wasm = ref false
  let output_filename = ref None
end

let options = 
  let open Getopt in
  [ ('s', "shared", Some (fun () -> Refs.shared := true), None);
    ('n', "nocgen", Some (fun () -> Refs.generate_c := false), None);
    (noshort, "dcmm", Some (fun () -> Refs.dump_cmm := true), None);
    (noshort, "dlinear", Some (fun () -> Refs.dump_linear := true), None);
    (noshort, "dstackless", Some (fun () -> Refs.dump_stackless := true), None);
    (noshort, "dwasm", Some (fun () -> Refs.dump_wasm := true), None);
    ('v', "verbose", Some (fun () -> Refs.verbose := true), None);
    (noshort, "cc", None, Some (fun s -> Refs.cc := s));
    ('o', nolong, None, Some (fun s -> Refs.output_filename := (Some s)))
  ]

let set_filename fn = Refs.filename := fn

let print_syntax () =
  Printf.printf "Syntax: cmm_of_wasm [-s -n -v -o <name>] [--shared --nocgen --dstackless --dcmm --dlinear  --verbose] [--cc=CC]] filename\n";
  exit (-1)

let setup () =
  if Array.length Sys.argv <= 1 then print_syntax () else 
  try
    Getopt.parse_cmdline options set_filename
   with | _ -> print_syntax ()
    
let shared () = !(Refs.shared)
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
    | None -> !Refs.filename
