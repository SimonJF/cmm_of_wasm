
module Refs = struct
  let filename = ref ""
  let test = ref false
end

let options = 
  [ ('t', "test", Some (fun () -> Refs.test := true), None) ]

let set_filename fn = Refs.filename := fn

let print_syntax () =
  Printf.printf "Syntax: cmm_of_wasm [-t] filename\n";
  exit (-1)

let setup () =
  if Array.length Sys.argv <= 1 then print_syntax () else 
  try
    Getopt.parse_cmdline options set_filename
  with | _ -> print_syntax ()
    
let filename () = !(Refs.filename)
let test () = !(Refs.test)

