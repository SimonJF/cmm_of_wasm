open Libwasm
open Cmm


let filename = "add.wasm"

(* Crude, for now *)
let log s = Printf.printf "%s\n" s

let load_binary filename =
  log ("Loading " ^ filename);
  let ic = open_in_bin filename in
  try
    let len = in_channel_length ic in
    let buf = Bytes.make len '\x00' in
    really_input ic buf 0 len;
    log "Decoding...";
    let success = Decode.decode filename (Bytes.to_string buf) in
    close_in ic;
    success
  with exn -> close_in ic; raise exn 


let () =
    (* Load file -- TEMP, will want to do a fancier loader after
     * we get the basics written *)
    log "Hello, and welcome to cmm_of_wasm!";
    let wasm_module = load_binary filename in
    Libwasm.Print.module_ stdout !Flags.width wasm_module;
    log "Eliminating stack...";
    let _ = Eliminate_stack.compile_module wasm_module in
    log "Done!"
