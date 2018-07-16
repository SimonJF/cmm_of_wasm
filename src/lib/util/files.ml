(* I was getting fed up of looking this up every time... *)
(* Still more intuitive than Java. *)
let read_text_file filename =
  let ic = open_in filename in
  try
    let len = in_channel_length ic in
    let buf = Bytes.make len '\x00' in
    really_input ic buf 0 len;
    close_in ic;
    Bytes.to_string buf
  with exn -> close_in ic; raise exn
