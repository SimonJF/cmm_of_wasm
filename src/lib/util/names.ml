let string_of_name =
  Libwasm.Utf8.encode

let name_of_string =
  Libwasm.Utf8.decode

let sanitise x =
  let orig_x = x in
  let replace_with str chr_str rep_str =
    let re = Str.regexp_string chr_str in
    Str.global_replace re rep_str str in

  (* First, if x is empty, return "_" *)
  if x = "" then "_empty_" else
    begin

    (* First, replace underscores with a triple underscore to avoid clashes *)
      (* disabled for now *)
      (*
    let x = replace_with x "_" "___" in
      *)

    (* Next, need to make sure that the string begins with a legal
     * character. *)
    let re_valid_start = Str.regexp "[_a-zA-Z]" in
    let string_of_char = Printf.sprintf "%c" in
    let x =
      if not (Str.string_match re_valid_start (string_of_char x.[0]) 0) then
        "_" ^ x
      else x in

    (* Next, replace "sensible" yet illegal characters *)
    let x = replace_with x "." "_dot_" in
    let x = replace_with x "-" "_dash_" in

    (* Finally, if the name contains silly characters, strip them out and
     * add a base64 digest to the end. This is stateless and ensures uniqueness.
     * If you use silly names, I'm going to compile them in a silly way :) *)
    let silly_char_regex = Str.regexp "[^_a-zA-Z0-9]" in
    let has_silly_chars =
      try
        let _ = Str.search_forward silly_char_regex x 0 in
        true
      with Not_found -> false in

    if has_silly_chars then
      let md5_hash = Digest.string orig_x |> Digest.to_hex in
      let x = Str.global_replace silly_char_regex "" x in
      x ^ "_" ^ md5_hash
    else x
  end

