let name_to_string name =
  (* let n_to_string i = *)
  (*   if i < 256 then *)
  (*     String.make 1 (Char.chr i) *)
  (*   else *)
  (*     Printf.sprintf "(0x%x)" i *)
  (* in *)
  (* String.concat "" (List.map n_to_string name) *)
  Libwasm.Utf8.encode name

let internal_name name =
  "__cmmWasm" ^ (String.capitalize_ascii name)
