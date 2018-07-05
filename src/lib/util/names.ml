let name_to_string =
  Libwasm.Utf8.encode

let string_to_name =
  Libwasm.Utf8.decode

let sanitise x =
  let re = Str.regexp "[^_a-zA-Z0-9]" in
  Str.global_replace re "_"  x
