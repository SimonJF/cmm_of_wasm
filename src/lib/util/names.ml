let name_to_string =
  Libwasm.Utf8.encode

let string_to_name =
  Libwasm.Utf8.decode

let internal_name name =
  "__cmmWasm" ^ (String.capitalize_ascii name)
