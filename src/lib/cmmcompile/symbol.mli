type symbol =
  | Exported_symbol of string
  | Internal_symbol of string

val name : symbol -> string
