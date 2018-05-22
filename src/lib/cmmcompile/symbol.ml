type symbol =
  | Exported_symbol of string
  | Internal_symbol of string

let name = function
  | Exported_symbol s -> s
  | Internal_symbol s -> s
