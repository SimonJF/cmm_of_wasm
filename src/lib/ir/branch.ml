type t = {
  label: Label.t;
  args: Var.t list
}

let create label args = {
  label;
  args
}

let label x = x.label
let arguments x = x.args

let to_sexpr x =
  let open Libwasm.Sexpr in
  let args_str =
    List.map (Var.to_string) x.args |> String.concat ", " in
  Atom (Printf.sprintf "%s(%s)" (Label.branch_string x.label) args_str)

