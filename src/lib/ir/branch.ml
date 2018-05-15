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
