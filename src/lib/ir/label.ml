module Id = struct
  type t =
    | Gen of int
    | Return
  let count = ref (-1)
  let create () = incr count; Gen !count
  let return = Return
  let print ppf t =
    match t with
      | Gen t -> Format.fprintf ppf "l%i" t
      | Return -> Format.fprintf ppf "return"
    let is_return = function
      | Return -> true
      | _ -> false

  let reset () = count := -1

  module M = struct
    type nonrec t = t
    let compare = Pervasives.compare
  end
  module Map = Map.Make(M)
end

type t = {
  id : Id.t;
  arity: int
}

let create ~arity = {
  id = Id.create();
  arity = arity
}

let id lbl = lbl.id
let arity lbl = lbl.arity