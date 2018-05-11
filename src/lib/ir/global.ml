open Libwasm

type t = {
  id : int;
  type_ : Types.global_type;
  name : Ast.name option;
}

let count = ref (-1)
let create ~name (type_ : Types.global_type) =
  incr count;
  let id = !count in
  { id; name; type_ }

let name t =
  match t.name with
    | None -> None
    | Some name -> Some (Util.name_to_string name)

let is_mutable { type_ = GlobalType (_, mut); _ } =
  match mut with
    | Mutable -> true
    | Immutable -> false

let type_ { type_ = GlobalType (typ, _); _ } = typ

let print ppf t =
  match t.name with
    | None -> Format.fprintf ppf "g%i" t.id
    | Some name -> Format.fprintf ppf "%s" (Util.name_to_string name)

module M = struct
  type nonrec t = t
  let compare a b = compare a.id b.id
end

module Map = struct
  include Map.Make(M)
  let print f ppf s =
    let elts ppf s = iter (fun id v ->
        Format.fprintf ppf "@ (@[%a@ %a@])" print id f v) s in
    Format.fprintf ppf "@[<1>{@[%a@ @]}@]" elts s
end
