open Libwasm

module Id = struct
  type t = int
  let count = ref (-1)
  let create () =
    incr count;
    !count

  let to_string = string_of_int
  let compare = Pervasives.compare
end

type t = {
  id : Id.t;
  type_ : Types.global_type;
  name : Ast.name option;
  initial_value : Libwasm.Values.value
}

let id x = x.id

let create ~name (type_ : Types.global_type) initial_value =
  let id = Id.create () in
  { id; name; type_ ; initial_value }

let name t =
  match t.name with
    | None -> None
    | Some name -> Some (Util.Names.name_to_string name)

let is_mutable { type_ = GlobalType (_, mut); _ } =
  match mut with
    | Mutable -> true
    | Immutable -> false

let type_ { type_ = GlobalType (typ, _); _ } = typ

let initial_value x = x.initial_value

let print ppf t =
  match t.name with
    | None -> Format.fprintf ppf "g%s" (Id.to_string t.id)
    | Some name -> Format.fprintf ppf "%s" (Util.Names.name_to_string name)

let to_sexpr x =
  let open Libwasm.Sexpr in
  let name =
    match name x with
      | None -> []
      | Some name -> [Atom ("name " ^ name)] in

  Node ("global", name @ [
    Atom ("id " ^ (Id.to_string x.id));
    Atom ("type " ^ (Types.string_of_global_type x.type_))
  ])

