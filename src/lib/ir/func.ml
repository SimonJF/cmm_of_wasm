open Util.Names

type t = { name: Libwasm.Ast.name option; id : int ; ty: Libwasm.Types.func_type }
let count = ref (-1)
let create ty =
  incr count;
  { name = None; id = !count ; ty}

let print ppf t =
  match t.name with
  | None -> Format.fprintf ppf "f%i" t.id
  | Some name -> Format.fprintf ppf "%s:%i" (name_to_string name) t.id

let is_named t name =
  match t.name with
  | None -> false
  | Some tname -> sanitise (name_to_string tname) = sanitise name

let name t =
  match t.name with
  | None -> None
  | Some name -> Some (sanitise (name_to_string name))

let type_ f = f.ty

let to_sexpr x =
  let open Libwasm.Sexpr in
  let name =
    match name x with
      | Some name ->
          [Atom ("name " ^ name)]
      | None -> [] in
  Node ("fn ", name @ [
    Atom ("id " ^ string_of_int x.id);
    Atom ("ty " ^ Libwasm.Types.string_of_func_type x.ty)
  ])

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

let with_name f name = { f with name = Some name }
