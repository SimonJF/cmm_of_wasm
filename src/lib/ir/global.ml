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
  data : global_data
}

and initial_value =
  | Constant of Libwasm.Values.value
  | AnotherGlobal of t

and global_data =
  | DefinedGlobal of { name: string option; initial_value : initial_value }
  | ImportedGlobal of { module_name: string; global_name: string }

let id x = x.id

let create_defined ~name ~(ty: Types.global_type) ~(initial_value: initial_value) =
  let id = Id.create () in
  (* TODO: Sanitisation / encoding / all that nonsense *)
  let name =
    match name with
      | None -> None
      | Some name -> Some (Util.Names.name_to_string name) in

  let data = DefinedGlobal { name; initial_value } in
  { id; type_ = ty; data }

let create_imported ~module_name ~global_name ~ty =
  let id = Id.create () in
  let data = ImportedGlobal { module_name; global_name } in
  { id; type_ = ty; data }

let data x = x.data

let is_mutable { type_ = GlobalType (_, mut); _ } =
  match mut with
    | Mutable -> true
    | Immutable -> false

let type_ { type_ = GlobalType (typ, _); _ } = typ

let name x =
    match x.data with
      | DefinedGlobal { name } ->
          begin
            match name with
              | Some name -> name
              | None -> ""
          end
      | ImportedGlobal { module_name; global_name } ->
          Printf.sprintf "%s.%s" module_name global_name

let print ppf x =
  match x.data with
    | DefinedGlobal { name } ->
        begin
          match name with
            | Some name -> Format.fprintf ppf "%s" name
            | None -> Format.fprintf ppf "g%s" (Id.to_string x.id)
        end
    | ImportedGlobal { module_name; global_name } ->
        Format.fprintf ppf "%s.%s" module_name global_name

let to_sexpr x =
  let open Libwasm.Sexpr in
  let str =
    Printf.sprintf "global %s:%s:%s"
      (name x)
      (Id.to_string x.id)
      (Types.string_of_global_type x.type_) in
  Atom (str)

