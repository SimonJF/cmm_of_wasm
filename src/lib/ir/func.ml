open Util.Names

type core_info = {
  id : int;
  ty: Libwasm.Types.func_type;
}

type import_info = {
  module_name: string;
  function_name: string
}

type t =
  | DefinedFunction of { core_info: core_info; name: Libwasm.Ast.name option }
  | ImportedFunction of { core_info: core_info; import_info: import_info }

let count = ref (-1)

let create_core ty =
  incr count;
  { id = !count; ty }

let create_defined ty =
  let core_info = create_core ty in
  DefinedFunction { core_info; name = None }

let create_imported ~module_name ~function_name ty =
  let import_info = { module_name; function_name } in
  let core_info = create_core ty in
  ImportedFunction { core_info; import_info }

let imported_function_name import_info =
  import_info.module_name ^ "_" ^ import_info.function_name


let print ppf t =
  match t with
    | DefinedFunction { core_info; name } ->
        begin
          match name with
          | None -> Format.fprintf ppf "f%i" core_info.id
          | Some name -> Format.fprintf ppf "%s:%i" (name_to_string name) core_info.id
        end
    | ImportedFunction { core_info; import_info } ->
        Format.fprintf ppf "%s:%i" (imported_function_name import_info) core_info.id

let name t =
  match t with
    | DefinedFunction { name } ->
        begin
          match name with
          | None -> None
          | Some name -> Some (sanitise (name_to_string name))
        end
    | ImportedFunction { core_info; import_info } ->
        Some (imported_function_name import_info)

let type_ = function
  | DefinedFunction { core_info } ->
      core_info.ty
  | ImportedFunction { core_info } ->
      core_info.ty

let to_sexpr x =
  let open Libwasm.Sexpr in
  let name =
    match name x with
      | Some name -> name
      | None -> "" in
  match x with
    | DefinedFunction { core_info; _ } ->
        let id = core_info.id in
        let ty = Libwasm.Types.string_of_func_type core_info.ty in
        Atom (Printf.sprintf "fun %i:%s:%s" id name ty)
    | ImportedFunction { core_info; import_info } ->
        let id = core_info.id in
        let ty = Libwasm.Types.string_of_func_type core_info.ty in
        Atom (Printf.sprintf "fun-imp %i:%s:%s" id name ty)

let func_id = function
  | DefinedFunction { core_info } -> core_info.id
  | ImportedFunction { core_info } -> core_info.id

module M = struct
  type nonrec t = t
  let compare a b = compare (func_id a) (func_id b)
end
module Map = struct
  include Map.Make(M)
  let print f ppf s =
    let elts ppf s = iter (fun id v ->
        Format.fprintf ppf "@ (@[%a@ %a@])" print id f v) s in
    Format.fprintf ppf "@[<1>{@[%a@ @]}@]" elts s
end

let with_name f name =
  match f with
    | DefinedFunction { core_info } ->
        DefinedFunction { core_info; name = Some name }
    | ImportedFunction _ -> failwith "cannot set name on imported function"
