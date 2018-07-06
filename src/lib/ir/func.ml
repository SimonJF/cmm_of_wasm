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
  | DefinedFunction of { core_info: core_info }
  | ImportedFunction of { core_info: core_info; import_info: import_info }

let count = ref (-1)

let create_core ty =
  incr count;
  { id = !count; ty }

let create_defined ty =
  let core_info = create_core ty in
  DefinedFunction { core_info }

let create_imported ~module_name ~function_name ty =
  let import_info = { module_name; function_name } in
  let core_info = create_core ty in
  ImportedFunction { core_info; import_info }

let print ppf t =
  match t with
    | DefinedFunction { core_info } ->
        Format.fprintf ppf "fun %i:%s" core_info.id
          (Libwasm.Types.string_of_func_type core_info.ty)
    | ImportedFunction { core_info; import_info } ->
        Format.fprintf ppf "fun-imp %s.%s:%i:%s"
          import_info.module_name
          import_info.function_name
          core_info.id
          (Libwasm.Types.string_of_func_type core_info.ty)

let to_string t =
  let open Format in
  print str_formatter t;
  flush_str_formatter ()

let type_ = function
  | DefinedFunction { core_info } ->
      core_info.ty
  | ImportedFunction { core_info } ->
      core_info.ty

let to_sexpr x = Libwasm.Sexpr.Atom (to_string x)

let func_id = function
  | DefinedFunction { core_info } -> core_info.id
  | ImportedFunction { core_info } -> core_info.id

let symbol ~module_name = function
  | DefinedFunction { core_info } ->
      module_name ^ "_funcinternal_" ^ (string_of_int core_info.id)
  | ImportedFunction { import_info } ->
      import_info.module_name ^ "_func_" ^ import_info.function_name

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

let is_imported f =
  match f with
    | DefinedFunction _ -> false
    | ImportedFunction _ -> true
