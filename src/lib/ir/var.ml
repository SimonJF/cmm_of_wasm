open Libwasm

type t = (int * Types.value_type)
let compare ((x, _):t) (y, _) = Pervasives.compare x y
let count = ref (-1)
let create typ = incr count; !count, typ
let rename (_, typ) = create typ
let type_ (_, typ) = typ
let print ppf (t, _) = Format.fprintf ppf "v%i" t
let reset () = count := -1
let to_string (id, _) = Format.sprintf "v%i" id

module M = struct
    type nonrec t = t
    let compare = compare
end
module Map = Map.Make(M)
