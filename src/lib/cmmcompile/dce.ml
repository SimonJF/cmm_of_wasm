(* Dead code elimination pass. Removes unused let-bindings,
 * being mindful of not eliminating redundant loads / stores
 * as per the WASM spec. *)
open Cmm

type dce_info = {
  usages: int;
  pure: bool
}

let empty_dce_info usages pure = {
  usages;
  pure
}

(* An identifier used where we need to perform an action, but don't need
 * the result. This means we have fewer variables for reg allocation, but
 * still perform the necessary side-effects *)
let impure_ident = Ident.create "impure"

let increment_usages ht ident =
  match Hashtbl.find_opt ht ident with
    | Some dce_inf ->
        Hashtbl.replace ht ident { dce_inf with usages = dce_inf.usages + 1 }
    | None -> Hashtbl.add ht ident (empty_dce_info 1 true)

let mark_purity ht ident is_pure =
  match Hashtbl.find_opt ht ident with
    | Some dce_inf ->
        Hashtbl.replace ht ident { dce_inf with pure = is_pure }
    | None -> Hashtbl.add ht ident (empty_dce_info 0 is_pure)

let get_dce_info ht ident =
  match Hashtbl.find_opt ht ident with
    | Some dce_inf -> dce_inf
    | None -> empty_dce_info 0 true

(* Given an expression, marks all usages, returning true
 * if the expression is pure, returning false if not. *)
let populate_info ht expr =

  let rec go =
    (* Unfortunately, && shortcuts, so we don't get all side-effects
     * performed :P Therefore, we write our own. *)
    let all_pure xs = List.fold_left (fun acc x -> (go x) && acc) true xs in

    function
      | Cvar ident -> increment_usages ht ident; true
      | Clet (ident, e1, e2) ->
          let is_e1_pure = go e1 in
          mark_purity ht ident is_e1_pure;
          go e2
      | Cassign (_, e) ->
          let _ = go e in
          false
      | Ctuple es -> all_pure es
      | Csequence (e1, e2) -> all_pure [e1; e2]
      | Cifthenelse (i, t, e) -> all_pure [i; t; e]
      | Cswitch (e, _, e_arr, _) ->
          let es = Array.to_list e_arr in
          all_pure (e :: es)
      | Cloop e -> go e
      | Ccatch (_, handlers, e) ->
          let e_pure = go e in
          let es_pure =
            List.fold_left (fun acc (_, _, e) -> go e && acc) true handlers in
          e_pure && es_pure
      | Cexit (_, es) -> all_pure es
      | Ctrywith (e1, _, e2) -> all_pure [e1; e2]
      | Cop (Capply _, es, _)
      | Cop (Cextcall _, es, _)
      | Cop (Cextcall_indirect _, es, _)
      | Cop (Cload _, es, _)
      | Cop (Calloc, es, _)
      | Cop (Cstore _, es, _)
      | Cop (Craise _, es, _) ->
          List.iter (fun x -> let _ = go x in ()) es;
          false
      | Cop (_, es, _) -> all_pure es
      (* Remainder are constants, which are trivially pure *)
      | _ -> true in
    go expr

let dce ht expr =
  let rec go = function
    | Clet (ident, e1, e2) ->
        let dce_info = get_dce_info ht ident in
        if dce_info.usages = 0 && dce_info.pure then
          (* Dead code: we can axe it *)
          go e2
        else if dce_info.usages = 0 && (not dce_info.pure) then
          (* Redundant load / store, but we still need to perform it.
           * Bind it to the "impure" identifier. *)
          Clet (impure_ident, go e1, go e2)
        else
          (* Otherwise, we need the binding. *)
          Clet (ident, go e1, go e2)
    | Cassign (ident, e) -> Cassign (ident, go e)
    | Ctuple es -> Ctuple (List.map (go) es)
    | Cop (op, es, dbg) -> Cop (op, List.map (go) es, dbg)
    | Csequence (e1, e2) -> Csequence (go e1, go e2)
    | Cifthenelse (i, t, e) -> Cifthenelse (go i, go t, go e)
    | Cswitch (e, is, es, dbg) -> Cswitch (go e, is, Array.map (go) es, dbg)
    | Cloop e -> Cloop (go e)
    | Ccatch (flag, handlers, e) ->
        let handlers =
          List.map (fun (i, params, e) -> (i, params, go e)) handlers in
        Ccatch (flag, handlers, go e)
    | Cexit (i, es) -> Cexit (i, List.map (go) es)
    | Ctrywith (e1, i, e2) -> Ctrywith (go e1, i, go e2)
    | constant -> constant in
  go expr

let perform_dce expr =
  let ht = Hashtbl.create 300 in
  let _ = populate_info ht expr in
  dce ht expr

