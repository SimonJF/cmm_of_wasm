(* Dead code elimination pass. Removes unused let-bindings,
 * being mindful of not eliminating redundant loads / stores
 * as per the WASM spec. *)
open Cmm

type purity = Pure | Impure

type dce_info = {
  usages: int;
  pure: purity;
  expr: expression option
}

let empty_dce_info usages pure expr = {
  usages;
  pure;
  expr
}

let increment_usages ht ident =
  (* Function parameters aren't in the hashtable. *)
  let ident_name = Ident.name ident in
  match Hashtbl.find_opt ht ident_name with
    | Some dce_inf ->
        Hashtbl.replace ht ident_name
          { dce_inf with usages = dce_inf.usages + 1 }
    | None ->
        Hashtbl.add ht ident_name (empty_dce_info 1 Pure None)

let mark_purity ht ident is_pure =
  let ident_name = Ident.name ident in
  match Hashtbl.find_opt ht ident_name with
    | Some dce_inf ->
        Hashtbl.replace ht ident_name { dce_inf with pure = is_pure }
    | None ->
        Hashtbl.add ht ident_name (empty_dce_info 0 is_pure None)

let set_expression ht ident expr =
  let ident_name = Ident.name ident in
  match Hashtbl.find_opt ht ident_name with
    | Some dce_info ->
        Hashtbl.replace ht ident_name { dce_info with expr = (Some expr) }
    | None ->
        Hashtbl.add ht ident_name (empty_dce_info 0 Pure (Some expr))

let get_dce_info ht ident =
  let ident_name = Ident.name ident in
  match Hashtbl.find_opt ht ident_name with
    | Some x -> x
    | None -> empty_dce_info 0 Pure None

(* Given an expression, marks all usages, returning true
 * if the expression is pure, returning false if not. *)
let populate_info ht expr =

  let rec go =
    (* Unfortunately, && shortcuts, so we don't get all side-effects
     * performed :P Therefore, we write our own. *)
    let all_pure xs =
      List.fold_left (fun acc x -> if (go x) = Pure then acc else Impure) Pure xs in

    function
      | Cvar ident -> increment_usages ht ident; Pure
      | Clet (ident, e1, e2) ->
          set_expression ht ident e1;
          let is_e1_pure = go e1 in
          mark_purity ht ident is_e1_pure;
          go e2
      | Cassign (_, e) ->
          let _ = go e in
          Impure
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
            List.map (fun (_, _, e) -> go e) handlers
            |> List.fold_left (fun acc x -> if x = Pure then acc else Impure) Pure in
          if e_pure = Pure && es_pure = Pure then
            Pure
          else Impure
      | Cexit (_, es) -> all_pure es
      | Ctrywith (e1, _, e2) -> all_pure [e1; e2]
      | Cop (Capply _, es, _)
      | Cop (Cdivi, es, _)
      | Cop (Cdiviu, es, _)
      | Cop (Cextcall _, es, _)
      | Cop (Cextcall_indirect _, es, _)
      | Cop (Cload _, es, _)
      | Cop (Calloc, es, _)
      | Cop (Cstore _, es, _)
      | Cop (Craise _, es, _) ->
          List.iter (fun x -> let _ = go x in ()) es;
          Impure
      | Cop (_, es, _) -> all_pure es
      (* Remainder are constants, which are trivially pure *)
      | Cconst_int _
      | Cconst_natint  _
      | Cconst_float _
      | Cconst_symbol _
      | Cconst_pointer _
      | Cconst_natpointer _
      | Cblockheader _  -> Pure in
    go expr

let dce ht expr =
  let rec go = function
    | (Cvar ident) as unchanged ->
        (* If the variable is only used once, and the expression it is
         * bound to is pure, then we can inline. *)
        let dce_info = get_dce_info ht ident in
        if dce_info.usages = 1 && dce_info.pure = Pure then
          begin
            match dce_info.expr with
              | Some expr -> go expr
              | None -> unchanged
          end
        else
          unchanged
    | Clet (ident, e1, e2) ->
        let dce_info = get_dce_info ht ident in
        if dce_info.usages = 0 && dce_info.pure = Pure then
          (* Dead code: we can axe it *)
          go e2
        else if dce_info.usages = 1 && dce_info.pure = Pure then
          (* Only used once, so reasonable to inline. We can kill
           * the binding. *)
          go e2
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
    (* Constants *)
    | Cconst_int _
    | Cconst_natint  _
    | Cconst_float _
    | Cconst_symbol _
    | Cconst_pointer _
    | Cconst_natpointer _
    | Cblockheader _ as e -> e in
  go expr

let perform_dce expr =
  let ht = Hashtbl.create 300 in
  let _ = populate_info ht expr in
  dce ht expr

