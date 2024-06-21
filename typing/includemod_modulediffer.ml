(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                Malo Monin, projet Cambium, Inria Paris                 *)
(*                                                                        *)
(*   Copyright 2024 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type ('v, 't) field = {
  item : Types.signature_item;
  value : 'v;
  type_ : 't;
}

let field_name field = Ident.name (Types.signature_item_id field.item)

type suggestion =
  | Suggest_add of Types.signature_item
  | Suggest_rename of Types.signature_item * string
  | Suggest_change_value_type of Ident.t * Types.type_expr

let apply_suggestion subst suggestion =
  match suggestion with
  | Suggest_rename (item, suggested_name) ->
      (* FIXME: This does not work. *)
      let id = Types.signature_item_id item in
      let old_path = Path.Pident id in
      let new_ident = Ident.create_local suggested_name in
      Subst.add_type new_ident old_path subst
  | _ -> subst

let fuzzy_match_names compatibility_test missings additions =
  let cutoff = 8 in
  let m = List.length missings in
  let n = List.length additions in

  (* An implementation of the Gale-Shapley algorithm, where missing fields
      propose and added fields accept or refuse. *)
  let stable_marriages () =
    let missing_fields = Array.of_list missings in
    let added_fields = Array.of_list additions in

    let preferences =
      let added_names =
        additions
        |> List.mapi (fun j field -> (field_name field, j))
        |> Misc.Trie.of_list
      in
      Array.init m (fun i ->
          let missing_name = field_name missing_fields.(i) in
          Misc.Trie.compute_preferences ~cutoff added_names missing_name)
    in
    (* [is_married.(i)] indicates whether there exists a [j] such that
        [missing_fields.(i)] and [added_fields.(j)] are married. *)
    let is_married = Array.make m false in
    (* [added_fields.(j)] is married with [missing_fields.(i)] iff
        [added_fields.(j) = Some (i, d)] where [d] is the edition distance
        between the names. *)
    let marriages = Array.make n None in

    (* Marries [missing_fields.(i)] with [added_fields.(j)], unless
        [added_fields.(j)] already has a better marriage. *)
    let try_marry i j d =
      if compatibility_test missing_fields.(i) added_fields.(j) then
        match marriages.(j) with
        | None ->
            marriages.(j) <- Some (i, d);
            is_married.(i) <- true
        | Some (i', d') ->
            if d < d' then (
              marriages.(j) <- Some (i, d);
              is_married.(i) <- true;
              is_married.(i') <- false)
    in

    let ok = ref false in
    while not !ok do
      ok := true;
      for i = 0 to m - 1 do
        if not is_married.(i) then
          match preferences.(i) () with
          | Seq.Nil -> ()
          | Seq.Cons ((j, d), tl) ->
              ok := false;
              preferences.(i) <- tl;
              try_marry i j d
      done
    done;

    let actually_missing =
      missings
      |> List.filteri (fun i _ -> not is_married.(i))
      |> List.map (fun missing -> Suggest_add missing.item)
    in
    let name_changes = ref [] in
    for j = 0 to n - 1 do
      match marriages.(j) with
      | None -> ()
      | Some (i, _) ->
          let name_change =
            Suggest_rename
              (added_fields.(j).item,
              field_name missing_fields.(i))
          in
          name_changes := name_change :: !name_changes
    done;

    actually_missing @ !name_changes
  in

  let greedy () =
    let rec list_extract predicate l =
      match l with
      | [] -> None
      | hd :: tl when predicate hd -> Some (hd, tl)
      | hd :: tl -> (
          match list_extract predicate tl with
          | None -> None
          | Some (x, tl') -> Some (x, hd :: tl'))
    in

    let compute_distance expected_field added_field =
      if compatibility_test expected_field added_field then
        let distance =
          Misc.edit_distance
            (field_name expected_field)
            (field_name added_field)
            cutoff
        in
        Misc.Maybe_infinite.of_option distance
      else
        Misc.Maybe_infinite.Infinity ()
    in

    let remaining_added_fields = ref additions in
    let name_changes = ref [] in
    let actually_missing =
      missings
      |> List.filter
        (fun missing_field ->
          match
            list_extract
              (fun added_field ->
                compute_distance missing_field added_field
                  < Misc.Maybe_infinite.Finite cutoff)
              !remaining_added_fields
          with
          | None -> true
          | Some (added_field, additions) ->
              let name_change =
                Suggest_rename (added_field.item, field_name missing_field)
              in
              name_changes := name_change :: !name_changes;
              remaining_added_fields := additions;
              false)
      |> List.map (fun missing -> Suggest_add missing.item)
    in

    actually_missing @ !name_changes
  in

  if m * n <= 1_000_000 then
    stable_marriages ()
  else
    greedy ()

let compute_signature_diff env subst sig1 sig2 =
  try
    let _ = Includemod.signatures env ~subst ~mark:false sig1 sig2 in
    None
  with
  | Includemod.Error (_, Includemod.Error.In_Signature reason) -> Some reason
