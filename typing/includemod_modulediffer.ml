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

let rec list_remove x list =
  match list with
  | [] -> []
  | hd :: tl when hd = x -> tl
  | hd :: tl -> hd :: list_remove x tl

let list_uncons list =
  match list with
  | [] -> failwith "Expected list not to be empty"
  | hd :: tl -> hd, tl

type ('v, 't) field = {
  item : Types.signature_item;
  value : 'v;
  type_ : 't;
}

let field_id field = Types.signature_item_id field.item
let field_name field = Ident.name (field_id field)

type rename_suggestion = {
  item_to_rename : Types.signature_item;
  suggested_ident : Ident.t;
}

type suggestion =
  | Suggest_add of Types.signature_item
  | Suggest_rename of rename_suggestion
  | Suggest_change_value_type of Types.signature_item * Types.type_expr

let suggestion_item suggestion =
  match suggestion with
  | Suggest_add item ->
      item
  | Suggest_rename {item_to_rename; _} ->
      item_to_rename
  | Suggest_change_value_type (item, _) ->
      item

let apply_suggestion subst suggestion =
  match suggestion with
  | Suggest_rename {item_to_rename = Sig_type (id, _, _, _); suggested_ident} ->
      (* FIXME: This does not work. *)
      let corresponding_path = Path.Pident suggested_ident in
      Subst.add_type id corresponding_path subst
  | _ -> subst

let fuzzy_match_names compatibility_test missings additions =
  (* The edit distance between an existing name and a suggested rename must be
     at most half the length of the name. *)
  let cutoff name = String.length name / 2 in
  let m = List.length missings in
  let n = List.length additions in

  (* An implementation of Zoltan Kiraly's "New Algorithm" presented in
     "Linear Time Local Approximation Algorithm for Maximum Stable Marriage":
     https://www.mdpi.com/1999-4893/6/3/471. It computes a 3/2-approximation of
     a maximum stable marriage in linear time (linear in the sum of the lengths
     of the preference lists).

     The man/woman semantic is used in order to be consistent with the paper.
     Missing items are men, added items are women. *)
  let stable_marriages () =
    let open struct
      type distance = int

      type lad_preferences = {
        mutable lad_previous_layers : (int list * distance) list;
          (** Invariant: this list is not empty. *)
        mutable lad_current_layer : int list;
          (** Invariant: this list is not empty. *)
        mutable lad_current_layer_distance : distance;
        mutable lad_next_layers : (int list * distance) Seq.t
      }

      type bachelor_preferences = {
        mutable bachelor_current_layer : int list;
          (** Invariant: the list is not empty. *)
        mutable bachelor_current_layer_distance : distance;
        mutable bachelor_next_layers : (int list * distance) list;
      }

      type man_situation =
        | Lad of lad_preferences
        | Bachelor of bachelor_preferences

      type man_state =
        | Active of man_situation
        | Engaged_man of man_situation
        | Inactive
          (** Old bachelor. *)

      type woman_state =
        | Maiden
        | Engaged_woman of int * distance
    end in

    let missing_fields = Array.of_list missings in
    let added_fields = Array.of_list additions in

    let man_states =
      let added_names =
        additions
        |> List.mapi (fun j field -> (field_name field, j))
        |> Misc.Trie.of_list
      in
      Array.init m (fun i ->
        let missing_name = field_name missing_fields.(i) in
        let sequence =
          Misc.Trie.compute_preference_layers
            ~cutoff:(cutoff missing_name)
            added_names
            missing_name
        in
        match sequence () with
        | Seq.Nil ->
            Inactive
        | Seq.Cons ((layer, distance), tail) ->
            Active (Lad {
              lad_previous_layers = [ (layer, distance) ];
              lad_current_layer = layer;
              lad_current_layer_distance = distance;
              lad_next_layers = tail;
            })
      )
    in
    let woman_states = Array.make n Maiden in

    let is_uncertain i =
      match man_states.(i) with
      | Engaged_man Lad {lad_current_layer = current_layer; _}
      | Engaged_man Bachelor {bachelor_current_layer = current_layer; _} ->
        let women = current_layer in
        List.exists (fun j -> woman_states.(j) = Maiden) women
      | _ -> false
    in

    let is_flighty j =
      match woman_states.(j) with
      | Engaged_woman (i, _) -> is_uncertain i
      | _ -> false
    in

    let man_situation i =
      match man_states.(i) with
      | Active situation | Engaged_man situation -> Some situation
      | Inactive -> None
    in

    let get_preferred_woman man_situation =
      match man_situation with
      | Lad preferences ->
          preferences.lad_current_layer
          |> List.find_opt (fun j -> woman_states.(j) = Maiden)
          |> Option.value ~default:(List.hd preferences.lad_current_layer),
          preferences.lad_current_layer_distance
      | Bachelor preferences ->
          preferences.bachelor_current_layer
          |> List.find_opt (fun j -> woman_states.(j) = Maiden)
          |> Option.value ~default:(List.hd preferences.bachelor_current_layer),
          preferences.bachelor_current_layer_distance
    in

    let propose i j d =
      if is_flighty j then
        true
      else
        match woman_states.(j) with
        | Maiden -> true
        | Engaged_woman (i', d') ->
            d < d' ||
            d = d' &&
              match man_situation i, man_situation i' with
              | Some Bachelor _, Some Lad _ -> true
              | _ -> false
    in

    let remove_woman i j =
      match man_situation i with
      | Some Lad preferences -> (
          match list_remove j preferences.lad_current_layer with
          | [] -> (
              match preferences.lad_next_layers () with
              | Seq.Nil ->
                  let (layer, distance), tail =
                    list_uncons preferences.lad_previous_layers
                  in
                  man_states.(i) <- Active (Bachelor {
                    bachelor_current_layer = layer;
                    bachelor_current_layer_distance = distance;
                    bachelor_next_layers = tail;
                  })
              | Seq.Cons ((layer, distance), next) ->
                  preferences.lad_previous_layers <-
                    (layer, distance) :: preferences.lad_previous_layers;
                  preferences.lad_current_layer <- layer;
                  preferences.lad_current_layer_distance <- distance;
                  preferences.lad_next_layers <- next)
          | remaining_women ->
              preferences.lad_current_layer <- remaining_women)
      | Some Bachelor preferences -> (
            match
              list_remove j preferences.bachelor_current_layer,
              preferences.bachelor_next_layers
            with
            | [], [] ->
                man_states.(i) <- Inactive;
            | [], (layer, distance) :: next_layers ->
                preferences.bachelor_current_layer <- layer;
                preferences.bachelor_current_layer_distance <- distance;
                preferences.bachelor_next_layers <- next_layers
            | remaining_women, _ ->
                preferences.bachelor_current_layer <- remaining_women)
      | None ->
          failwith "[remove_woman] should not be called with an old bachelor."
    in

    let ok = ref false in
    while not !ok do
      ok := true;
      for i = 0 to m - 1 do
        match man_states.(i) with
        | Active situation ->
          ok := false;
          let (j, d) = get_preferred_woman situation in
          if
            compatibility_test missing_fields.(i) added_fields.(j)
              && propose i j d
          then (
            (* [j] breaks off with her current fiance (if any). *)
            (match woman_states.(j) with
            | Engaged_woman (i', _) ->
                (* [i'] is an engaged man, so he has a situation. *)
                man_states.(i') <- Active (Option.get (man_situation i'));
                if not (is_uncertain i') then
                  remove_woman i' j
            | _ -> ());
            (* [i] and [j] become engaged. *)
            man_states.(i) <- Engaged_man situation;
            woman_states.(j) <- Engaged_woman (i, d))
          else
            remove_woman i j
        | _ -> ()
      done
    done;

    let actually_missing =
      missings
      |> List.filteri (fun i _ ->
        match man_states.(i) with
        | Engaged_man _ -> false
        | _ -> true)
      |> List.map (fun missing -> Suggest_add missing.item)
    in
    let name_changes = ref [] in
    for j = 0 to n - 1 do
      match woman_states.(j) with
      | Engaged_woman (i, _) ->
          let name_change =
            Suggest_rename {
              item_to_rename = added_fields.(j).item;
              suggested_ident = field_id missing_fields.(i);
            }
          in
          name_changes := name_change :: !name_changes
      | _ -> ()
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
          let expected_name = field_name expected_field in
          Misc.edit_distance
            expected_name
            (field_name added_field)
            (cutoff expected_name)
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
          let missing_id = field_id missing_field in
          let missing_name = Ident.name missing_id in
          match
            list_extract
              (fun added_field ->
                compute_distance missing_field added_field
                  < Misc.Maybe_infinite.Finite (cutoff missing_name))
              !remaining_added_fields
          with
          | None -> true
          | Some (added_field, additions) ->
              let name_change =
                Suggest_rename {
                  item_to_rename = added_field.item;
                  suggested_ident = missing_id;
                }
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
