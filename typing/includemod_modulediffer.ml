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

module Field = struct
  type ('v, 't) t = {
    item : Types.signature_item;
    value : 'v;
    type_ : 't;
  }

  let first_order item value type_ = {
    item;
    value;
    type_;
  }

  let second_order item value = {
    item;
    value;
    type_ = ();
  }

  let ident field = Types.signature_item_id field.item
  let name field = Ident.name (ident field)
end

module Suggestion = struct
  type alteration =
    | Add_item
    | Rename_item of Ident.t
    | Change_type_of_value of Types.type_expr
    | Change_type of Types.type_declaration

  type t = {
    subject : Types.signature_item;
    alteration : alteration;
  }

  let add item = {
    subject = item;
    alteration = Add_item;
  }

  let rename item ident = {
    subject = item;
    alteration = Rename_item ident;
  }

  let change_type_of_value item ty = {
    subject = item;
    alteration = Change_type_of_value ty;
  }

  let change_type item ty = {
    subject = item;
    alteration = Change_type ty;
  }

  let apply subst suggestion =
    match suggestion with
    | {
      subject = Sig_type (id, _, _, _);
      alteration = Rename_item suggested_ident;
    } ->
        let path = Path.Pident id in
        Subst.add_type suggested_ident path subst
    | _ -> subst
end

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
        mutable lad_next_layers : (int list * distance) Seq.t;
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
        |> List.mapi (fun j field -> (Field.name field, j))
        |> Misc.Trie.of_list
      in
      Array.init m (fun i ->
        let missing_name = Field.name missing_fields.(i) in
        let sequence =
          Misc.Trie.compute_preference_layers
            ~cutoff:(cutoff missing_name)
            ~max_elements:75
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
      |> List.map (fun missing -> Suggestion.add missing.Field.item)
    in
    let name_changes = ref [] in
    for j = 0 to n - 1 do
      match woman_states.(j) with
      | Engaged_woman (i, _) ->
          let name_change =
            Suggestion.rename
              added_fields.(j).Field.item
              (Field.ident missing_fields.(i))
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
          let expected_name = Field.name expected_field in
          Misc.edit_distance
            expected_name
            (Field.name added_field)
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
          let missing_id = Field.ident missing_field in
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
                Suggestion.rename added_field.Field.item missing_id
              in
              name_changes := name_change :: !name_changes;
              remaining_added_fields := additions;
              false)
      |> List.map (fun missing -> Suggestion.add missing.Field.item)
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

module ItemId = struct
  type item_kind =
    | Value
    | Module
    | Class
    | Type
    | Module_type
    | Class_type
    | Type_ext

  type t = item_kind * string

  let of_item item =
    let open Types in
    match item with
    | Sig_value (id, _, _) -> Value, Ident.name id
    | Sig_type (id, _, _, _) -> Type, Ident.name id
    | Sig_typext (id, _, _, _) -> Type_ext, Ident.name id
    | Sig_module (id, _, _, _, _) -> Module, Ident.name id
    | Sig_modtype (id, _, _) -> Module_type, Ident.name id
    | Sig_class (id, _, _, _) -> Class, Ident.name id
    | Sig_class_type (id, _, _, _) -> Class_type, Ident.name id

  let compare = compare
end
module AffectedItemSet = Set.Make (ItemId)

let direction = Includemod.Directionality.any
let is_modtype_eq get (sgs : Includemod.Error.signature_symptom) gotten expected =
  let expected = Subst.modtype Keep sgs.subst (get expected) in
  Includemod.is_modtype_equiv
    sgs.env
    (get gotten)
    expected

let compute_suggestions
    (sgs : Includemod.Error.signature_symptom)
    destructor
    compatibility_test
    incompatibility_destructor
=
  let missing_fields = List.filter_map destructor sgs.missings in
  let added_fields = List.filter_map destructor sgs.additions in

  let general_suggestions =
    fuzzy_match_names compatibility_test missing_fields added_fields
  in

  let content_changes =
    List.filter_map incompatibility_destructor sgs.incompatibles
  in

  general_suggestions @ content_changes

let compute_second_order_suggestions sgs =
  let open Includemod.Error in

  let type_suggestions =
    compute_suggestions
      sgs
      (fun item ->
        match item with
        | Types.Sig_type  (_, decl, _, _) ->
            Some (Field.second_order item decl)
        | _ -> None)
      (fun expected gotten ->
        let id, loc, _ = Includemod.item_ident_name gotten.item in
        match
          Includemod.Core_inclusion.type_declarations
            ~direction ~loc
            sgs.env sgs.subst id
            gotten.value expected.value
        with
        | Ok _ -> true
        | Error _ -> false)
      (function
        | item, Core (Type_declarations {expected; _}) ->
          Some (Suggestion.change_type item expected)
        | _ -> None)
  in

  let module_type_suggestions =
    let get x = Option.get x.Field.value.Types.mtd_type in
    compute_suggestions
      sgs
      (fun item ->
        match item with
        | Types.Sig_modtype (_, decl, _) ->
            Some (Field.second_order item decl)
        | _ -> None)
      (is_modtype_eq get sgs)
      (fun _ -> None)
  in

  let class_type_suggestions =
    compute_suggestions
      sgs
      (fun item ->
        match item with
        | Types.Sig_class_type (_, decl, _, _) ->
            Some (Field.second_order item decl)
        | _ -> None)
      (fun expected gotten ->
        let id, loc, _ = Includemod.item_ident_name gotten.Field.item in
        match
          Includemod.Core_inclusion.class_type_declarations
            ~loc ~direction sgs.env sgs.subst
            id gotten.Field.value expected.Field.value
        with
        | Ok _ -> true
        | Error _ -> false)
      (fun _ -> None)
  in

  List.rev (class_type_suggestions @ module_type_suggestions @ type_suggestions)

let compute_first_order_suggestions sgs =
  let open Includemod.Error in

  let value_suggestions =
    compute_suggestions
      sgs
      (fun item ->
        match item with
        | Types.Sig_value (_, desc, _) ->
            Some (Field.first_order item desc desc.val_type)
        | _ -> None)
      (fun expected gotten ->
        let id, loc, _ = Includemod.item_ident_name gotten.Field.item in
        match
          Includemod.Core_inclusion.value_descriptions
            ~direction ~loc
            sgs.env sgs.subst id
            gotten.value expected.value
        with
        | Ok _ -> true
        | Error _ -> false)
      (function
        | item, Core (Value_descriptions {expected; _}) ->
          Some (Suggestion.change_type_of_value item expected.val_type)
        | _ -> None)
  in

  let module_suggestions =
    compute_suggestions
      sgs
      (fun item ->
        match item with
        | Types.Sig_module (_, _, decl, _, _) ->
            Some (Field.first_order item decl decl.md_type)
        | _ -> None)
      (is_modtype_eq (fun x -> x.Field.type_) sgs)
      (fun _ -> None)
  in

  let class_suggestions =
    compute_suggestions
      sgs
      (fun item ->
        match item with
        | Types.Sig_class (_, decl, _, _) ->
            Some (Field.first_order item decl decl.cty_type)
        | _ -> None)
      (fun expected gotten ->
        match
          let id, loc, _ = Includemod.item_ident_name gotten.Field.item in
          Includemod.Core_inclusion.class_declarations
            sgs.env sgs.subst ~loc ~direction
            id expected.value gotten.value
        with
        | Ok _ -> true
        | Error _ -> false)
      (fun _ -> None)
  in

  List.rev (class_suggestions @ module_suggestions @ value_suggestions)

let compute_suggestions sgs passes =
  let open Includemod.Error in

  let rec iterate f sgs i =
    if i = 0 then
      [], sgs
    else
      let suggestions = f sgs in

      let subst = List.fold_left Suggestion.apply sgs.subst suggestions in

      match compute_signature_diff sgs.env subst sgs.sig1 sgs.sig2 with
      | None ->
          suggestions, sgs
      | Some sgs' ->
          let new_suggestions, _ = iterate f sgs' (i - 1) in
          new_suggestions @ suggestions, sgs'
  in

  let second_order_suggestions, sgs' =
    iterate compute_second_order_suggestions sgs passes
  in
  let first_order_suggestions, _ =
    iterate compute_first_order_suggestions sgs' passes
  in

  second_order_suggestions @ first_order_suggestions
  |> List.fold_left
    (fun (acc, affected_items) suggestion ->
      let id = ItemId.of_item suggestion.Suggestion.subject in
      if AffectedItemSet.mem id affected_items then
        acc, affected_items
      else
        (suggestion :: acc, AffectedItemSet.add id affected_items))
    ([], AffectedItemSet.empty)
  |> fst
