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

module Item_id = struct
  type item_kind =
    | Value
    | Module
    | Class
    | Type
    | Module_type
    | Class_type
    | Type_ext

  type t = item_kind * string

  let kind_of_item item =
    let open Types in
    match item with
    | Sig_value (_, _, _) -> Value
    | Sig_type (_, _, _, _) -> Type
    | Sig_typext (_, _, _, _) -> Type_ext
    | Sig_module (_, _, _, _, _) -> Module
    | Sig_modtype (_, _, _) -> Module_type
    | Sig_class (_, _, _, _) -> Class
    | Sig_class_type (_, _, _, _) -> Class_type

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
module AffectedItemSet = Set.Make (Item_id)

module Suggestion = struct
  type alteration =
    | Add_item
    | Rename_item of Ident.t
    | Change_type_of_value of Types.type_expr
    | Change_type_of_module of Types.module_type
    | Change_type_of_class of Types.class_declaration
    | Change_type of Types.type_declaration
    | Change_module_type of Types.modtype_declaration

  type t = {
    affects : Item_id.t;
    subject : Types.signature_item;
    alteration : alteration;
  }

  let add item = {
    affects = Item_id.of_item item;
    subject = item;
    alteration = Add_item;
  }

  let rename item ident = {
    affects = Item_id.kind_of_item item, Ident.name ident;
    subject = item;
    alteration = Rename_item ident;
  }

  let change_type_of_value item ty = {
    affects = Item_id.of_item item;
    subject = item;
    alteration = Change_type_of_value ty;
  }

  let change_type_of_module item mty = {
    affects = Item_id.of_item item;
    subject = item;
    alteration = Change_type_of_module mty;
  }

  let change_type_of_class item cty = {
    affects = Item_id.of_item item;
    subject = item;
    alteration = Change_type_of_class cty;
  }

  let change_type item ty = {
    affects = Item_id.of_item item;
    subject = item;
    alteration = Change_type ty;
  }

  let change_module_type item mty = {
    affects = Item_id.of_item item;
    subject = item;
    alteration = Change_module_type mty;
  }

  let apply subst suggestion =
    match suggestion with
    | {
      subject = Sig_type (id, _, _, _);
      alteration = Rename_item suggested_ident;
    } ->
        let path = Path.Pident id in
        Subst.add_type suggested_ident path subst
    | {
      subject = Sig_modtype (id, _, _);
      alteration = Rename_item suggested_ident;
    } ->
        Subst.add_modtype suggested_ident (Path.Pident id) subst
    | _ -> subst
end

type 'a diff = {
  delete : 'a list;
  add : 'a list;
  substitute : ('a * 'a) list;
}

let reverse_diff d =
  {
    delete = d.add;
    add = d.delete;
    substitute = List.map (fun (right, left) -> (left, right)) d.substitute;
  }

(** An implementation (in [diff]) of Zoltan Kiraly's "New Algorithm," presented
    in "Linear Time Local Approximation Algorithm for Maximum Stable Marriage":
    https://www.mdpi.com/1999-4893/6/3/471. It computes a 3/2-approximation of
    a maximum stable marriage in linear time (linear in the sum of the lengths
    of the preference lists). *)
module Stable_marriage_diff = struct
  let rec list_remove x list =
    match list with
    | [] -> []
    | hd :: tl when hd = x -> tl
    | hd :: tl -> hd :: list_remove x tl

  (* This implementation does not use the same semantics as the original paper.
     Below is a conversion from the paper's terms to the implementation's terms:
     - woman: left
     - man: right
     - engaged (woman / man): paired
     - maiden (woman) / active (man): available
     - lad: first phase
     - bachelor: second phase
     - old bachelor: closed
     - uncertain (man): has a better choice
     - flighty (woman): has a weak pair *)

  type distance = int

  type 'a preferences = {
    mutable previous_layers : 'a;
        (** Invariant: if this is a list, it is not empty. *)
    mutable current_layer : int list; (** Invariant: this list is not empty. *)
    mutable current_layer_distance : distance;
    mutable next_layers : (int list * distance) Seq.t;
  }

  type left_state =
    | Left_available
    | Left_paired of int * distance

  type right_phase =
    | First_phase of (int list * distance) list preferences
    | Second_phase of unit preferences

  type right_state =
    | Right_available of right_phase
    | Right_paired of right_phase
    | Right_closed

  let rec diff
    ~cutoff ?max_elements ~compatibility_test
    left right
  =
    let n = Array.length left in
    let m = Array.length right in

    if m > n then
      diff
        ~cutoff ?max_elements
        ~compatibility_test:(fun a b -> compatibility_test b a)
        right left
      |> reverse_diff
    else

    let left_states = Array.make n Left_available in
    let right_states =
      let left_trie =
        left
        |> Array.to_seq
        |> Seq.mapi (fun j field -> (Field.name field, j))
        |> Misc.Trie.of_seq
      in
      Array.map
        (fun right_field ->
          let name = Field.name right_field in
          let sequence =
            Misc.Trie.compute_preference_layers
              ~cutoff:(cutoff name)
              ?max_elements
              left_trie
              name
          in
          match sequence () with
          | Seq.Nil ->
              Right_closed
          | Seq.Cons ((layer, distance), tail) ->
              Right_available (First_phase {
                previous_layers = [ (layer, distance) ];
                current_layer = layer;
                current_layer_distance = distance;
                next_layers = tail;
              }))
        right
    in

    let has_better_choice i =
      match right_states.(i) with
      | Right_paired
        (First_phase {current_layer; _}
        | Second_phase {current_layer; _}) ->
          List.exists (fun j -> left_states.(j) = Left_available) current_layer
      | _ -> false
    in

    let has_weak_pair j =
      match left_states.(j) with
      | Left_paired (i, _) -> has_better_choice i
      | _ -> false
    in

    let phase i =
      match right_states.(i) with
      | Right_available phase | Right_paired phase -> Some phase
      | Right_closed -> None
    in

    let get_preferred_candidate phase =
      match phase with
      | First_phase {current_layer; current_layer_distance; _}
      | Second_phase {current_layer; current_layer_distance; _} ->
          current_layer
          |> List.find_opt (fun j -> left_states.(j) = Left_available)
          |> Option.value ~default:(List.hd current_layer),
          current_layer_distance
    in

    let propose i j d =
      has_weak_pair j ||
      match left_states.(j) with
      | Left_available -> true
      | Left_paired (i', d') ->
          d < d' ||
          d = d' &&
            match phase i, phase i' with
            | Some Second_phase _, Some First_phase _ -> true
            | _ -> false
    in

    let create_second_phase layers =
      match layers with
      | [] -> assert false
      | (layer, distance) :: tail ->
          Second_phase {
            previous_layers = ();
            current_layer = layer;
            current_layer_distance = distance;
            next_layers = List.to_seq tail;
          }
    in

    let remove_left_from_preferences preferences j nil_callback cons_callback =
      match list_remove j preferences.current_layer with
      | [] -> (
          match preferences.next_layers () with
          | Seq.Nil ->
              nil_callback ()
          | Seq.Cons ((layer, distance), next) ->
              preferences.current_layer <- layer;
              preferences.current_layer_distance <- distance;
              preferences.next_layers <- next;
              cons_callback layer distance)
      | remaining_elements ->
          preferences.current_layer <- remaining_elements
    in

    let remove_left i j =
      match phase i with
      | Some First_phase preferences ->
          remove_left_from_preferences
            preferences
            j
            (fun () ->
              let phase = create_second_phase preferences.previous_layers in
              right_states.(i) <- Right_available phase)
            (fun layer distance ->
              preferences.previous_layers <-
                (layer, distance) :: preferences.previous_layers)
      | Some Second_phase preferences ->
          remove_left_from_preferences
            preferences
            j
            (fun () -> right_states.(i) <- Right_closed)
            (fun _ _ -> ())
      | None ->
          assert false
    in

    let ok = ref false in
    while not !ok do
      ok := true;
      for i = 0 to m - 1 do
        match right_states.(i) with
        | Right_available right_phase ->
            ok := false;
            let (j, d) = get_preferred_candidate right_phase in
            if compatibility_test right.(i) left.(j) && propose i j d then (
              (* Unpair [j]. *)
              (match left_states.(j) with
              | Left_paired (i', _) ->
                  (* [i'] is paired, so it has a phase. *)
                  right_states.(i') <- Right_available (Option.get (phase i'));
                  if not (has_better_choice i') then
                    remove_left i' j
              | _ -> ());
              (* Pair [i] and [j]. *)
              right_states.(i) <- Right_paired right_phase;
              left_states.(j) <- Left_paired (i, d))
            else
              remove_left i j
        | _ -> ()
      done
    done;

    {
      delete =
        left_states
        |> Array.to_seq
        |> Seq.zip (Array.to_seq left)
        |> Seq.filter_map (function
          | left_field, Left_available -> Some left_field
          | _, Left_paired _ -> None)
        |> List.of_seq;

      add =
        right
        |> Array.to_list
        |> List.filteri (fun i _ ->
          match right_states.(i) with
          | Right_paired _ -> false
          | _ -> true);

      substitute =
        left_states
        |> Array.to_seq
        |> Seq.zip (Array.to_seq left)
        |> Seq.filter_map (function
          | left_field, Left_paired (i, _) -> Some (left_field, right.(i))
          | _, Left_available -> None)
        |> List.of_seq;
    }
end

let greedy_matching ~compatibility_test ~cutoff missings additions =
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

let fuzzy_match_names compatibility_test missings additions =
  (* The edit distance between an existing name and a suggested rename must be
     at most half the length of the name. *)
  let cutoff name = String.length name / 2 in
  let m = List.length missings in
  let n = List.length additions in

  if m < 60 && n < 60 then
    (* Stable marriages. *)
    let diff =
      Stable_marriage_diff.diff
        ~cutoff ~max_elements:10 ~compatibility_test
        (Array.of_list additions)
        (Array.of_list missings)
    in

    let missings =
      List.map (fun field -> Suggestion.add field.Field.item) diff.add
    in

    let name_changes =
      List.map
        (fun (got, expected) ->
          Suggestion.rename got.Field.item (Field.ident expected))
        diff.substitute
    in

    missings @ name_changes

  else
    (* Greedy. *)
    greedy_matching ~compatibility_test ~cutoff missings additions

let compute_signature_diff env subst sig1 sig2 =
  try
    let _ = Includemod.signatures env ~subst ~mark:false sig1 sig2 in
    None
  with
  | Includemod.Error (_, Includemod.Error.In_Signature reason) -> Some reason

let is_modtype_eq get (sgs : Includemod.Error.signature_symptom)
    got expected =
  let expected = Subst.modtype Keep sgs.subst (get expected) in
  Includemod.is_modtype_equiv
    sgs.env
    (get got)
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

  let module_suggestions =
    compute_suggestions
      sgs
      (fun item ->
        match item with
        | Types.Sig_module (_, _, decl, _, _) ->
            Some (Field.first_order item decl decl.md_type)
        | _ -> None)
      (is_modtype_eq (fun x -> x.Field.type_) sgs)
      (function
        | item, Module_type {expected; _} ->
            Some (Suggestion.change_type_of_module item expected)
        | _ -> None)
  in

  let type_suggestions =
    compute_suggestions
      sgs
      (fun item ->
        match item with
        | Types.Sig_type  (_, decl, _, _) ->
            Some (Field.second_order item decl)
        | _ -> None)
      (fun expected got ->
        let id, loc, _ = Includemod.item_ident_name got.item in
        match
          Includemod.Item.type_declarations
           ~loc
            sgs.env sgs.subst id
            got.value expected.value
        with
        | Ok _ -> true
        | Error _ -> false)
      (function
        | item, Core (Type_declarations {expected; _}) ->
            Some (Suggestion.change_type item expected)
        | _ -> None)
  in

  let module_type_suggestions =
    let get x = x.Field.value.Types.mtd_type in
    let compare e g =  match get g, get e with
      | _, None -> true
      | None, Some _ -> false
      | Some g, Some e ->
          is_modtype_eq Fun.id sgs e g
    in
    compute_suggestions
      sgs
      (fun item ->
        match item with
        | Types.Sig_modtype (_, decl, _) ->
            Some (Field.second_order item decl)
        | _ -> None)
      compare
      (function
        | item, Module_type_declaration {expected; _} ->
            Some (Suggestion.change_module_type item expected)
        | _ -> None)
  in

  let class_type_suggestions =
    compute_suggestions
      sgs
      (fun item ->
        match item with
        | Types.Sig_class_type (_, decl, _, _) ->
            Some (Field.second_order item decl)
        | _ -> None)
      (fun expected got ->
        let id, loc, _ = Includemod.item_ident_name got.Field.item in
        match
          Includemod.Item.class_type_declarations
            ~loc sgs.env sgs.subst
            id got.Field.value expected.Field.value
        with
        | Ok _ -> true
        | Error _ -> false)
      (fun _ -> None)
  in

  List.rev (
    class_type_suggestions
    @ module_type_suggestions
    @ type_suggestions
    @ module_suggestions)

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
      (fun expected got ->
        let id, loc, _ = Includemod.item_ident_name got.Field.item in
        match
          Includemod.Item.value_descriptions
            ~loc sgs.env sgs.subst
            id got.value expected.value
        with
        | Ok _ -> true
        | Error _ -> false)
      (function
        | item, Core (Value_descriptions {expected; _}) ->
            Some (Suggestion.change_type_of_value item expected.val_type)
        | _ -> None)
  in

  let class_suggestions =
    compute_suggestions
      sgs
      (fun item ->
        match item with
        | Types.Sig_class (_, decl, _, _) ->
            Some (Field.first_order item decl decl.cty_type)
        | _ -> None)
      (fun expected got ->
        match
          let id, loc, _ = Includemod.item_ident_name got.Field.item in
          Includemod.Item.class_declarations
            sgs.env sgs.subst ~loc
            id expected.value got.value
        with
        | Ok _ -> true
        | Error _ -> false)
      (function
        | item, Core (Class_declarations {expected; _}) ->
            Some (Suggestion.change_type_of_class item expected)
        | _ -> None)
  in

  List.rev (class_suggestions @ value_suggestions)

let suggest sgs =
  let open Includemod.Error in

  let rec iterate f sgs fioul =
    if fioul = 0 then ([], Some sgs) else

    let suggestions = f sgs in

    if List.is_empty suggestions then
      (suggestions, Some sgs)
    else
      let subst = List.fold_left Suggestion.apply sgs.subst suggestions in
      match compute_signature_diff sgs.env subst sgs.sig1 sgs.sig2 with
      | None ->
          (suggestions, None)
      | Some sgs' ->
          let new_suggestions, sgs'' = iterate f sgs' (fioul - 1) in
          (new_suggestions @ suggestions, sgs'')
  in

  let all_suggestions =
    match iterate compute_second_order_suggestions sgs 5 with
    | second_order_suggestions, None ->
        second_order_suggestions
    | second_order_suggestions, Some sgs' ->
        let first_order_suggestions = compute_first_order_suggestions sgs' in
        second_order_suggestions @ first_order_suggestions
  in

  all_suggestions
  |> List.fold_left
    (fun (acc, affected_items) suggestion ->
      if AffectedItemSet.mem suggestion.Suggestion.affects affected_items then
        acc, affected_items
      else
        (suggestion :: acc,
        AffectedItemSet.add suggestion.Suggestion.affects affected_items))
    ([], AffectedItemSet.empty)
  |> fst
