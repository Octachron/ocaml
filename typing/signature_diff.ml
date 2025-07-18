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

module AffectedItemSet = Set.Make (
    struct
      type t = Shape.Sig_component_kind.t * string
      let compare (x:t) (y:t) = Repr.compare x y
    end)

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
    subject : Types.signature_item;
    alteration : alteration;
  }

  let add subject = { subject; alteration = Add_item }
  let rename subject ident = { subject; alteration = Rename_item ident }

  let change_type_of_value subject ty =
    { subject; alteration = Change_type_of_value ty }
  let change_type_of_module subject mty =
    { subject; alteration = Change_type_of_module mty }
  let change_type_of_class subject cty =
    { subject; alteration = Change_type_of_class cty }
  let change_type subject ty = { subject; alteration = Change_type ty }
  let change_module_type subject mty =
    { subject; alteration = Change_module_type mty }

  let apply subst { subject; alteration } =
    match alteration, subject with
    | Rename_item suggested_ident, Sig_type (id, _, _, _) ->
        let path = Path.Pident id in
        Subst.add_type suggested_ident path subst
    | Rename_item suggested_ident, Sig_modtype (id, _, _) ->
        Subst.add_modtype suggested_ident (Path.Pident id) subst
    | _ -> subst
end



let compute_signature_diff env subst sig1 sig2 =
  match Includemod.signatures env ~subst ~mark:false sig1 sig2
  with
  | _ -> None
  | exception Includemod.(Error (_, Error.In_Signature reason)) -> Some reason

let is_modtype_eq (sgs : Includemod.Error.signature_symptom) got expected =
  let expected = Subst.modtype Keep sgs.subst expected in
  Includemod.is_modtype_equiv sgs.env got expected

module Field = struct
  open Stable_matching.Item
  let name item = Ident.name @@ Types.signature_item_id item
  let make item kind = { name = name item; kind; item }
end

let fuzzy_match_suggestions ~compatibility missing_fields added_fields =
  let { Stable_matching.missings; renamings } =
    Stable_matching.fuzzy_match_names ~compatibility missing_fields added_fields
  in
  List.map Suggestion.add missings @
  List.map (fun (x,y) -> Suggestion.rename x @@ Types.signature_item_id y)
    renamings

let compute_suggestions
    (sgs : Includemod.Error.signature_symptom)
    destructor
    ~compatibility
    incompatibility_destructor
=
  let missing_fields = List.filter_map destructor sgs.missings in
  let added_fields = List.filter_map destructor sgs.additions in
  let general_suggestions =
    fuzzy_match_suggestions ~compatibility missing_fields added_fields
  in
  let content_changes =
    List.filter_map incompatibility_destructor sgs.incompatibles
  in
  general_suggestions @ content_changes

type signature_symptom = Includemod.Error.signature_symptom

let module_suggestions (sgs:signature_symptom) =
  compute_suggestions sgs
    (function
      | Types.Sig_module (_, _, decl, _, _) as item ->
          Some (Field.make item decl.md_type)
      | _ -> None)
    ~compatibility:(is_modtype_eq sgs)
    (function
      | item, Module_type {expected; _} ->
          Some (Suggestion.change_type_of_module item expected)
      | _ -> None)

let type_suggestions (sgs:signature_symptom) =
  compute_suggestions sgs
    (function
      | Types.Sig_type (_,decl,_,_) as item -> Some (Field.make item decl)
      | _ -> None)
    ~compatibility:(Includemod.Item.type_declarations sgs.env sgs.subst)
    (function
      | item, Core (Type_declarations {expected; _}) ->
          Some (Suggestion.change_type item expected)
      | _ -> None)

let module_type_suggestions (sgs:signature_symptom) =
  let compatibility g e =  match g, e with
    | _, None -> true
    | None, Some _ -> false
    | Some g, Some e -> is_modtype_eq sgs g e
  in
  compute_suggestions sgs
    (function
      | Types.Sig_modtype (_, decl, _) as item ->
          Some (Field.make item decl.mtd_type)
      | _ -> None)
    ~compatibility
    (function
      | item, Module_type_declaration {expected; _} ->
          Some (Suggestion.change_module_type item expected)
      | _ -> None)

 let class_type_suggestions (sgs:signature_symptom) =
    compute_suggestions sgs
      (function
        | Types.Sig_class_type (_, decl, _, _) as item ->
            Some (Field.make item decl)
        | _ -> None)
      ~compatibility:(
        Includemod.Item.class_type_declarations sgs.env sgs.subst
      )
      (fun _ -> None)

let value_suggestions (sgs:signature_symptom) =
  compute_suggestions sgs
    (function
      | Types.Sig_value (_, desc, _) as item ->
          Some (Field.make item desc)
      | _ -> None)
    ~compatibility:(Includemod.Item.value_descriptions sgs.env sgs.subst)
    (function
      | item, Core (Value_descriptions {expected; _}) ->
          Some (Suggestion.change_type_of_value item expected.val_type)
      | _ -> None)

  let class_suggestions (sgs:signature_symptom) =
    compute_suggestions sgs
      (function
        | Types.Sig_class (_, decl, _, _) as item ->
            Some (Field.make item decl)
        | _ -> None)
      ~compatibility:(Includemod.Item.class_declarations sgs.env sgs.subst)
      (function
        | item, Core (Class_declarations {expected; _}) ->
            Some (Suggestion.change_type_of_class item expected)
        | _ -> None)

let compute_second_order_suggestions sgs =
  List.rev (
    class_type_suggestions sgs
    @ module_type_suggestions sgs
    @ type_suggestions sgs
    @ module_suggestions sgs)

let compute_first_order_suggestions sgs =
  List.rev (class_suggestions sgs @ value_suggestions sgs)

let rec iterate f previous_suggestions (sgs:signature_symptom) fuel =
  if fuel = 0 then (previous_suggestions, Some sgs) else
    let suggestions = f sgs in
    if List.is_empty suggestions then
      previous_suggestions, Some sgs
    else
      let subst = List.fold_left Suggestion.apply sgs.subst suggestions in
      let suggestions = suggestions @ previous_suggestions in
      match compute_signature_diff sgs.env subst sgs.sig1 sgs.sig2 with
      | None -> suggestions, None
      | Some sgs -> iterate f suggestions sgs (fuel - 1)

let deduplicate suggestions =
  let add_item (unique_suggestions, affected_items) suggestion =
    let key =
      let kind, ident, _ =
        Types.classify_signature_item suggestion.Suggestion.subject
      in
      kind, Ident.name ident
    in
    if AffectedItemSet.mem key affected_items then
      unique_suggestions, affected_items
    else
      (suggestion :: unique_suggestions, AffectedItemSet.add key affected_items)
  in
  let suggestions, _ =
    List.fold_left add_item ([], AffectedItemSet.empty) suggestions
  in
  suggestions

let suggest sgs =
  let all_suggestions =
    let type_suggestions, remaining =
      iterate compute_second_order_suggestions [] sgs 5
    in
    match remaining with
    | None -> type_suggestions
    | Some remaining ->
        let value_suggestions = compute_first_order_suggestions remaining in
        type_suggestions @ value_suggestions
  in
 deduplicate all_suggestions
