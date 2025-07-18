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



let compute_signature_diff env subst sig1 sig2 =
  try
    let _ = Includemod.signatures env ~subst ~mark:false sig1 sig2 in
    None
  with
  | Includemod.Error (_, Includemod.Error.In_Signature reason) -> Some reason

let is_modtype_eq (sgs : Includemod.Error.signature_symptom) got expected =
  let expected = Subst.modtype Keep sgs.subst expected in
  Includemod.is_modtype_equiv sgs.env got expected

module Field = struct
  open Stable_matching.Item
  let name item = Ident.name @@ Types.signature_item_id item
  let make item kind = { name = name item; kind; item }
end

let compute_suggestions
    (sgs : Includemod.Error.signature_symptom)
    destructor
    ~compatibility
    incompatibility_destructor
=
  let missing_fields = List.filter_map destructor sgs.missings in
  let added_fields = List.filter_map destructor sgs.additions in

  let { Stable_matching.missings; renamings } =
    Stable_matching.fuzzy_match_names ~compatibility
      missing_fields added_fields
  in
  let general_suggestions =
    List.map Suggestion.add missings @
    List.map (fun (x,y) -> Suggestion.rename x @@ Types.signature_item_id y)
      renamings
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
            Some (Field.make item decl.md_type)
        | _ -> None)
      ~compatibility:(is_modtype_eq sgs)
      (function
        | item, Module_type {expected; _} ->
            Some (Suggestion.change_type_of_module item expected)
        | _ -> None)
  in

  let type_suggestions =
    compute_suggestions
      sgs
      (function
        | Types.Sig_type (_,decl,_,_) as item -> Some (Field.make item decl)
        | _ -> None)
      ~compatibility:(Includemod.Item.type_declarations sgs.env sgs.subst)
      (function
        | item, Core (Type_declarations {expected; _}) ->
            Some (Suggestion.change_type item expected)
        | _ -> None)
  in

  let module_type_suggestions =
    let compatibility g e =  match g, e with
      | _, None -> true
      | None, Some _ -> false
      | Some g, Some e -> is_modtype_eq sgs g e
    in
    compute_suggestions
      sgs
      (fun item ->
        match item with
        | Types.Sig_modtype (_, decl, _) ->
            Some (Field.make item decl.Types.mtd_type)
        | _ -> None)
      ~compatibility
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
            Some (Field.make item decl)
        | _ -> None)
      ~compatibility:(
        Includemod.Item.class_type_declarations sgs.env sgs.subst
      )
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
            Some (Field.make item desc)
        | _ -> None)
      ~compatibility:(Includemod.Item.value_descriptions sgs.env sgs.subst)
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
            Some (Field.make item decl)
        | _ -> None)
      ~compatibility:(Includemod.Item.class_declarations sgs.env sgs.subst)
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
