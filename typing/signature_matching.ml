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
  open Stable_matching.Item
  let name item = Ident.name @@ Types.signature_item_id item
  let make item kind = { name = name item; kind; item }
  let item x = x.item
end

module Suggestion = struct
  type alteration =
    | Missing_item
    | Possible_match of Ident.t Location.loc

  type 'a t = {
    subject : Types.signature_item;
    alteration : 'a;
  }

  type report = {
    alterations: alteration t list;
    incompatibles: Includemod.Error.sigitem_symptom t list
  }

  let missing subject =
    { subject = Field.item subject; alteration = Missing_item }
  let possible_match (left, right) =
    let _ , id, loc = Types.classify_signature_item left in
    { subject = right; alteration = Possible_match { Location.txt = id; loc } }
  let incompatible (subject, symptom) = { subject; alteration=symptom }

  let apply_renaming subst (left, right) =
    Includemod.item_subst (Types.signature_item_id left) right subst
end

let max_right_items = 20
let cutoff name =
  match String.length name with
  | 0 | 1 -> 0 (* Proposing to rename "x" to "y" is dubious. *)
  | 2 | 3 | 4 -> 1
  | 5 | 6 | 7 | 8 -> 2
  | 9 | 10 | 11 -> 3
  | len -> len/4

let fuzzy_match_suggestions env compatibility ~subst current =
  let open Stable_matching in
  let compatibility = compatibility env subst in
  let matches =
    fuzzy_match_names ~max_right_items ~cutoff ~compatibility
      current.left current.right
  in
  match matches.pairs with
  | [] -> false, subst, current
  | pairs ->
      let subst = List.fold_left Suggestion.apply_renaming subst pairs in
      true, subst, { matches with pairs = pairs @ current.pairs }

type ('a,'b,'c,'d,'v,'cl,'ext) type_kind_map =
  {
    module_types: 'a;
    modules: 'b;
    types: 'c;
    class_types: 'd;
    values: 'v;
    classes: 'cl;
    extensions: 'ext
  }

let empty = {
  module_types = [];
  modules = [];
  types = [];
  class_types = [];
  values = [];
  classes = [];
  extensions = [];
}

let classify map = function
  | Types.Sig_module (_, _, decl, _, _) as item ->
      { map with modules = Field.make item decl :: map.modules }
  | Types.Sig_type (_,decl,_,_) as item ->
      { map with types = Field.make item decl :: map.types }
  | Types.Sig_modtype (_, decl, _) as item ->
      let module_types = Field.make item decl :: map.module_types in
      { map with module_types }
  | Types.Sig_class_type (_, decl, _, _) as item ->
      { map with class_types = Field.make item decl :: map.class_types }
  | Types.Sig_value (_, desc, _) as item ->
      { map with values = Field.make item desc :: map.values }
  | Types.Sig_class (_, decl, _, _) as item ->
      { map with classes = Field.make item decl :: map.classes }
  | Types.Sig_typext (_,decl,_,_) as item ->
      { map with extensions = Field.make item decl :: map.extensions }

let init (sgs:Includemod.Error.signature_symptom) =
  let left = List.fold_left classify empty sgs.additions in
  let right = List.fold_left classify empty sgs.missings in
  let init_kind proj =
    { Stable_matching.left = proj left; pairs = []; right = proj right }
  in
  sgs.env, sgs.subst,
  {
    module_types = init_kind (fun x -> x.module_types);
    modules = init_kind (fun x -> x.modules);
    types = init_kind (fun x -> x.types);
    class_types = init_kind (fun x -> x.class_types);
    values = init_kind (fun x -> x.values);
    classes = init_kind (fun x -> x.classes);
    extensions = init_kind (fun x -> x.extensions)
  }

module C = Includemod.Check

let rec iterate env subst lim map =
  let fuzzy_match c ~subst items = fuzzy_match_suggestions env c ~subst items in
  let pmty, subst, module_types =
    fuzzy_match C.module_types ~subst map.module_types in
  let pm, subst, modules = fuzzy_match C.modules ~subst map.modules in
  let pty, subst, types = fuzzy_match C.types ~subst map.types in
  let progress = pmty || pm || pty in
  if progress && lim > 0 then
    iterate env subst (lim-1)
      { map with module_types; modules; types }
  else subst, map

let value_suggestions env subst map =
  let fuzzy_match compat current =
    let open Stable_matching in
    let compatibility x y = compat env subst x y in
    fuzzy_match_names ~max_right_items ~cutoff ~compatibility
      current.left current.right
  in
  let values = fuzzy_match C.values map.values in
  let classes = fuzzy_match C.classes map.classes in
  let class_types = fuzzy_match C.class_types map.class_types in
  let extensions = fuzzy_match C.extensions map.extensions in
  { map with values; classes; class_types; extensions }

let suggest
    (sgs : Includemod.Error.signature_symptom)
=
  let env, subst, start = init sgs in
  let subst, with_types = iterate env subst 6 start in
  let all = value_suggestions env subst with_types in
  let collect proj l =
    let km: _ Stable_matching.matches = proj all in
    List.map Suggestion.possible_match km.pairs
    @ List.rev_map Suggestion.missing km.right
    @ l
  in
  {
    Suggestion.incompatibles =
      List.map Suggestion.incompatible sgs.incompatibles;
    alterations =
      []
      |> collect (fun x -> x.module_types)
      |> collect (fun x -> x.modules)
      |> collect (fun x -> x.types)
      |> collect (fun x -> x.class_types)
      |> collect (fun x -> x.classes)
      |> collect (fun x -> x.values)
      |> collect (fun x -> x.extensions)
  }
