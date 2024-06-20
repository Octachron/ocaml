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

val field_name : ('v, 't) field -> string

type suggestion =
  | Suggest_add of Types.signature_item
  | Suggest_rename of Types.signature_item * string
  | Suggest_change_value_type of Ident.t * Types.type_expr

val fuzzy_match_names :
  (('v, 't) field ->
  ('v, 't) field -> bool) ->
  ('v, 't) field list -> ('v, 't) field list ->
  suggestion list

val compute_signature_diff :
  Env.t -> Subst.t ->
  Types.signature -> Types.signature ->
  Includemod.Error.signature_symptom option
