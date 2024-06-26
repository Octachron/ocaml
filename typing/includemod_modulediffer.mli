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

module Suggestion : sig
  type alteration =
    | Add_item
    | Rename_item of Ident.t
    | Change_type_of_value of Types.type_expr

  type t = {
    subject : Types.signature_item;
    alteration : alteration;
  }

  val add : Types.signature_item -> t
  val rename : Types.signature_item -> Ident.t -> t
  val change_type_of_value : Types.signature_item -> Types.type_expr -> t

  val apply : Subst.t -> t -> Subst.t
end

val fuzzy_match_names :
  (('v, 't) field ->
  ('v, 't) field -> bool) ->
  ('v, 't) field list -> ('v, 't) field list ->
  Suggestion.t list

val compute_signature_diff :
  Env.t -> Subst.t ->
  Types.signature -> Types.signature ->
  Includemod.Error.signature_symptom option
