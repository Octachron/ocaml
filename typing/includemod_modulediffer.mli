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

module Field : sig
  type ('v, 't) t = {
    item : Types.signature_item;
    value : 'v;
    type_ : 't;
  }

  val first_order : Types.signature_item -> 'v -> 't -> ('v, 't) t
  val second_order : Types.signature_item -> 'v -> ('v, unit) t

  val name : ('v, 't) t -> string
end

module Suggestion : sig
  type alteration =
    | Add_item
    | Rename_item of Ident.t
    | Change_type_of_value of Types.type_expr
    | Change_type of Types.type_declaration

  type t = {
    subject : Types.signature_item;
    alteration : alteration;
  }

  val add : Types.signature_item -> t
  val rename : Types.signature_item -> Ident.t -> t
  val change_type_of_value : Types.signature_item -> Types.type_expr -> t
  val change_type : Types.signature_item -> Types.type_declaration -> t

  val apply : Subst.t -> t -> Subst.t
end

val fuzzy_match_names :
  (('v, 't) Field.t ->
  ('v, 't) Field.t -> bool) ->
  ('v, 't) Field.t list -> ('v, 't) Field.t list ->
  Suggestion.t list

val compute_signature_diff :
  Env.t -> Subst.t ->
  Types.signature -> Types.signature ->
  Includemod.Error.signature_symptom option
