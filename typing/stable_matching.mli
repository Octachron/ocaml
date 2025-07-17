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


module Trie : sig
  type 'a t

  val add: 'a t -> string -> 'a -> unit
  val of_seq: (string * 'a) Seq.t -> 'a t

  val compute_preferences:
    ?deletion_cost:int ->
    ?insertion_cost:int ->
    ?substitution_cost:int ->
    ?cutoff:int ->
    'a t ->
    string ->
    ('a * int) Seq.t
  (** Returns a sequence that yields the data associated with the strings of the
      trie, together with the distance to the specified string, in order from
      closest to farthest.

      Each node of the sequence should be called at most once. *)

  val compute_preference_layers:
    ?deletion_cost:int ->
    ?insertion_cost:int ->
    ?substitution_cost:int ->
    ?cutoff:int ->
    ?max_elements:int ->
    'a t ->
    string ->
    ('a list * int) Seq.t
  (** Returns a sequence that yields the data associated with the strings of the
      trie, groupped by distance to the specified string, together with said
      distance, in order from closest to farthest.

      Each node of the sequence should be called at most once. *)
end

module Field: sig
  type ('v, 't) t = {
    item : Types.signature_item;
    value : 'v;
    type_ : 't;
  }
  val ident: ('v,'t) t -> Ident.t
  val first_order: Types.signature_item -> 'v -> 't -> ('v,'t) t
  val second_order: Types.signature_item -> 'v -> ('v,unit) t
end

val fuzzy_match_names:
  ( (('a,'b) Field.t as 'f) -> 'f -> bool) -> 'f list -> 'f list
  -> 'f list * ('f * 'f) list
