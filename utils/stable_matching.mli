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

type cost_model = {
  insertion:int;
  deletion:int;
  substitution:int;
}

module Trie : sig
  type 'a t

  val add: 'a t -> string -> 'a -> unit
  val of_seq: (string * 'a) Seq.t -> 'a t

  val compute_preferences:
    cost_model -> ?cutoff:int -> 'a t -> string -> ('a * int) Seq.t
  (** Returns a sequence that yields the data associated with the strings of the
      trie, together with the distance to the specified string, in order from
      closest to farthest.

      Each node of the sequence should be called at most once. *)

  val compute_preference_layers:
    ?cost:cost_model -> ?cutoff:int -> ?max_elements:int
    -> 'a t -> string -> ('a list * int) Seq.t
  (** Returns a sequence that yields the data associated with the strings of the
      trie, groupped by distance to the specified string, together with said
      distance, in order from closest to farthest.

      Each node of the sequence should be called at most once. *)
end

module Item: sig
  type ('v, 'k) t = {
    name: string;
    item : 'v;
    kind : 'k;
  }
  val item: ('v,'k) t -> 'v
end

type ('a,'v) matches = {
  left: 'a list;
  pairs:('v * 'v) list;
  right:'a list;
}
type ('v,'k) item_matches =  (('v,'k) Item.t, 'v) matches

val fuzzy_match_names:
  compatibility:('k -> 'k -> bool)
  -> ('v,'k) Item.t list -> ('v,'k) Item.t list
  -> ('v,'k) item_matches
