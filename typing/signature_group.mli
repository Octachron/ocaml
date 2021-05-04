(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*  Florian Angeletti, projet Cambium, Inria Paris                        *)
(*                                                                        *)
(*   Copyright 2021 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Fold on a signature by syntactic group of items *)

(** Classes and class types generate ghosts signature items, we group them
    together before printing *)
type sig_item =
  {
    src: Types.signature_item;
    post_ghosts: Types.signature_item list
    (** ghost classes types are post-declared *);
  }

type core_rec_group =
  | Not_rec of sig_item
  | Rec_group of sig_item list

(** Private row types are manifested as a sequence of definitions
    preceding a recursive group, we collect them and separate them from the
    syntatic recursive group. *)
type rec_group =
  { pre_ghosts: Types.signature_item list; group:core_rec_group }

val group: Types.signature -> (rec_group * Types.signature) Seq.t
val iter: (rec_group -> unit) -> Types.signature -> unit
val fold: ('acc -> rec_group -> 'acc) -> 'acc -> Types.signature -> 'acc

type 'a in_place_patch = {
  ghosts: Types.signature;
  replace_by: Types.signature;
  info: 'a;
}

val replace_in_place:
  ( rec_group:sig_item list -> ghosts:Types.signature -> Types.signature_item
    -> 'a in_place_patch option )
  -> Types.signature -> ('a * Types.signature) option
