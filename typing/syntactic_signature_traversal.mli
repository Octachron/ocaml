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

type 'ident core_rec_group =
  | Not_rec of sig_item
  | Rec_group of ('ident list * sig_item list)

(** Private row types are manifested as a sequence of definitions
    preceding a recursive group, we collect them and separate them from the
    syntatic recursive group. *)
type 'ident gen_rec_group =
  { pre_ghosts: Types.signature_item list; group:'ident core_rec_group }

(** Some identifiers may require hiding when printing *)
type bound_ident = { hide:bool; ident:Ident.t }

type rec_group = bound_ident gen_rec_group


val fold:
  ('acc -> rec_group -> 'acc) -> 'acc -> Types.signature -> 'acc


val iter:
  (rec_group -> unit) ->  Types.signature -> unit
