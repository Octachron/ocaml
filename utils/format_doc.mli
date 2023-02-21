(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Florian Angeletti, projet Cambium, Inria Paris             *)
(*                                                                        *)
(*   Copyright 2023 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Composable document for the Format formatting engine *)

type box_type =
  | H
  | V
  | HV
  | HoV
  | B

type stag = Format.stag

type element =
  | Data of string
  | With_size of int
  | Open_box of { kind: box_type ; indent:int }
  | Close_box
  | Open_tag of Format.stag
  | Close_tag
  | Open_tbox
  | Tbreak of { width : int; offset : int }
  | Set_tab
  | Close_tbox
  | Break of { fits : string * int * string as 'a; breaks : 'a }
  | Flush of { newline:bool }
  | Newline
  | If_newline

type t
type doc = t

val format: Format.formatter -> doc -> unit
val fold: ('acc -> element -> 'acc) -> 'acc -> doc -> 'acc


val open_box: box_type -> int -> doc -> doc
val close_box: doc -> doc

val string: string -> doc -> doc
val bytes: bytes -> doc -> doc
val with_size: int -> doc -> doc

val int: int -> doc -> doc
val float: float -> doc -> doc
val char: char -> doc -> doc
val bool: bool -> doc -> doc

val space: doc -> doc
val cut: doc -> doc
val break: spaces:int -> indent:int -> doc -> doc

val custom_break: fits:(string * int * string as 'a) -> breaks:'a -> doc -> doc
val force_newline: doc -> doc
val if_newline: doc -> doc

val flush: doc -> doc
val force_stop: doc -> doc

val open_tbox: doc -> doc
val set_tab: doc -> doc
val tab: doc -> doc
val tab_break: int -> doc -> doc
val close_tbox: doc -> doc

val open_stag: stag -> doc -> doc
val close_stag: doc -> doc

val iter: ?sep:(doc -> doc)
  -> (('a -> unit) -> 'b -> unit)
  -> ('a -> doc -> doc) -> 'b ->
  doc -> doc

val list: ?sep:(doc->doc) -> ('a -> doc -> 'doc) -> 'a list -> doc -> doc
val array: ?sep:(doc->doc) -> ('a -> doc -> 'doc) -> 'a array -> doc -> doc
val seq: ?sep:(doc->doc) -> ('a -> doc -> 'doc) -> 'a Seq.t -> doc -> doc

val text: string -> doc -> doc

val option: ?none:(doc->doc) -> ('a -> doc -> doc) -> 'a option -> doc -> doc
val result: ok:('a -> doc -> doc) -> error:('b -> doc -> doc) ->
  ('a,'b) result -> doc -> doc

val either: left:('a -> doc -> doc) -> right:('b -> doc -> doc) ->
  ('a,'b) Either.t -> doc -> doc



type ('a,'b) fmt = ('a, doc, doc,'b) format4

val printf: ('a, doc -> doc) fmt -> 'a
val kprintf: ( (doc -> doc) -> 'b) -> ('a, 'b) fmt -> 'a
