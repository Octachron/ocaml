
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Gabriel Radanne, projet Cambium, INRIA Rocquencourt        *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type ('a, 'b, 'c, 'd) config = {
  deletion : 'a -> int ;
  insertion : 'b -> int ;
  keep : 'a -> 'b -> 'c -> int ;
  change : 'a -> 'b -> 'd -> int ;
}

type change =
  | Insert
  | Delete
  | Change
  | Keep

type patch = change list

val diff :
  ('a, 'b, 'c, 'd) config -> int ->
  ('a -> 'b -> ('c, 'd) result) ->
  'a array -> 'b array -> patch option
