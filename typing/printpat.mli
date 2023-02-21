(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)



val pretty_const
  : Asttypes.constant -> string

val top_pretty: 'k Typedtree.general_pattern Format_doc.printer

val pretty_pat: Format.formatter -> 'k Typedtree.general_pattern -> unit
val pretty_line: 'k Typedtree.general_pattern list Format_doc.printer
val pretty_matrix: 'k Typedtree.general_pattern list list Format_doc.printer
