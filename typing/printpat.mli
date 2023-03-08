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
val top_pretty
    : ('k Typedtree.general_pattern, 'impl) Format_doc.Compat.printer
val pretty_pat
    : 'k Typedtree.general_pattern -> unit
val pretty_line
    : ('k Typedtree.general_pattern list, 'impl) Format_doc.Compat.printer
val pretty_matrix
    : ('k Typedtree.general_pattern list list, 'impl) Format_doc.Compat.printer
