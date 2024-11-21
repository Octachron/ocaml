(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Florian Angeletti, projet Cambium, Inria Paris             *)
(*                                                                        *)
(*   Copyright 2024 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type version =
  | Downward_compatible of Diagnostic_history.version
  | Exact of Diagnostic_history.version

val reference_version: version -> Diagnostic_history.version
val exact_version: version -> Diagnostic_history.version option

type path = string list
type report_paths = { deprecated: path list; invalid: path list }

(** Metada module *)
val diagnostic:
  version:version -> 'a Diagnostic.t -> 'a Diagnostic.record -> report_paths
