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

open Log
module Fmt: sig
  type 'a printer = Format.formatter -> 'a -> unit
  type extension_printer =
    { extension: 'b. 'b extension -> 'b printer option}
  val add_extension: extension_printer -> unit
end

type t = {
  name:string;
  make:
    'a. ?color:Misc.Color.setting -> version:version -> device:Log.device ->
    'a def -> 'a log;
}
val fmt: t
val fmt_with_fields:t
val json: t
val sexp: t


module Json_schema:sig
  val pp_log: Format.formatter -> 'a log -> unit
  val pp:  Version.t option -> 'a def -> Format.formatter -> unit
end
