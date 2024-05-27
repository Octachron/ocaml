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


let json_schema = ref false
let history = ref false

let args =
  [ "-json-schema", Arg.Set json_schema, "print all known json_schema";
    "-history", Arg.Set history, "print log format history"
  ]

let () =
  Arg.parse args ignore "print log information";
  let open Log in
  if !json_schema then
    Format.printf "%t@." (Json_schema.pp Compiler.scheme);
  if !history then
    Format.printf "%a@." Version.pp_history Compiler_log_version.history
