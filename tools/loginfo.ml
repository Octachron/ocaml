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


let json_schema = ref None
let history = ref false
let log_schemas = [
  "compiler";
  "toplevel"; "error"; "kind"; "msg"; ]


let args =
  [ "-json-schema", Arg.Symbol (log_schemas, fun x -> json_schema := Some x),
    " print all known json_schema";
    "-history", Arg.Set history, " print log format history"
  ]

let () =
  Arg.parse args ignore "print log information";
  let open Log in
  begin match !json_schema with
  | None -> ()
  | Some "compiler" ->
    Format.printf "%t@." (Json_schema.pp Compiler.scheme)
  | Some "toplevel" ->
    Format.printf "%t@." (Json_schema.pp Toplevel.scheme)
  | Some "error" ->
    Format.printf "%t@." (Json_schema.pp Error.scheme)
  | Some "kind" ->
    Format.printf "%t@." (Json_schema.pp Location.Error_log.Kind.scheme)
  | Some "msg" ->
    Format.printf "%t@." (Json_schema.pp Location.Error_log.Msg.scheme)
  | _ -> ()
  end;
  if !history then
    Format.printf "%a@." Version.pp_history Compiler_log_version.history
