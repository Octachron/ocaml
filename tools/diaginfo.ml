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
let output = ref None
let log_schemas = [
  "compiler";
  "toplevel"; "error"; "kind"; "msg"; ]


let args =
  [ "-json-schema", Arg.Symbol (log_schemas, fun x -> json_schema := Some x),
    " print all known json_schema";
    "-history", Arg.Set history, " print log format history";
    "-o", Arg.String (fun x -> output := Some x), " output file"
  ]

let formatter = function
  | None -> Format.std_formatter
  | Some s -> Format.formatter_of_out_channel (Out_channel.open_bin s)
open Log
let schema ppf =
  function
  | None -> ()
  | Some "compiler" ->
    Format.fprintf ppf "%t@." (Json_schema.pp Compiler.scheme)
  | Some "toplevel" ->
    Format.fprintf ppf "%t@." (Json_schema.pp Toplevel.scheme)
  | Some "error" ->
    Format.fprintf ppf "%t@." (Json_schema.pp Error.scheme)
  | Some "kind" ->
    Format.fprintf ppf "%t@." (Json_schema.pp Location.Error_log.Kind.scheme)
  | Some "msg" ->
    Format.fprintf ppf "%t@." (Json_schema.pp Location.Error_log.Msg.scheme)
  | _ -> ()

let history ppf =
  if !history then
    Format.fprintf ppf "%a%!"
      Version.pp_history Compiler_log_version.history

let () =
  Arg.parse args ignore "print log information";
  let ppf = formatter !output in
  schema ppf !json_schema;
  history ppf
