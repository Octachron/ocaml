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
let version = ref None
let log_schemas = [
  "compiler";
  "toplevel"; "error"; "kind"; "msg"; ]

module JSchema = Diagnostic_backends.Json_schema

let args =
  [ "-json-schema", Arg.Symbol (log_schemas, fun x -> json_schema := Some x),
    " print all known json_schema";
    "-history", Arg.Set history, " print log format history";
    "-version", Arg.String (fun x -> version := Some x), " schema version";
    "-o", Arg.String (fun x -> output := Some x), " output file"
  ]

let formatter = function
  | None -> Format.std_formatter
  | Some s -> Format.formatter_of_out_channel (Out_channel.open_bin s)
open Log
let version () =
  match !version with
  | None -> Version.current_version Compiler_log_version.history
  | Some v ->
      match Scanf.sscanf_opt v "%d.%d"
              (fun major minor -> Version.{major;minor})
      with
      | Some v -> v
      | None -> Version.current_version Compiler_log_version.history
let schema v ppf =
  function
  | None -> ()
  | Some "compiler" ->
    Format.fprintf ppf "%t@." (JSchema.pp v Compiler.scheme)
  | Some "toplevel" ->
    Format.fprintf ppf "%t@." (JSchema.pp v Toplevel.scheme)
  | Some "error" ->
    Format.fprintf ppf "%t@." (JSchema.pp v Error.scheme)
  | Some "kind" ->
    Format.fprintf ppf "%t@." (JSchema.pp v Location.Error_log.Kind.scheme)
  | Some "msg" ->
    Format.fprintf ppf "%t@." (JSchema.pp v Location.Error_log.Msg.scheme)
  | _ -> ()

let history ppf =
  if !history then
    Format.fprintf ppf "%a%!"
      Version.pp_history Compiler_log_version.history

let () =
  Arg.parse args ignore "print log information";
  let ppf = formatter !output in
  let version = version () in
  schema version ppf !json_schema;
  history ppf
