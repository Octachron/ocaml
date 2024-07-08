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
  "meta";
  "config";
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
open Reports
let version () =
  match !version with
  | None -> Some (Version.current_version V.history)
  | Some v ->
      match Scanf.sscanf_opt v "%d.%d"
              (fun major minor -> Version.{major;minor})
      with
      | Some _ as v -> v
      | None -> Some (Version.current_version V.history)
let schema v ppf =
  function
  | None -> ()
  | Some "meta" ->
    Format.fprintf ppf "%t@." (JSchema.pp v Log.Metadata.scheme)
  | Some "config" ->
    Format.fprintf ppf "%t@." (JSchema.pp v Config.scheme)
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


module Pp = struct
  open Format
  open Log.Version
  module Vmap = Map.Make(struct
      type t = Log.Version.t
      let compare: t -> t -> int = Stdlib.compare
    end)

  module String_map = Map.Make(String)

  let group_by_version_then_scheme events =
    let add (m,errors) e =
      let map_at_v =
        Option.value ~default:String_map.empty (Vmap.find_opt e.version m) in
      let prev =
        Option.value ~default:[] (String_map.find_opt e.scheme map_at_v)
      in
      let map_at_v = String_map.add e.scheme (e.event::prev) map_at_v in
      let errors = match e.event with Error e -> e :: errors | _ -> errors in
      Vmap.add e.version map_at_v m, errors
    in
    Seq.fold_left add (Vmap.empty,[]) events


  let status ppf range =
    match stage range with
    | Lifetime.Refinement -> fprintf ppf "refined"
    | Lifetime.Creation -> fprintf ppf "created"
    | Lifetime.Expansion -> fprintf ppf "expanded"
    | Lifetime.Deprecation -> fprintf ppf "deprecated"
    | Lifetime.Deletion -> fprintf ppf "deleted"
    | Lifetime.Future -> fprintf ppf "future"


  let error ppf = function
    | Time_travel (v,x) ->
        fprintf ppf "Error: future key (%a<%a)" Version.pp v Version.pp x
    | Duplicate_key s -> fprintf ppf "Error: duplicate %s" s
    | Invalid_constructor_expansion s ->
        fprintf ppf "Error: second constructor expansion %s" s
    | Inconsistent_change (range,key_name) ->
        fprintf ppf "Error inconsistent change of the %a key %s"
          status range
          key_name
    | Sealed_version v -> fprintf ppf "Error: seal breach %a" Version.pp v

  let base_event ppf =
    function
    | Refinement r ->
        fprintf ppf "Refinement: %s>%s,%s"
          r.base_name r.new_name r.typ
    | Creation -> fprintf ppf "Creation"
    | New_key {name;typ} ->
        if typ = "" then fprintf ppf "Key %s" name
        else fprintf ppf "Key %s, %s" name typ
    | Expansion {name;expansion} -> fprintf ppf "Key %s>%s" name expansion
    | Make_required name -> fprintf ppf "Newly required %s" name
    | Deprecation name -> fprintf ppf "Deprecation %s" name
    | Seal -> fprintf ppf "Seal"
    | Deletion name -> fprintf ppf "Deletion %s" name
    | Error e -> error ppf e

  let scheme_at_v ppf (scheme_name,events) =
    Format.fprintf ppf "@[<v 2>%s@,%a@]"
      scheme_name
      (pp_print_list base_event) (List.rev events)

  let events_by_version_then_scheme ppf (version, map_at_v) =
    Format.fprintf ppf "@[<v 2>%a@," Version.pp version;
    pp_print_seq scheme_at_v ppf (String_map.to_seq map_at_v);
    Format.fprintf ppf "@]"

  let errors ppf = function
    | [] -> ()
    | errors ->
        fprintf ppf "@[<v 2>Invalid diagnostic history@,%a@]"
          (pp_print_list error) errors

  let history ppf h =
    let events = events h in
    let m, err = group_by_version_then_scheme events in
    fprintf ppf "@[<v>%a%a@]"
      errors err
      (pp_print_seq events_by_version_then_scheme) (Vmap.to_seq m);
    if not (List.is_empty err) then exit 2
end

let history ppf =
  if !history then
    Format.fprintf ppf
      "@[<v 2>Metadata:@,%a@;<0 -2>\
      Config:@,%a@;<0 -2>\
       Main:@,%a@]%!"
      Pp.history Metadata_versions.history
      Pp.history Config_versions.history
      Pp.history V.history

let () =
  Arg.parse args ignore "print log information";
  let ppf = formatter !output in
  let version = version () in
  schema version ppf !json_schema;
  history ppf
