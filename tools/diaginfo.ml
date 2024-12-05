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

open Diagnostic_history
let json_schema = ref None
let history = ref false
let output = ref None
let version = ref None
let log_schemas = [
  "meta";
  "config";
  "compiler";
  "toplevel"; "error"; "kind"; "msg"; ]

module JSchema = struct
  module Pp = Diagnostic_backends.Pp
  open Diagnostic
  open Pp
  let string s ppf = Format.fprintf ppf "%S" s
  let bool = Pp.bool json
  let item = Pp.item json
  let header name  =
      [
        (item ~key:"$schema" @@
         string "https://json-schema.org/draft/2020-12/schema");
        (item ~key:"$id" @@ string @@
         Format.asprintf "https://github.com/ocaml/schema/%s.schema.json"
           name);
      ]

  let tfield  x = item ~key:"type" (string x)
  let obj prs = record json prs
  let array prs = list json prs

  let sref x =
    item ~key:"$ref" @@ Format.dprintf {|"#/$defs/%s"|} (scheme_name x)

  let rec typ: type a b. a typ -> Format.formatter -> unit = function
    | Int -> tfield {|integer|}
    | Bool -> tfield {|boolean|}
    | Unit -> tfield {|int|}
    | String -> tfield {|string|}
    | Float -> tfield "number"
    | List e ->
        Format.dprintf "%t,@ %t"
          (tfield  {|array|})
          (item ~key:"items" @@ obj [typ e] )
    | Pair (x,y) -> tuple_typ [typ x; typ y]
    | Triple (x,y,z) -> tuple_typ [typ x; typ y; typ z]
    | Quadruple (x,y,z,w) -> tuple_typ [typ x;typ y; typ z; typ w]
    | Sum x -> sref x
    | Record x -> sref x
    | Custom x -> typ x.default
  and tuple_typ = fun l ->
    Format.dprintf "%t,@ %t"
      (tfield  {|array|})
      (item ~key:"prefixItems" @@ array @@
       List.map (fun x -> obj [x]) l
      )

  let any_typ = tfield {|object|}

  let desc_field d = item ~key:"description" @@ string d

  let one_of l = item ~key:"oneOf" (array l)
  let const name = item ~key:"const" @@ string name
  let sum ~desc x =
    let brule name core =
      let name = const name in
      let contents = item ~key:"contents" (tuple_typ core) in
      let next = item ~key:"next" any_typ in
      one_of [
        obj [tuple_typ (name::core)];
        obj [tuple_typ [name; obj [contents; next]]]
      ]
    in
    let constructor (name, kty) =
      match kty.ltyp with
      | T Unit -> obj [const name]
      | T (Pair(x,y)) -> brule name [typ x; typ y]
      | T (Triple(x,y,z)) -> brule name [typ x; typ y; typ z]
      | T (Quadruple(x,y,z,w)) ->
          brule name [typ x; typ y; typ z; typ w]
      | T ty -> brule name [typ ty]
    in
    obj [
      desc_field desc;
      one_of (List.map constructor (field_infos x))
    ]

  let field v (key, {status; ltyp=T ty; _ }) =
    match v with
    | None -> Some (item ~key (obj [typ ty]))
    | Some _ as v ->
        let stage = Diagnostic_history.Lifetime.stage_at v status in
        match stage with
        | Future | Deletion -> None
        | _ ->
              let typ = typ ty in
              let fields =
                match stage with
                | Deprecation ->
                    let deprecated = item ~key:"deprecated" (bool true) in
                    [typ; deprecated]
                | _ -> [typ]
              in
              Some (item ~key (obj fields))

  let fields v x = List.filter_map (field v) x

  let required_fields x =
    List.filter_map
      (fun (k, kinfo) -> if is_optional kinfo then None else Some(string k))
      x

  let obj_typ = item ~key:"type" (string "object")

  let schema_field =
    item ~key:"schema" @@ obj [obj_typ]

  let record_fields ~desc v x =
    [ obj_typ;
      desc_field desc;
      item ~key:"properties" @@ obj (fields v x);
      item ~key:"required" @@ array (required_fields x)
    ]

  let simple_record ~desc x = obj (record_fields ~desc None x)

  module String_map = Misc.Stdlib.String.Map
  let union map a = List.fold_left (fun m add -> add m) map a
  let rec refs: type a.
    a typ -> ((Format.formatter -> unit) String_map.t as 'r) -> 'r  =
    fun ty map -> match ty with
      | Sum x ->
          let name = scheme_name x in
          let desc = scheme_description x in
          if String_map.mem name map then map
          else
            let map = String_map.add name (sum ~desc x) map in
            subrefs (field_infos x) map
      | Record x ->
          let name = scheme_name x in
          if String_map.mem name map then map
          else
            let desc= scheme_description x in
            let fields = field_infos x in
            let map =
              String_map.add name
                (simple_record ~desc fields) map in
            subrefs fields map
      | Int -> map
      | Bool -> map
      | String -> map
      | Unit -> map
      | Float -> map
      | List elt -> refs elt map
      | Pair (x,y) -> union map [refs x; refs y]
      | Triple (x,y,z) -> union map [refs x; refs y; refs z]
      | Quadruple (x,y,z,w) -> union map [refs x; refs y; refs z; refs w]
      | Custom t -> refs t.default map
  and subrefs: type a.
    (string * label_metadata) list ->
    ((Format.formatter -> unit) String_map.t as 'm) -> 'm
    = fun keys map ->
      union map @@
      List.map (fun (_, { ltyp = T t; _}) -> refs t) keys

   let pp v sch ppf =
     let keys = metakey :: field_infos sch in
     let defs = match String_map.bindings (subrefs keys String_map.empty) with
       | [] -> []
       | defs ->
           let prs = List.map (fun (key,pr) -> item ~key pr) defs in
           [item ~key:"$defs" @@ obj prs]
     in
     obj (
       header (scheme_name sch)
       @ defs
       @ schema_field :: record_fields ~desc:(scheme_description sch) v keys
     ) ppf

  end


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
open Compiler_diagnostic
module Errd = Location.Error_diagnostic
let version () =
  match !version with
  | None -> Some (Diagnostic_history.current_version V.history)
  | Some v ->
      match Scanf.sscanf_opt v "%d.%d"
              (fun major minor -> {Diagnostic_history.major;minor})
      with
      | Some _ as v -> v
      | None -> Some (Diagnostic_history.current_version V.history)
let schema v ppf =
  function
  | None -> ()
  | Some "meta" ->
    Format.fprintf ppf "%t@." (JSchema.pp v Diagnostic.Metadata.scheme)
  | Some "config" ->
    Format.fprintf ppf "%t@." (JSchema.pp v Config_diagnostic.scheme)
  | Some "compiler" ->
    Format.fprintf ppf "%t@." (JSchema.pp v scheme)
  | Some "toplevel" ->
    Format.fprintf ppf "%t@." (JSchema.pp v Toplevel_diagnostic.scheme)
  | Some "error" ->
    Format.fprintf ppf "%t@." (JSchema.pp v Error.scheme)
  | Some "kind" ->
    Format.fprintf ppf "%t@." (JSchema.pp v Errd.Kind.scheme)
  | Some "msg" ->
    Format.fprintf ppf "%t@." (JSchema.pp v Errd.Msg.scheme)
  | _ -> ()


module Pp = struct
  open Format
  module Vmap = Map.Make(struct
      type t = Diagnostic_history.version
      let compare: t -> t -> int = Stdlib.compare
    end)

  module String_map = Map.Make(String)

  let group_by_version_then_scheme event_seq =
    let open Diagnostic_history in
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
    Seq.fold_left add (Vmap.empty,[]) event_seq


  let status ppf range =
    match Lifetime.stage range with
    | Lifetime.Inception -> fprintf ppf "refined"
    | Lifetime.Publication -> fprintf ppf "created"
    | Lifetime.Expansion -> fprintf ppf "expanded"
    | Lifetime.Deprecation -> fprintf ppf "deprecated"
    | Lifetime.Deletion -> fprintf ppf "deleted"
    | Lifetime.Future -> fprintf ppf "future"


  let error ppf =
    let open Diagnostic_history in
    function
    | Time_travel (v,x) ->
        fprintf ppf "Error: future key (%a<%a)" pp v pp x
    | Duplicate_key s -> fprintf ppf "Error: duplicate %s" s
    | Invalid_constructor_expansion s ->
        fprintf ppf "Error: second constructor expansion %s" s
    | Invalid_publication s ->
        fprintf ppf "Error: second constructor publication %s" s
    | Inconsistent_change (range,key_name) ->
        fprintf ppf "Error inconsistent change of the %a key %s"
          status range
          key_name
    | Sealed_version v -> fprintf ppf "Error: seal breach %a" pp v

  let base_event ppf =
    function
    | Inception r ->
        fprintf ppf "Inception: %s>%s,%s" r.base_name r.new_name r.typ
    | Declaration -> fprintf ppf "Declaration"
    | Publication name -> fprintf ppf "Publication %s" name
    | Creation {name;typ} ->
        if typ = "" then fprintf ppf "New label %s" name
        else fprintf ppf "New label %s, %s" name typ
    | Expansion {name;expansion} ->
        fprintf ppf "Constructor %s>%s" name expansion
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
    Format.fprintf ppf "@[<v 2>%a@," pp version;
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
      Pp.history Diagnostic.Metadata_versions.history
      Pp.history Config_diagnostic.Versions.history
      Pp.history V.history

let () =
  Arg.parse args ignore "print log information";
  let ppf = formatter !output in
  let version = version () in
  schema version ppf !json_schema;
  history ppf
