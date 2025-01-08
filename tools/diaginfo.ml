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
let json_schema_to_print = ref None
let history = ref false
let output = ref None
let version = ref None
let adt_schema_to_print = ref None
let log_schemas = [
  "meta";
  "config";
  "compiler";
  "toplevel"; "error"; "kind"; "msg"; ]

module String_map = Misc.Stdlib.String.Map


(** Collect sum and record definitions from a scheme *)
module Defs = struct
  open Diagnostic
  let union map a = List.fold_left (fun m add -> add m) map a
  let rec refs: type a.
    a typ -> (any_typ String_map.t as 'r) -> 'r  =
    fun ty map -> match ty with
      | Sum x ->
          let name = scheme_name x in
          if String_map.mem name map then map
          else
            let map = String_map.add name (T ty) map in
            subrefs (field_infos x) map
      | Record x ->
          let name = scheme_name x in
          if String_map.mem name map then map
          else
            let map = String_map.add name (T ty) map in
            subrefs (field_infos x) map
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
    (string * label_metadata) list -> (any_typ String_map.t as 'm) -> 'm
    = fun keys map ->
      union map @@
      List.map (fun (_, { ltyp = T t; _}) -> refs t) keys
end

module JSchema = struct
  open Diagnostic
  module Pp = Diagnostic_backends.Pp
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

  let tuple l =
    Format.dprintf "%t,@ %t"
      (tfield  {|array|})
      (item ~key:"prefixItems" @@ array l)

  let tuple_typ l = tuple (List.map (fun x -> obj [x]) l)

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

  let any_typ = tfield {|object|}

  let desc_field d = item ~key:"description" @@ string d

  let obj_typ = item ~key:"type" (string "object")
  let record_type desc fields required =
    [ obj_typ;
      desc;
      item ~key:"properties" @@ obj fields;
      item ~key:"required" @@ array required
    ]

  let one_of l = item ~key:"oneOf" (array l)
  let const name = item ~key:"const" @@ string name
  let sum ~desc x =
    let brule name core =
      let name = const name in
      let forward_record =
        let kcontents = "contents" in
        let contents = item ~key:kcontents (obj [tuple_typ core]) in
        let next = item ~key:"next" (obj [any_typ]) in
        let desc = desc_field "expanded record for forward compatibility" in
        record_type desc [contents; next] [string kcontents]
      in
      obj [one_of [
        obj [tuple_typ (name::core)];
        obj [tuple [obj [name]; obj forward_record]]
      ]]
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

  let schema_field =
    item ~key:"schema" @@ obj [obj_typ]

  let record_fields ~desc v x =
    record_type (desc_field desc) (fields v x) (required_fields x)

  let simple_record ~desc x = obj (record_fields ~desc None x)

  let def_printer = function
      | T (Sum x) -> sum ~desc:(scheme_description x) x
      | T (Record x) ->
        simple_record ~desc:(scheme_description x) (field_infos x)
      | _ -> ignore

   let pp v sch ppf =
     let keys = metakey :: field_infos sch in
     let refs = Defs.subrefs keys String_map.empty in
     let defs =
       if String_map.is_empty refs then []
       else
         let refs = String_map.bindings refs in
         let prs =
           List.map (fun (key,ty) -> item ~key (def_printer ty)) refs
         in
         [item ~key:"$defs" @@ obj prs]
     in
     obj (
       header (scheme_name sch)
       @ defs
       @ schema_field :: record_fields ~desc:(scheme_description sch) v keys
     ) ppf

  end

module Annotated_adt = struct

  open Diagnostic
  let time ppf () = Format.fprintf ppf "@ *@ "
  let tuple ~parentheses components ppf =
    let pr = Format.pp_print_list ~pp_sep:time (|>) in
    if parentheses then Format.fprintf ppf "@[(%a)@]" pr components
    else Format.fprintf ppf "@[%a@]" pr components

  let string s ppf = Format.pp_print_string ppf s

  let rec typ: type a b.
    parentheses:bool -> a typ -> Format.formatter -> unit =
    fun ~parentheses x ->
    let t x = typ ~parentheses:true x in
    let tuple = tuple ~parentheses in
    match x with
    | Int -> string "int"
    | Bool ->  string "bool"
    | Unit -> string "int"
    | String -> string "string"
    | Float -> string "number"
    | List e ->
        Format.dprintf "%t array" (typ ~parentheses  e)
    | Pair (x,y) -> tuple [t x; t y]
    | Triple (x,y,z) -> tuple [t x; t y; t z]
    | Quadruple (x,y,z,w) -> tuple [t x;t y; t z; t w]
    | Sum x -> string (scheme_name x)
    | Record x -> string (scheme_name x)
    | Custom x -> typ ~parentheses x.default

  let sum x ppf =
    let constructor ppf (name, kty) =
      match kty.ltyp with
      | T Unit -> Format.fprintf ppf "@ | %s" name
      | T t ->
        Format.fprintf ppf "@ @[<2>| %s of@ %t@]"
          name (typ ~parentheses:false t)
    in
    List.iter (constructor ppf) (field_infos x)

  let record x ppf =
    let field ppf (name, { ltyp=T ty; _ }) =
      Format.fprintf ppf "@ @[<2>%s:@ %t;@]" name (typ ~parentheses:false ty)
    in
    Format.fprintf ppf "{";
    List.iter (field ppf) (field_infos x);
    Format.fprintf ppf "@ }"

  let def (T x) = match x with
    | Sum x -> sum x
    | Record x -> record x
    | _ -> ignore

   let pp _v sch ppf =
     let keys = field_infos sch in
     let pp_def ppf (name, ty) =
       Format.fprintf ppf "@[<hv 2>type %s = %t@]" name (def ty)
    in
    let subdefs = Defs.subrefs keys String_map.empty in
    let pp_sep ppf () = Format.fprintf ppf "@,@," in
    let typ = T (Record sch) in
    Format.fprintf ppf "@[<v>%a@,@,@ %a@]@."
      pp_def (scheme_name sch, typ)
      Format.(pp_print_seq ~pp_sep pp_def) (String_map.to_seq subdefs)
end

let args =
  [ "-json-schema",
     Arg.Symbol (log_schemas, fun x -> json_schema_to_print := Some x),
    " print all known schema in json schema format";
    "-adt-schema",
    Arg.Symbol (log_schemas, fun x -> adt_schema_to_print := Some x),
    " print all known schema in annotated ADT schema format";
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
let json_schema v ppf =
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



let adt _v ppf = function
  | None -> ()
  | Some "meta" ->
    Format.fprintf ppf "%t@." (Annotated_adt.pp v Diagnostic.Metadata.scheme)
  | Some "config" ->
    Format.fprintf ppf "%t@." (Annotated_adt.pp v Config_diagnostic.scheme)
  | Some "compiler" ->
    Format.fprintf ppf "%t@." (Annotated_adt.pp v scheme)
  | Some "toplevel" ->
    Format.fprintf ppf "%t@." (Annotated_adt.pp v Toplevel_diagnostic.scheme)
  | Some "error" ->
    Format.fprintf ppf "%t@." (Annotated_adt.pp v Error.scheme)
  | Some "kind" ->
    Format.fprintf ppf "%t@." (Annotated_adt.pp v Errd.Kind.scheme)
  | Some "msg" ->
    Format.fprintf ppf "%t@." (Annotated_adt.pp v Errd.Msg.scheme)
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
  json_schema version ppf !json_schema_to_print;
  adt version ppf !adt_schema_to_print;
  history ppf
