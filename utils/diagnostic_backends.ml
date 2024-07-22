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
module Fmt = struct

  type 'a printer = Format.formatter -> 'a -> unit
  type pr = Format.formatter -> unit
  type extension_printer =
  { extension: 'b. 'b extension -> 'b printer option}

  type list_convention = {
    list_open: pr;
    list_close: pr;
    sep: pr;
  }

  type assoc = {
    assoc_open:pr;
    assoc_close: pr;
    sep: unit printer;
    open_with_label: pr;
    label_sep: pr;
    close_with_label: pr;
  }

  type conv = {
    string:string printer;
    atom: string -> pr;
    assoc:assoc;
    list:list_convention;
  }

  let bool _conv b ppf = Format.pp_print_bool ppf b

  let escape_string ppf str =
    Format.fprintf ppf {|"|};
    for i = 0 to String.length str - 1 do
      match str.[i] with
      | '\\' -> Format.fprintf ppf {|\\|}
      | '\"' -> Format.fprintf ppf {|\"|}
      | '\n' -> Format.fprintf ppf {|\n|}
      | '\t' -> Format.fprintf ppf {|\t|}
      | '\r' -> Format.fprintf ppf {|\r|}
      | '\b' -> Format.fprintf ppf {|\b|}
      | '\x00' .. '\x1F' | '\x7F' as c ->
          Format.fprintf ppf "\\u%04X" (Char.code c)
      | c -> Format.fprintf ppf "%c" c
    done;
    Format.fprintf ppf {|"|}

  let item conv ~key elt ppf =
    conv.assoc.open_with_label ppf;
    conv.atom key ppf;
    conv.assoc.label_sep ppf;
    elt ppf;
    conv.assoc.close_with_label ppf

  let list conv prs ppf =
    let pp_sep ppf () = conv.list.sep ppf in
    conv.list.list_open ppf;
    Format.pp_print_list ~pp_sep (fun ppf pr -> pr ppf) ppf prs;
    conv.list.list_close ppf

  let tuple ~inline conv prs ppf =
    let pp_sep ppf () = conv.list.sep ppf in
    if not inline then conv.list.list_open ppf;
    Format.pp_print_list ~pp_sep (fun ppf pr -> pr ppf) ppf prs;
    if not inline then conv.list.list_close ppf


  let record conv fields ppf =
    if List.is_empty fields then () else begin
      conv.assoc.assoc_open ppf;
      Format.pp_print_list ~pp_sep:conv.assoc.sep
        (fun ppf pr -> pr ppf) ppf fields;
      conv.assoc.assoc_close ppf
    end

  type ctx = {
    conv:conv;
    ext_printer:extension_printer;
    version:Version.t option
  }

  let rec elt : type a. ?inline:bool -> ctx -> a typ -> a -> pr =
    fun ?(inline=false) ctx typ x ppf ->
    match typ with
    | Unit -> Format.pp_print_int ppf 0
    | Int -> Format.pp_print_int ppf x
    | Bool -> bool ctx.conv x ppf
    | String -> ctx.conv.string ppf x
    | Pair (a,b) ->
        let x,y = x in
        tuple ~inline ctx.conv [
        elt ctx a x;
        elt ctx b y;
      ] ppf
    | Triple (a,b,c) ->
        let x, y, z = x in
        tuple ~inline ctx.conv [
        elt ctx a x;
        elt ctx b y;
        elt ctx c z;
      ] ppf
    | Quadruple (a,b,c,d) ->
        let x, y, z ,w = x in
        tuple ~inline ctx.conv [
        elt ctx a x;
        elt ctx b y;
        elt ctx c z;
        elt ctx d w
      ] ppf
    | Custom {pull; default; id } -> begin
        match ctx.ext_printer.extension id with
        | Some pr -> pr ppf x
        | None -> elt ctx default (pull ctx.version x) ppf
      end
    | List e -> list ctx.conv (List.map (elt ~inline:false ctx e) x) ppf
    | Sum _ ->
        destruct x (fun name (V(typ,x)) ->
            match typ with
            | Unit -> ctx.conv.atom name ppf
            | _ ->
                tuple ~inline:false ctx.conv
                  [ ctx.conv.atom name; elt ~inline:true ctx typ x ]
                  ppf
          )
    | Record m -> elt_record ctx (field_names m,x) ppf
  and trim_item: type a.
    ctx -> key:string -> optional:bool -> a typ -> a -> pr option =
    fun ctx ~key ~optional ty x ->
    if not optional then Some (elt_item ctx ~key ty x) else
      match ty, x with
      | List _ , [] -> None
      | Record def, _ ->
          begin match fields ctx (field_names def,x) with
          | [] -> None
          | _ :: _ as fields ->
              Some (item ctx.conv ~key @@ record ctx.conv fields)
          end
      | Custom {pull;default;id}, _ ->
          begin
            match ctx.ext_printer.extension id with
            | Some pr -> Some (fun ppf -> pr ppf x)
            | None -> trim_item ctx ~key ~optional default (pull ctx.version x)
          end
      | _ -> Some (elt_item ctx  ~key ty x)
  and elt_item: type a. ctx -> key:string -> a typ -> a -> pr =
    fun ctx ~key ty x ppf -> item ctx.conv ~key (elt ctx ty x) ppf
  and fields: type p. ctx -> (string list * p record)  -> pr list
    = fun ctx (keys,prod) ->
      let fields = Log.fields keys prod in
      let pp_field (name, optional, V(typ,x)) =
        trim_item ctx ~optional ~key:name typ x in
      List.filter_map pp_field fields
  and elt_record: type p. ctx -> (string list * p record) -> pr =
    fun ctx x -> record ctx.conv (fields ctx x)


  let direct = {
    atom = (fun _s -> ignore);
    string = Format.pp_print_string;
    list = {
      list_open = ignore;
      list_close = ignore ;
      sep = Format.dprintf "@ ";
    };
    assoc = {
      assoc_open = Format.dprintf "@[<v>";
      assoc_close = Format.dprintf "@]";
      open_with_label = ignore;
      label_sep = ignore;
      sep = (fun ppf () -> Format.fprintf ppf "@,");
      close_with_label = ignore;
    }
  }

  let direct_with_fields =
    let assoc =
      { direct.assoc with label_sep = Format.dprintf ": " }
    in
    { direct with atom = Format.dprintf "%s"; assoc  }

  let sexp =
    let list_open = Format.dprintf "@[("
    and list_close = Format.dprintf ")@]"
    and sep = Format.dprintf "@ " in
    {
      atom = (fun s ppf -> Format.pp_print_string ppf s);
      string = escape_string;
      list = {list_open; list_close; sep };
      assoc = {
        assoc_open = list_open;
        assoc_close = list_close;
        open_with_label = Format.dprintf "@[<b 2>(";
        sep = (fun ppf () -> sep ppf);
        label_sep = sep;
        close_with_label = Format.dprintf ")@]";
      }
    }

  let json =
    {
      string = escape_string;
      atom = (fun s ppf -> escape_string ppf s);
      list = {
        list_open=Format.dprintf "@[<b 2>[";
        list_close = Format.dprintf "]@]";
        sep = Format.dprintf ",@ ";
      };
      assoc = {
        assoc_open = Format.dprintf "@[<hv 2>{@ ";
        assoc_close = Format.dprintf "@;<0 -2>}@]";
        open_with_label = Format.dprintf "@[<b 2>";
        label_sep = Format.dprintf "@ :@ ";
        sep = (fun ppf () -> Format.fprintf ppf ",@ ");
        close_with_label = Format.dprintf "@]";
      }
    }


  let no_extension = { extension = fun _ -> None }
  let doc_printer (type a): a extension -> a printer option =
    function
    | Reports.Structured_text.Doc -> Some Format_doc.Doc.format
    | _ -> None
  let doc_extension = { extension = doc_printer  }
  let chain_extensions x y =
    let chain ext =
      match x.extension ext with
      | None -> y.extension ext
      |  Some _ as p -> p
    in
    { extension = chain }

  let extensions = ref doc_extension
  let add_extension x =
    extensions := chain_extensions x !extensions

end

  let with_conv ~structured ~extension conv settings version ppf scheme =
    let ctx = { Fmt.version=Some version; conv; ext_printer=extension} in
    let record ppf (R(def, r)) =
      let field_names = field_names def in
      let fs = Log.fields field_names r in
      if List.is_empty fs then () else
        let fields =
          let meta =
            Fmt.fields ctx (["metadata"],r) in
          meta @ Fmt.fields ctx (field_names,r)
        in
        Format.fprintf ppf "%t@." (Fmt.record ctx.conv fields)
    in
    let item ppf (name, V(typ,r)) =
      Fmt.elt_item ctx ~key:name typ r ppf
    in
    make ~structured ~printer:{record;item} settings version scheme ppf

  let structured conv =
    with_conv ~structured:true ~extension:Fmt.no_extension conv
  let sexp color version ppf sch = structured Fmt.sexp color version ppf sch
  let json color version ppf sch = structured Fmt.json color version ppf sch
  let direct color version ppf sch =
    with_conv ~structured:false ~extension:(!Fmt.extensions)
      Fmt.direct color version ppf sch
  let direct_with_fields color version ppf sch =
    with_conv ~structured:false ~extension:(!Fmt.extensions)
      Fmt.direct_with_fields color version ppf sch


  type t = {
    name:string;
    make: 'a. Misc.Color.setting option -> version -> Format.formatter ref
      -> 'a def -> 'a log;
  }
  let fmt = { name="direct"; make = direct }
  let fmt_with_fields = { name="direct_with_fields"; make = direct_with_fields }
  let sexp = { name="sexp" ; make = sexp }
  let json = { name = "json"; make = json }


module Json_schema = struct
  open Fmt
  let string s ppf = Format.fprintf ppf "%S" s
  let bool = Fmt.bool json
  let item = Fmt.item json
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

  let const name = item ~key:"const" @@ string name
  let sum x =
    let constructor (name, kty) =
      match kty.ltyp with
      | T Unit -> obj [const name]
      | T (Pair(x,y)) -> obj [tuple_typ [const name; typ x; typ y]]
      | T (Triple(x,y,z)) -> obj [tuple_typ [const name; typ x; typ y; typ z]]
      | T (Quadruple(x,y,z,w)) ->
          obj [tuple_typ [const name; typ x; typ y; typ z; typ w]]
      | T ty -> obj [tuple_typ [const name; typ ty]]
    in
    obj [ item ~key:"oneOf" (array (List.map constructor (field_infos x))) ]

  let field v (key, {status; ltyp=T ty; _ }) =
    match v with
    | None -> Some (item ~key (obj [typ ty]))
    | Some _ as v ->
        let stage = Version.stage_at v status in
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

  let record_fields v x =
    [
      obj_typ;
      item ~key:"properties" @@ obj (fields v x);
      item ~key:"required" @@ array (required_fields x)
    ]

  let simple_record x = obj (record_fields None x)

  module String_map = Misc.Stdlib.String.Map
  let union map a = List.fold_left (fun m add -> add m) map a
  let rec refs: type a.
    a typ -> ((Format.formatter -> unit) String_map.t as 'r) -> 'r  =
    fun ty map -> match ty with
      | Sum x ->
          let name = scheme_name x in
          if String_map.mem name map then map
          else
            let map = String_map.add (scheme_name x) (sum x) map in
            subrefs (field_infos x) map
      | Record x ->
          let name = scheme_name x in
          if String_map.mem name map then map
          else
            let fields = field_infos x in
            let map = String_map.add name (simple_record fields) map in
            subrefs fields map
      | Int -> map
      | Bool -> map
      | String -> map
      | Unit -> map
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
       @ schema_field :: record_fields v keys
     ) ppf

  let pp_log ppf log =
    let sch = log_scheme log in
    pp (log_version log) sch ppf

  end
