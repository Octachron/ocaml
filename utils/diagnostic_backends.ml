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

  let record conv fields ppf =
    if List.is_empty fields then () else begin
      conv.assoc.assoc_open ppf;
      Format.pp_print_list ~pp_sep:conv.assoc.sep
        (fun ppf pr -> pr ppf) ppf fields;
      conv.assoc.assoc_close ppf
    end

  let rec elt : type a. conv -> extension_printer -> a typ -> a -> pr =
    fun conv {extension} typ x ppf ->
    match typ with
    | Unit -> Format.pp_print_int ppf 0
    | Int -> Format.pp_print_int ppf x
    | Bool -> bool conv x ppf
    | String -> conv.string ppf x
    | Pair (a,b) ->
        let x,y = x in
        list conv [
        elt conv {extension} a x;
        elt conv {extension} b y;
      ] ppf
    | Triple (a,b,c) ->
        let x, y, z = x in
        list conv [
        elt conv {extension} a x;
        elt conv {extension} b y;
        elt conv {extension} c z;
      ] ppf
    | Quadruple (a,b,c,d) ->
        let x, y, z ,w = x in
        list conv [
        elt conv {extension} a x;
        elt conv {extension} b y;
        elt conv {extension} c z;
        elt conv {extension} d w
      ] ppf
    | Custom {pull; default; id } -> begin
        match extension id with
        | Some pr -> pr ppf x
        | None -> elt conv {extension} default (pull x) ppf
      end
    |  List e ->
        list conv (List.map (elt conv {extension} e) x) ppf
    | Sum _ ->
        destruct x (fun name (V(typ,x)) ->
            match typ with
            | Unit -> conv.atom name ppf
            | _ ->
                list conv
                  [ conv.atom name; elt conv {extension} typ x ]
                  ppf
          )
    | Record m -> elt_record conv {extension} (field_names m,x) ppf

  and elt_item: type a.
    conv -> extension_printer -> key:string -> a typ -> a -> pr =
    fun conv ext ~key ty x ppf -> item conv ~key (elt conv ext ty x) ppf
  and fields: type p.
    conv -> extension_printer -> (string list * p record)  -> pr list
    = fun conv ext (keys,prod) ->
      let fields = Log.fields keys prod in
      let pp_field (name, V(typ,x)) = elt_item conv ext ~key:name typ x in
      List.map pp_field fields

  and elt_record: type p.
    conv -> extension_printer -> (string list * p record) -> pr =
    fun conv ext x -> record conv (fields conv ext x)


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
        list_close = Format.dprintf "@,]@]";
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
    let record ppf (R(def, r)) =
      let field_names = field_names def in
      let fs = Log.fields field_names r in
      if List.is_empty fs then () else
        let fields =
          let meta =
            Fmt.fields conv extension (["metadata"],r) in
          meta @ Fmt.fields conv extension (field_names,r)
        in
        Format.fprintf ppf "%t@." (Fmt.record conv fields)
    in
    let item ppf (name, V(typ,r)) =
      Fmt.elt_item conv extension ~key:name typ r ppf
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
  let header =
      [
        (item ~key:"$schema" @@
         string "https://json-schema.org/draft/2020-12/schema");
        (item ~key:"$id" @@
         string "https://github.com/ocaml/schema/compiler.schema.json");
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
    let constructor (name, Key_metadata kty) =
      match kty.typ with
      | Unit -> obj [const name]
      | _ -> obj [tuple_typ [const name; typ kty.typ]]
    in
    obj [ item ~key:"oneOf" (array (List.map constructor (field_infos x))) ]

  let field v (key, Key_metadata km) =
    match v with
    | None -> Some (item ~key (obj [typ km.typ]))
    | Some v ->
        if (v < km.status.creation) then None else
          match km.status.deletion with
          | Some del when del <= v -> None
          | _ ->
              let typ = typ km.typ in
              let fields =
                match km.status.deprecation with
                | Some depr when depr >= v ->
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

  module String_map = Map.Make(String)
  let union a =
    List.fold_left (fun m acc -> String_map.union (fun _ x _ -> Some x) m acc)
      String_map.empty
      a
  let rec refs: type a. a typ -> (Format.formatter -> unit) String_map.t =
    fun ty -> match ty with
      | Sum x ->
          String_map.add (scheme_name x) (sum x) (subrefs @@ field_infos x)
      | Record x ->
          String_map.add (scheme_name x)
            (simple_record @@ field_infos x)
            (subrefs @@ field_infos x)
      | Int -> String_map.empty
      | Bool -> String_map.empty
      | String -> String_map.empty
      | Unit -> String_map.empty
      | List elt -> refs elt
      | Pair (x,y) -> union [refs x; refs y]
      | Triple (x,y,z) -> union [refs x; refs y; refs z]
      | Quadruple (x,y,z,w) -> union [refs x; refs y; refs z; refs w]
      | Custom t -> refs t.default
  and subrefs: type a.
    (string * key_metadata) list -> (Format.formatter -> unit) String_map.t
    = fun keys ->
      union @@
      List.map (fun (_,Key_metadata kty) -> refs kty.typ) keys

   let pp v sch ppf =
     let keys = metakey :: field_infos sch in
     let defs = match String_map.bindings (subrefs keys) with
       | [] -> []
       | defs ->
           let prs = List.map (fun (key,pr) -> item ~key pr) defs in
           [item ~key:"$defs" @@ obj prs]
     in
     obj (header @ defs @ schema_field :: record_fields (Some v) keys) ppf

  let pp_log ppf log =
    let sch = log_scheme log in
    pp (log_version log) sch ppf

  end
