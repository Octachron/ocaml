(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Florian Angeletti, projet Cambium, Inria Paris             *)
(*                                                                        *)
(*   Copyright 2023 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)


module K = struct type t = string let compare = compare end
module Keys = Map.Make(K)

module Version = struct

  type t = { major:int; minor:int }
  type range = { introduction: t; deprecation: t option }

  type error =
    | Duplicate_key of string
    | Time_travel of t * t
    | Sealed_version of t

  type base_event =
    | Creation
    | New_key of {name:string; typ:string}
    | Deprecation of string
    | Deletion of string
    | Seal
    | Error of error

  type event =
    { scheme: string; version:t; event:base_event }
  type _ history = {
    mutable current: t;
    mutable events: event list
  }

  type 'a update = {
    v:t;
    history:'a history;
    minor_update:bool;
  }
  let v x = x.v

  let register_event update scheme event =
    let h = update.history in
    h.events <- { scheme; version=update.v; event} :: h.events

  let error update sch err = register_event update sch (Error err)

  let breaking_change update sch = if update.minor_update then
      error update sch (Sealed_version update.history.current)

  let zeroth = { major = 0; minor = 0}
  let first = { major = 1; minor = 0 }

  let new_version history version =
    let sv = history.current in
    if version <= sv  then begin
      let error=Error (Time_travel (version, sv)) in
      let event = { scheme=""; version=sv; event=error} in
      history.events <- event :: history.events
    end;
    let minor_update = version.major = sv.major in
    history.current <- version;
    { v=version; minor_update; history }

  let current_version history = history.current

  open Format
  let pp_version ppf x = fprintf ppf "v%d.%d" x.major x.minor

  let pp_error ppf = function
    | Time_travel (v,x) ->
        Format.fprintf ppf "Error: future key (%a<%a)" pp_version v pp_version x
    | Duplicate_key s ->
        Format.fprintf ppf "Error: duplicate %s" s
    | Sealed_version v ->
        Format.fprintf ppf "Error: seal breach %a" pp_version v


  let pp_base_event ppf =
    function
    | Creation -> fprintf ppf "Creation"
    | New_key {name;typ} ->
       if typ = "" then fprintf ppf "Key %s" name
      else
       fprintf ppf "Key %s, %s" name typ
    | Deprecation name -> fprintf ppf "Deprecation %S" name
    | Seal -> fprintf ppf "Seal"
    | Deletion name -> fprintf ppf "Deletion %S" name
    | Error e -> pp_error ppf e

  let pp_history ppf h =
    let events = h.events in
    let module Vmap = Map.Make(struct
        type nonrec t = t
        let compare: t -> t -> int = Stdlib.compare
      end)
    in
    let add m e =
      let map_at_v =
        Option.value ~default:Keys.empty (Vmap.find_opt e.version m) in
      let prev = Option.value ~default:[] (Keys.find_opt e.scheme map_at_v) in
      let map_at_v = Keys.add e.scheme (e.event::prev) map_at_v in
      Vmap.add e.version map_at_v m
    in
    let m = List.fold_left add Vmap.empty events in
    let pp_scheme_at_v ppf (scheme_name,events) =
      Format.fprintf ppf "@[<v 2>%s@,%a@]"
        scheme_name
        (pp_print_list pp_base_event) events
    in
    let pp_version ppf (version, map_at_v) =
      Format.fprintf ppf "@[<v 2>%a@," pp_version version;
      pp_print_seq pp_scheme_at_v ppf (Keys.to_seq map_at_v);
      Format.fprintf ppf "@]"
    in
    Format.fprintf ppf "@[<v>%a@]"
      Format.(pp_print_seq pp_version) (Vmap.to_seq m)

end
type version = Version.t = { major:int; minor:int }

type doc = Format_doc.t
type _ extension = ..

type empty = Empty_tag

type polarity = Positive | Negative

type 'a typ =
  | Unit: unit typ
  | Bool: bool typ
  | Int: int typ
  | String: string typ
  | List: {optional:bool; elt:'a typ} -> 'a list typ
  | Option: 'a typ -> 'a option typ
  | Pair: 'a typ * 'b typ -> ('a * 'b) typ
  | Triple: 'a typ * 'b typ * 'c typ -> ('a * 'b * 'c) typ
  | Quadruple: 'a typ * 'b typ * 'c typ * 'd typ ->
      ('a * 'b * 'c * 'd) typ
  | Sum: 'a def -> 'a sum typ
  | Record: 'id def -> 'id record typ
  | Custom: { id :'b extension; pull: ('b -> 'a); default: 'a typ} ->
      'b typ

and ('a,'b) key = {
  name: string;
  typ: 'a typ;
  id: 'a Type.Id.t;
}
and 'a field = Field: ('a,'b) key * 'a -> 'b field
and 'id sum =
  | Constr: ('a,'id) key * 'a -> 'id sum
  | Enum of (unit,'id) key
and key_metadata =
    Key_metadata:
      { typ: 'a typ;
        creation:version;
        deprecation: version option;
        deletion: version option;
      } ->
      key_metadata
and 'a def = {
  scheme_name: string;
  mutable keys: (Keys.key * key_metadata) list;
  polarity: polarity;
}
and 'a record = 'a field Keys.t ref

type ppf_with_close =
  {
    initialized: bool ref;
    ppf: Format.formatter ref;
    close: unit -> unit;
  }

type 'a log =
  {
      mutable redirections: ppf_with_close Keys.t;
      version: version;
      scheme: 'a def;
      settings: Misc.Color.setting option;
      mode: 'a mode;
      printer:printer;
  }
and 'a mode =
  | Direct of ppf_with_close
  | Store of {data:'a record; out:ppf_with_close option}
and typed_record = R: 'a def * 'a record -> typed_record
and typed_val = V: 'a typ * 'a -> typed_val
and printer = {
  record: Format.formatter -> typed_record -> unit;
  value: Format.formatter -> typed_val -> unit
}

let destruct x f  = match x with
  | Enum kt -> f kt.name (V (Unit,()))
  | Constr (kt,x) -> f kt.name (V(kt.typ,x))
let scheme_name x = x.scheme_name
let field_infos d = d.keys
let field_names d = List.map fst d.keys
let log_scheme log = log.scheme
let log_version log = log.version

type 'a t = 'a log

let (.!()<-) scheme key metadata =
  scheme.keys <- (key.name, metadata) :: scheme.keys

let rec pp_typ: type a. Format.formatter -> a typ -> unit = fun ppf -> function
| Unit -> Format.pp_print_string ppf ""
| Int -> Format.pp_print_string ppf "i"
| Bool -> Format.pp_print_string ppf "b"
| String -> Format.pp_print_string ppf "s"
| List r ->
  Format.fprintf ppf "l%s %a"
    (if r.optional then "?" else "")
    with_parens r.elt
| Pair (x,y) -> Format.fprintf ppf "%a*%a" with_parens x with_parens y
| Triple (x,y,z) ->
    Format.fprintf ppf "%a*%a*%a" with_parens x with_parens y with_parens z
| Quadruple (x,y,z,w) ->
  Format.fprintf ppf "*%a*%a*%a*%a"
    with_parens x with_parens y with_parens z with_parens w
| Option elt -> Format.fprintf ppf "?%a" with_parens elt
| Sum def -> Format.fprintf ppf "%s" def.scheme_name
| Record def -> Format.fprintf ppf "%s" def.scheme_name
| Custom r -> pp_typ ppf r.default
and with_parens: type a. Format.formatter -> a typ -> unit = fun ppf elt ->
  let parens_needed =  match elt with
  | Pair _ -> true
  | Triple _ -> true
  | Quadruple _ -> true
  | _ -> false
  in
  if parens_needed then Format.fprintf ppf "(%a)" pp_typ elt else pp_typ ppf elt



let make_key name typ = { name; typ; id = Type.Id.make () }
let key_metadata update key =
   Key_metadata {
    creation=update.Version.v;
    deprecation=None;
    deletion=None;
    typ = key.typ
  }

let new_key update scheme name typ =
  begin match scheme.polarity with
  | Positive -> ()
  | Negative -> Version.breaking_change update scheme.scheme_name
  end;
  if List.mem_assoc name scheme.keys then
    Version.(error update scheme.scheme_name (Duplicate_key name));
  let key = make_key name typ in
  let metadata = key_metadata update key in
  scheme.!(key) <- metadata;
  Version.register_event update scheme.scheme_name
    (New_key {name; typ=Format.asprintf "%a" pp_typ typ});
  key

type _ extension += Version: version extension

let version_ty =
  let pull v = v.Version.major, v.Version.minor in
  Custom { id = Version; pull; default = Pair (Int,Int) }


module type Def = sig
  type id
  type vl
  type scheme = id def
  type log = id t
  type nonrec 'a key = ('a,id) key
  val scheme: scheme
  val deprecate: vl Version.update -> 'a key -> unit
  val delete: vl Version.update -> 'a key -> unit
  val seal: vl Version.update -> unit
end

module type Record = sig
  include Def
  val new_field: vl Version.update  -> string -> 'a typ -> 'a key
end

module type Sum = sig
  include Def
  val new_constr: vl Version.update -> string -> 'a typ -> 'a -> id sum
  val new_constr0: vl Version.update -> string -> id sum
end

module type Version_line = sig
  type id
  val history: id Version.history
  val v1: id Version.update
end


module New_local_def() = struct
  type id
  type nonrec 'a key = ('a,id) key
  type scheme = id def
  type log = id t
end


module New_root() = struct
  type id
  let history = {
    Version.current = Version.zeroth;
    events = [];
  }

  let v1 = Version.new_version history Version.first
end

let (.!()) scheme key =
  match List.assoc_opt key.name scheme.keys with
  | Some x -> x
  | None -> assert false

let active_key version scheme key =
  let Key_metadata m = scheme.!(key) in
  not (version < m.creation) &&
  match m.deletion with
  | None -> true
  | Some del -> version < del

let deprecate_key u key scheme =
  let Key_metadata r = scheme.!(key) in
  Version.register_event u scheme.scheme_name (Deprecation key.name);
  scheme.!(key) <-
    Key_metadata { r with deprecation = Some (Version.v u) }

let delete_key u key scheme =
  let Key_metadata r = scheme.!(key) in
  Version.register_event u scheme.scheme_name (Deletion key.name);
  scheme.!(key) <-
    Key_metadata { r with deletion = Some (Version.v u) }

let seal update scheme =
  Version.register_event update scheme.scheme_name Seal

module type Info = sig
  type vl
  val name: string
  val update: vl Version.update
end


module New_record(Vl:Version_line)(Info:Info with type vl:=Vl.id)() = struct
  include New_local_def ()
  let scheme =
    {
      scheme_name = Info.name;
      keys = [];
      polarity=Positive;
    }
    let () = Version.register_event Info.update Info.name Creation
    let new_field v name ty = new_key v scheme name ty
    let deprecate u k = deprecate_key u k scheme
    let delete u k = delete_key u k scheme
    let seal u = seal u scheme
end

module New_sum(Vl:Version_line)(Info:Info with type vl:=Vl.id)() = struct
  include New_local_def ()
  let scheme =
    {
      scheme_name = Info.name;
      keys = [];
      polarity = Negative;
    }
    let () = Version.register_event Info.update Info.name Creation
    let new_constr u name ty =
      let k = new_key u scheme name ty in
      fun x -> Constr(k,x)
    let new_constr0 u name =
      let k = new_key u scheme name Unit in
      Enum k

    let deprecate u k = deprecate_key u k scheme
    let delete u k = delete_key u k scheme
    let seal u = seal u scheme
end



(** {1:log_scheme_versionning  Current version of the log } *)



let version_range key scheme =
  let Key_metadata r =  scheme.!(key) in
  { Version.introduction = r.creation; deprecation = r.deprecation }


module Record = struct
  let (^=) k x = Field(k,x)
  let field_name (Field (k,_)) = k.name
  let make fields =
    let fields = List.fold_left (fun fields field ->
        Keys.add (field_name field) field fields
      ) Keys.empty fields
    in
    ref fields
  let fields x = !x
end

let is_optional (Key_metadata r) = match r.typ with
  | List r -> r.optional
  | Option _ -> true
  | _ -> false

let rec is_empty: type a. a typ -> a -> bool = fun ty x ->
  match ty, x with
  | List {optional=true; elt=_ }, [] -> true
  | Option _, None -> true
  | Option ty, Some x -> is_empty ty x
  | Record _, x ->
      Keys.for_all is_empty_field !x
  | _ -> false
and is_empty_field: type a. _ -> a field -> bool =
  fun _ (Field (kt,x)) -> is_empty kt.typ x

module Store = struct
  open Record
  let record:
    type ty s. s record -> key:(ty,s) key -> ty -> unit =
      fun store ~key x ->
        store := Keys.add key.name (key^=x) !store


  let get (type a b) (key: (a,b) key) (st:b record): a option =
    match Keys.find_opt key.name (fields st) with
    | None -> None
    | Some (Field(k,x)) ->
        match Type.Id.provably_equal k.id key.id with
        | None -> None
        | Some Type.Equal -> Some x

  let cons: type ty s. s record -> key:(ty list,s) key -> ty -> unit =
    fun store ~key x ->
      let l = match get key store with
        | None -> [x]
        | Some l -> x :: l
      in
      store := Keys.add key.name (key^=l) (fields store)

  let trim keys prod =
     let field key  =
        match Keys.find_opt key (fields prod) with
        | None -> None
        | Some (Field (kt,x)) as c ->
            if is_empty kt.typ x then None else c
      in
      List.filter_map field (List.rev keys)
end
let fields keys r =
  let keys = Store.trim keys r in
  List.map (fun (Field (k,v)) -> k.name, V(k.typ,v)) keys

module Compiler_log_version = New_root()

module Metadata = struct
  let v1 = Compiler_log_version.v1
  include New_record(Compiler_log_version)(struct
      let name = "metadata"
      let update = v1
    end)()
  let version = new_field v1 "version" version_ty
  let valid = new_field v1 "valid" Bool
  let path = List { optional = false; elt = String}
  let invalid_paths =
    new_field v1 "invalid_paths" (List {optional=true; elt=path})
  let () = seal v1
  let universal_key = make_key "metadata" (Record scheme)
  let metakey = "metadata", key_metadata v1 universal_key
end
let metakey = Metadata.metakey

module Validation = struct

  type path = string list

  let rec possibly_invalid: type a. a typ -> bool = function
    | Unit -> false
    | Int -> false
    | String -> false
    | Bool -> false
    | Pair (x,y) -> possibly_invalid x || possibly_invalid y
    | Triple (x,y,z) ->
        possibly_invalid x || possibly_invalid y || possibly_invalid z
    | Quadruple (x,y,z,w) ->
        possibly_invalid x
        || possibly_invalid y
        || possibly_invalid z
        || possibly_invalid w
    | List r -> possibly_invalid r.elt
    | Custom r -> possibly_invalid r.default
    | Option e -> possibly_invalid e
    | Sum _ -> true
    | Record _ -> true

  let rec record: type id.
    ?toplevel:bool -> version:version -> id def -> id record -> path list =
    fun ?(toplevel=false) ~version sch st ->
      (* don't add validation metakeys to empty sublog*)
      let invalid = fields ~version sch.keys (Record.fields st) in
      if toplevel then begin
        let metadata =
          let open Record in
          make [
            Metadata.version ^= version;
            Metadata.valid ^= List.is_empty invalid;
            Metadata.invalid_paths ^= invalid;
          ]
        in
        Store.record st ~key:Metadata.universal_key metadata
      end;
      invalid

  and fields: type id.
    version:version -> (Keys.key * key_metadata) list -> id field Keys.t
    -> path list =
    fun ~version metadata data ->
      List.concat_map (fun  (k,kmd) ->
        List.map (fun path -> k :: path)
        @@ field  ~version ~optional:(is_optional kmd)
        @@ Keys.find_opt k data
      ) metadata
  and field: type a.
    version:version -> optional:bool -> a field option -> path list =
    fun ~version ~optional k ->
      match optional, k with
      | true, None -> []
      | _, None -> [[]]
      | _, Some (Field (k,v)) -> value ~version v k.typ
  and value: type a. version:version -> a -> a typ -> path list =
    fun ~version v typ ->
      match typ with
      | Record m -> record ~version m v
      | Int -> []
      | Bool -> []
      | String -> []
      | Custom _ -> []
      | Unit -> []
      | List {elt; _} ->
          if possibly_invalid elt then
            List.concat_map (fun v -> value ~version v elt) v
          else []
      | Option m ->
          Option.value ~default:[]
            (Option.map (fun v -> value ~version v m) v)
      | Pair (x,y) ->
          let vx, vy = v in
          value ~version vx x @ value ~version vy y
      | Triple (x,y,z) ->
          let vx, vy, vz = v in
          value ~version vx x
          @ value ~version vy y
          @ value ~version vz z
      | Quadruple (x,y,z,w) ->
          let vx, vy, vz, vw = v in
          value ~version vx x
          @ value ~version vy y
          @ value ~version vz z
          @ value ~version vw w
      | Sum def ->
          begin match v with
          | Enum _ -> []
          | Constr(k, v) ->
              if active_key version def k then value ~version v k.typ
              else [[k.name]]
          end

end

module Structured_text = struct
  module Doc = Format_doc.Doc
  let v1 = Compiler_log_version.v1
  module Box_type = struct
    include New_sum(Compiler_log_version)
        (struct
          let name = "box_type"
          let update = v1
        end
        )()
    let h = new_constr0 v1 "H"
    let v = new_constr0 v1 "V"
    let hv = new_constr0 v1 "HV"
    let hov = new_constr0 v1 "HoV"
    let b = new_constr0 v1 "B"
    let () = seal v1
    type _ extension += Box_type: Doc.box_type extension
    let typ =
      let pull = function
        | Doc.H -> h
        | Doc.V -> v
        | Doc.HoV -> hov
        | Doc.HV -> hv
        | Doc.B -> b
      in
      Custom { id = Box_type; pull; default = Sum scheme}
  end

  module Format_tag = struct
    include New_sum(Compiler_log_version)
        (struct
          let name = "format_tag"
          let update = v1
        end
        )()


    let unknown = new_constr v1 "<Unknown>" String
    let string_tag = new_constr v1 "String_tag" String

    type _ extension += Format_tag: Format.stag extension
    let map: (Obj.Extension_constructor.t, Format.stag -> id sum) Hashtbl.t =
      Hashtbl.create 5
    let register_tag ext conv = Hashtbl.replace map ext conv
    let typ =
      let pull = function
        | Format.String_tag s -> string_tag s
        | x ->
            let ext = Obj.Extension_constructor.of_val x in
            match Hashtbl.find map ext with
            | exception Not_found ->
                unknown (Obj.Extension_constructor.name ext)
            | f -> f x
      in
      Custom { id = Format_tag; pull; default = Sum scheme}

    let register_tag0 v ext =
      let name = Obj.Extension_constructor.name ext in
      let name = match String.rindex name '.' with
        | exception Not_found -> name
        | dot -> String.sub name (dot+1) (String.length name - dot -1)
      in
      let constr = new_constr0 v name in
      register_tag ext (fun _ -> constr)

   let () =
      Array.iter (register_tag0 v1)
        Misc.Style.[|
          [%extension_constructor Error];
          [%extension_constructor Warning];
          [%extension_constructor Loc];
          [%extension_constructor Inline_code];
          [%extension_constructor Hint];
          [%extension_constructor Deletion];
          [%extension_constructor Insertion];
          [%extension_constructor Modification];
          [%extension_constructor Preservation];
        |];
      seal v1

  end


  include New_sum(Compiler_log_version)
    (struct
      let name = "structured_text"
      let update = v1
    end)
    ()

  let text = new_constr v1 "Text" String
  let with_size = new_constr v1 "With_size" Int
  let open_box = new_constr v1 "Open_box" (Pair(Box_type.typ,Int))
  let close_box = new_constr0 v1 "Close_box"
  let open_tag = new_constr v1 "Open_tag" Format_tag.typ
  let close_tag = new_constr0 v1 "Close_tag"
  let open_tbox = new_constr0 v1 "Open_tbox"
  let close_tbox = new_constr0 v1 "Close_tbox"
  let tab_break = new_constr v1 "Tab_break" (Pair(Int,Int))
  let set_tab = new_constr0 v1 "Set_tab"
  let simple_break = new_constr v1 "Simple_break" (Pair(Int,Int))
  let break =
    let alt = Triple(String,Int,String) in
    new_constr v1 "Break" (Pair(alt,alt))
  let flush = new_constr v1 "Flush" Bool
  let newline = new_constr0 v1 "Newline"
  let if_newline = new_constr0 v1 "If_newline"

  let deprecated = new_constr0 v1 "<deprecated>"
  let () = seal v1

  type _ extension += Doc: Doc.t extension
  let typ =
    let elt_pull = function
      | Doc.Text x -> text x
      | Doc.With_size x -> with_size x
      | Doc.Open_box r -> open_box (r.kind, r.indent)
      | Doc.Close_box -> close_box
      | Doc.Open_tag t -> open_tag t
      | Doc.Close_tag -> close_tag
      | Doc.Open_tbox -> open_tbox
      | Doc.Close_tbox -> close_tbox
      | Doc.Tab_break t -> tab_break (t.width,t.offset)
      | Doc.Set_tab -> set_tab
      | Doc.Simple_break r -> simple_break (r.spaces, r.indent)
      | Doc.Break r -> break (r.fits, r.breaks)
      | Doc.Flush r -> flush r.newline
      | Doc.Newline -> newline
      | Doc.If_newline -> if_newline
      | Doc.Deprecated _ -> deprecated
    in
    let default = List {optional=false; elt = Sum scheme} in
    let pull d =
      List.rev @@
      Format_doc.Doc.fold (fun l x -> elt_pull x :: l ) [] d in
    Custom {id = Doc; default; pull }

  let register_tag = Format_tag.register_tag
  let register_tag0 = Format_tag.register_tag0

 end

let make ~structured ~printer settings version scheme ppf =
  let mode =
    let out = {initialized=ref false; ppf; close=ignore} in
    if structured then Store {data=Record.make []; out= Some out}
    else Direct out
  in
  {
    redirections = Keys.empty;
    settings;
    version;
    printer;
    mode;
    scheme;
  }

let redirect log key ?(close=ignore) ppf  =
  log.redirections <-
    Keys.add key.name {initialized=ref false;ppf;close} log.redirections

let key_scheme: type a b. (a record,b) key -> a def  = fun key ->
  match key.typ with
  | Custom _ -> assert false
  | Record sch -> sch

let item_key_scheme: type a b. (a record list,b) key -> a def  = fun key ->
  match key.typ with
  | Custom _ -> assert false
  | List { elt=Custom _ ; _ } -> assert false
  | List { elt=Record sch; _ } -> sch
  | _ -> .

let option_key_scheme: type a b. (a record option,b) key -> a def  = fun key ->
  match key.typ with
  | Custom _ -> assert false
  | Option (Custom _) -> assert false
  | Option (Record sch) -> sch
  | _ -> .

let generic_detach key_scheme ~store ~lift ~extract log key =
  let out = Keys.find_opt key.name log.redirections in
  let mode = match log.mode with
    | Direct d ->
        let out = Option.value ~default:d out in
        Direct out
    | Store {data=st; out=st_out} ->
        let data =
          match Option.bind (Store.get key st) extract with
          | Some data -> data
          | None ->
              let data = Record.make [] in
              store st ~key (lift data); data
        in
        let out = match out with
          | Some _ -> out
          | _ -> st_out
        in Store { data; out }
  in
  let child =
    { scheme=key_scheme key;
      mode;
      printer=log.printer;
      version = log.version;
      settings = log.settings;
      redirections = Keys.empty;
    } in
  child

let some x = Some x
let detach log key =
  generic_detach key_scheme
    ~store:Store.record ~lift:Fun.id ~extract:some log key
let detach_item log key =
  generic_detach item_key_scheme
    ~store:Store.cons
    ~lift:Fun.id
    ~extract:(Fun.const None)
    log key
let detach_option log key =
  generic_detach option_key_scheme
    ~store:Store.record
    ~lift:some
    ~extract:Fun.id log key

module Fmt = struct
   let init color ppf =
    let color = Misc.Style.enable_color color in
    Misc.Style.set_tag_handling ~color !ppf;
    Format.fprintf !ppf "@[<v>"

  let flush c =
    Format.fprintf !(c.ppf) "%!"

  let separate c = Format.pp_print_newline !(c.ppf) ()

  let close c =
    if not !(c.initialized) then ()
    else (Format.fprintf !(c.ppf) "@,@]%!"; c.initialized := false);
    c.close ()
end

let set key x log =
  if not (active_key log.version log.scheme key) then () else
  match log.mode with
  | Direct d ->
    let r = Keys.find_opt key.name log.redirections in
    let out = Option.value ~default:d r in
    let ppf = !(out.ppf) in
    if not !(d.initialized) then
      (Fmt.init log.settings out.ppf ; d.initialized := true);
    Format.fprintf ppf "@[<v>%a@,@]%!"
     log.printer.value (V(key.typ,x))
  | Store st -> Store.record st.data ~key x

let cons key x log =
  match log.mode with
  | Direct _-> set key [x] log
  | Store st -> Store.cons st.data ~key x

let (.%[]<-) log key x = set key x log

let f key log fmt = Format.kasprintf (fun s -> log.%[key] <- s) fmt
let itemf key log fmt = Format.kasprintf (fun s -> cons key s log) fmt

let d key log fmt = Format_doc.kdoc_printf (fun s -> log.%[key] <- s) fmt
let itemd key log fmt = Format_doc.kdoc_printf (fun s -> cons key s log) fmt

let o key log fmt = Format_doc.kdoc_printf (fun s -> log.%[key] <- Some s) fmt


let flush: type a. a log -> unit = fun log ->
  begin match log.mode with
  | Direct d -> Fmt.flush d
  | Store st ->
      let _valid =
        Validation.record ~toplevel:true ~version:log.version log.scheme st.data
      in
      Option.iter (fun out ->
          let ppf = !(out.ppf) in
          log.printer.record ppf (R(log.scheme, st.data))
        ) st.out;
        st.data := Keys.empty
  end;
  Keys.iter (fun _ -> Fmt.flush) log.redirections

let separate log = match log.mode with
  | Direct d -> Fmt.separate d
  | _ -> ()

let close: type a. a log -> unit = fun log ->
  begin match log.mode with
  | Direct d -> Fmt.close d
  | Store { out = Some out } -> out.close ()
  | Store _ -> ()
  end;
  Keys.iter (fun _ -> Fmt.close) log.redirections

let close log = flush log; close log

let replay source dest =
  match source.mode with
  | Direct _ -> ()
  | Store st ->
      Keys.iter
        (fun _ (Field(key,x)) -> dest.%[key] <- x )
        (Record.fields st.data)

(** {1:log_creation }*)

let tmp scheme = {
  settings = None;
  redirections = Keys.empty;
  version = {major=0; minor=0};
  scheme;
  printer = { record = (fun _ _ -> ()); value = (fun _ _ -> ()) };
  mode = Store { out=None; data=Record.make [] }
}


let slist = List { optional=true; elt=String }

module type Compiler_record = Record with type vl := Compiler_log_version.id
module type Compiler_sum = Sum with type vl := Compiler_log_version.id

module Debug = struct
  let v1 = Compiler_log_version.v1
  include New_record(Compiler_log_version)
      (struct
        let name = "debug"
        let update = v1
      end)
      ()
  let parsetree = new_field v1 "parsetree" (Option String)
  let source = new_field v1 "source" (Option String)
  let typedtree = new_field v1 "typedtree" (Option String)
  let shape = new_field v1 "shape" (Option String)
  let instr = new_field v1 "instr" (Option String)
  let lambda = new_field v1 "lambda" (Option String)
  let raw_lambda = new_field v1 "raw_lambda" (Option String)
  let flambda = new_field v1 "flambda" slist
  let raw_flambda = new_field v1 "raw_flambda" slist
  let clambda = new_field v1 "clambda" slist
  let raw_clambda = new_field v1 "raw_clambda" slist
  let cmm = new_field v1 "cmm" slist
  let remove_free_vars_equal_to_args =
    new_field v1 "remove-free-vars-equal-to-args" slist
  let unbox_free_vars_of_closures =
    new_field v1 "unbox-free-vars-of-closures" slist
  let unbox_closures = new_field v1 "unbox-closures" slist
  let unbox_specialised_args = new_field v1 "unbox-specialised-args" slist
  let mach = new_field v1 "mach" slist
  let linear = new_field v1 "linear" slist
  let cmm_invariant = new_field v1 "cmm_invariant" (Option String)
  let profile = new_field v1 "profile" (Option String)
  let () = seal v1
end

module Error =
    New_record(Compiler_log_version)
      (struct
        let name = "error_report"
        let update = Compiler_log_version.v1
      end)
      ()

module Compiler = struct
  let v1 = Compiler_log_version.v1
  include New_record(Compiler_log_version)
      (struct
        let name = "compiler"
        let update = v1
      end)
      ()
  let debug = new_field v1  "debug" (Option (Record Debug.scheme))
end

let doc = Structured_text.typ
let dloc = List { optional=true; elt = Structured_text.typ}
module Toplevel = struct
  let v1 = Compiler_log_version.v1
  include New_record(Compiler_log_version)
      (struct
        let name = "toplevel"
        let update = v1
      end)
      ()
  let v1 = Compiler_log_version.v1
  let output = new_field v1 "output" doc
  let backtrace = new_field v1 "backtrace" (Option doc)
  let compiler_log =
    new_field v1 "compiler_log" (Option (Record Compiler.scheme))
  let errors = new_field v1 "errors" dloc
  let trace = new_field v1 "trace" dloc
  let () = seal v1
end

let log_if dlog key flag printer x =
  if flag then
    Format.kasprintf (fun s -> dlog.%[key] <- Some (s)) "%a" printer x
