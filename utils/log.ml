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
  let make ~major ~minor = { major; minor }

  type range = { introduction: t; deprecation: t option; deletion: t option }

  type error =
    | Duplicate_key of string
    | Time_travel of t * t
    | Sealed_version of t

  type base_event =
    | Creation
    | New_key of {name:string; typ:string}
    | Make_required of string
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
    | Make_required name -> fprintf ppf "Newly required %s" name
    | Deprecation name -> fprintf ppf "Deprecation %s" name
    | Seal -> fprintf ppf "Seal"
    | Deletion name -> fprintf ppf "Deletion %s" name
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

type _ extension = ..

type empty = Empty_tag

type polarity = Positive | Negative

type 'a typ =
  | Unit: unit typ
  | Bool: bool typ
  | Int: int typ
  | String: string typ
  | List: 'a typ -> 'a list typ
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
        optional: bool;
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
  item: Format.formatter -> string * typed_val -> unit
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
| List elt -> Format.fprintf ppf "l %a" with_parens elt
| Pair (x,y) -> Format.fprintf ppf "%a*%a" with_parens x with_parens y
| Triple (x,y,z) ->
    Format.fprintf ppf "%a*%a*%a" with_parens x with_parens y with_parens z
| Quadruple (x,y,z,w) ->
  Format.fprintf ppf "*%a*%a*%a*%a"
    with_parens x with_parens y with_parens z with_parens w
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
let key_metadata ~optional update key =
   Key_metadata {
    creation=update.Version.v;
    optional;
    deprecation=None;
    deletion=None;
    typ = key.typ
  }

let new_key ~optional update scheme name typ =
  begin match scheme.polarity with
  | Positive -> ()
  | Negative -> Version.breaking_change update scheme.scheme_name
  end;
  if List.mem_assoc name scheme.keys then
    Version.(error update scheme.scheme_name (Duplicate_key name));
  let key = make_key name typ in
  let metadata = key_metadata ~optional update key in
  scheme.!(key) <- metadata;
  Version.register_event update scheme.scheme_name
    (New_key {
        name;
        typ=Format.asprintf "%s%a" (if optional then "?" else "") pp_typ typ
      };
    );
  key

type _ extension += Version: version extension

let version_ty =
  let pull v = v.Version.major, v.Version.minor in
  Custom { id = Version; pull; default = Pair (Int,Int) }


module type Def = sig
  type id
  type vl
  type definition
  type scheme = id def
  type raw_type = definition
  type t = id log
  type nonrec 'a key = ('a,id) key

  val scheme: scheme
  val raw_type: raw_type typ

  val deprecate: vl Version.update -> 'a key -> unit
  val delete: vl Version.update -> 'a key -> unit
  val seal: vl Version.update -> unit
end

module type Record = sig
  type id
  include Def with type id := id and type definition := id record
  val make_required: vl Version.update -> 'a key -> unit
  val new_field: vl Version.update  -> string -> 'a typ -> 'a key
  val new_field_opt: vl Version.update  -> string -> 'a typ -> 'a key
end

module type Sum = sig
  type id
  include Def with type id := id and type definition := id sum
  val new_constr: vl Version.update -> string -> 'a typ -> 'a -> raw_type
  val new_constr0: vl Version.update -> string -> raw_type
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
  type t = id log
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

type key_status =
  | Valid
  | Deprecated
  | Deleted
  | Future

let key_status version (Key_metadata kmd) =
  if version < kmd.creation then Future
  else match kmd.deprecation, kmd.deletion with
    | _, Some del when del <= version -> Deleted
    | Some dc, _ when dc <= version -> Deprecated
    | _ -> Valid

let active_key version scheme key =
  match key_status version (scheme.!(key)) with
  | Valid | Deprecated -> true
  | Future | Deleted -> false

let make_required u key scheme =
  let Key_metadata r = scheme.!(key) in
  Version.register_event u scheme.scheme_name (Make_required key.name);
 scheme.!(key) <-
    Key_metadata { r with optional = false }

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
  type raw_type = id record
  let scheme = {
    scheme_name = Info.name;
    keys = [];
    polarity=Positive;
  }
  let raw_type = Record scheme

  let () = Version.register_event Info.update Info.name Creation
  let new_field v name ty = new_key ~optional:false v scheme name ty
  let new_field_opt v name ty = new_key ~optional:true v scheme name ty
  let deprecate u k = deprecate_key u k scheme
  let delete u k = delete_key u k scheme
  let make_required u k = make_required u k scheme
  let seal u = seal u scheme
end

module New_sum(Vl:Version_line)(Info:Info with type vl:=Vl.id)() = struct
  include New_local_def ()
  type raw_type = id sum
  let scheme = {
    scheme_name = Info.name;
    keys = [];
    polarity = Negative;
  }
  let raw_type = Sum scheme

  let () = Version.register_event Info.update Info.name Creation
  let new_constr u name ty =
    let k = new_key ~optional:false u scheme name ty in
    fun x -> Constr(k,x)
  let new_constr0 u name =
    let k = new_key ~optional:false u scheme name Unit in
    Enum k

  let deprecate u k = deprecate_key u k scheme
  let delete u k = delete_key u k scheme
  let seal u = seal u scheme
end



(** {1:log_scheme_versionning  Current version of the log } *)



let version_range key scheme =
  let Key_metadata r =  scheme.!(key) in
  {
    Version.introduction = r.creation;
    deprecation = r.deprecation;
    deletion = r.deletion
  }


module Record = struct
  let (^=) k x = Field(k,x)
  let (^=?) k = function
    | None -> []
    | Some x -> [Field(k,x)]

  let field_name (Field (k,_)) = k.name
  let make fields =
    let fields = List.fold_left (fun fields field ->
        Keys.add (field_name field) field fields
      ) Keys.empty fields
    in
    ref fields
  let fields x = !x
end

let is_optional (Key_metadata r) = r.optional

let rec is_empty: type a. a typ -> a -> bool = fun ty x ->
  match ty, x with
  | List _, [] -> true
  | Record _, x -> Keys.for_all is_empty_field !x
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

  let dynamic_get name st =
    Keys.find_opt name (fields st)
    |> Option.map (fun (Field(k,x)) -> V (k.typ,x))

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

module Metadata_versions = New_root()
module Metadata = struct
  let v1 = Metadata_versions.v1
  include New_record(Metadata_versions)(struct
      let name = "metadata"
      let update = v1
    end)()
  let version = new_field v1 "version" version_ty
  module Validity = struct
    include New_sum(Metadata_versions)(struct
        let name = "validity"
        let update = v1
        end
      )()
      let full = new_constr0 v1 "Full"
      let deprecated = new_constr0 v1 "Deprecated"
      let invalid = new_constr0 v1 "Invalid"
      let () = seal v1
  end
  let valid = new_field v1 "valid" Validity.raw_type
  let path = List String
  let invalid_paths = new_field_opt v1 "invalid_paths" (List path)
  let deprecated_paths = new_field_opt v1 "deprecated_paths" (List path)
  let () = seal v1
  let universal_key = make_key "metadata" (Record scheme)
  let metakey = "metadata", key_metadata ~optional:false v1 universal_key
end
let metakey = Metadata.metakey



module Validation = struct

  type path = string list
  type report_paths = { deprecated: path list; invalid: path list }
  let (@^) h l = {
    deprecated = h.deprecated @ l.deprecated;
    invalid = h.invalid @ l.invalid
  }
  let none =  { invalid = []; deprecated=[]}
  let invalid x = { invalid = [x]; deprecated = [] }
  let deprecated x = { deprecated = [x]; invalid = [] }
  let concat_map f l = List.fold_left (fun acc x -> f x @^ acc) none l



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
    | List elt -> possibly_invalid elt
    | Custom r -> possibly_invalid r.default
    | Sum _ -> true
    | Record _ -> true




  let rec record: type id.
    ?toplevel:bool -> version:version -> id def -> id record -> report_paths =
    fun ?(toplevel=false) ~version sch st ->
    let r = fields ~version sch.keys (Record.fields st) in
    (* don't add validation metakeys to empty sublog*)
    if toplevel then begin
      let valid = match List.is_empty r.deprecated, List.is_empty r.invalid with
        | true, true -> Metadata.Validity.full
        | false, true ->Metadata. Validity.deprecated
        | _, false -> Metadata.Validity.invalid
      in
      let metadata =
        let open Record in
        make [
          Metadata.version ^= version;
          Metadata.valid ^= valid;
          Metadata.invalid_paths ^= r.invalid;
          Metadata.deprecated_paths ^= r.deprecated;
        ]
      in
      Store.record st ~key:Metadata.universal_key metadata
    end;
    r
  and fields: type id.
    version:version -> (Keys.key * key_metadata) list -> id field Keys.t
    -> report_paths = fun ~version metadata data ->
    concat_map (fun (k,kmd) ->
        match key_status version kmd with
        | Future | Deleted -> invalid [k]
        | Deprecated ->
            deprecated [k]  @^
            field  ~version ~optional:(is_optional kmd) (Keys.find_opt k data)
        | Valid ->
            field  ~version ~optional:(is_optional kmd) (Keys.find_opt k data)
      ) metadata
  and field: type a.
    version:version -> optional:bool -> a field option -> report_paths =
    fun ~version ~optional k ->
    match optional, k with
    | true, None -> none
    | false, None -> invalid []
    | _, Some (Field (k,v)) -> value ~version v k.typ
  and value: type a. version:version -> a -> a typ -> report_paths =
    fun ~version v typ ->
    match typ with
    | Record m -> record ~version m v
    | Int -> none
    | Bool -> none
    | String -> none
    | Custom _ -> none
    | Unit -> none
    | List elt ->
        if possibly_invalid elt then
          concat_map (fun v -> value ~version v elt) v
        else none
    | Pair (x,y) ->
        let vx, vy = v in
        value ~version vx x @^ value ~version vy y
    | Triple (x,y,z) ->
        let vx, vy, vz = v in
        value ~version vx x
        @^ value ~version vy y
        @^ value ~version vz z
    | Quadruple (x,y,z,w) ->
        let vx, vy, vz, vw = v in
        value ~version vx x
        @^ value ~version vy y
        @^ value ~version vz z
        @^ value ~version vw w
    | Sum def ->
        match v with
        | Enum k ->
            begin match key_status version def.!(k) with
            | Valid -> none
            | Future | Deleted -> invalid [k.name]
            | Deprecated -> deprecated [k.name]
            end
        | Constr(k, v) ->
            begin match key_status version def.!(k) with
            | Valid -> value ~version v k.typ
            | Future | Deleted -> invalid [k.name]
            | Deprecated -> deprecated [k.name] @^ value ~version v k.typ
            end
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
  | List (Custom _ ) -> assert false
  | List (Record sch) -> sch
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
     log.printer.item (key.name, V(key.typ,x))
  | Store st -> Store.record st.data ~key x

let cons key x log =
  match log.mode with
  | Direct _-> set key [x] log
  | Store st -> Store.cons st.data ~key x

let (.%[]<-) log key x = set key x log

let get key log = match log.mode with
  | Direct _ -> None
  | Store st -> Store.get key st.data

let dynamic_get key log = match log.mode with
  | Direct _ -> None
  | Store st -> Store.dynamic_get key st.data

let f key log fmt = Format.kasprintf (fun s -> log.%[key] <- s) fmt
let itemf key log fmt = Format.kasprintf (fun s -> cons key s log) fmt

let d key log fmt = Format_doc.kdoc_printf (fun s -> log.%[key] <- s) fmt
let itemd key log fmt = Format_doc.kdoc_printf (fun s -> cons key s log) fmt

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
  printer = { record = (fun _ _ -> ()); item = (fun _ _ -> ()) };
  mode = Store { out=None; data=Record.make [] }
}



let log_if dlog key flag printer x =
  if flag then
    Format.kasprintf (fun s -> dlog.%[key] <- s) "%a" printer x
