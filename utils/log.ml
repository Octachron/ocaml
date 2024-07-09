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
  type version = t
  let make ~major ~minor = { major; minor }

  module Lifetime = struct
    type t = {
      refinement: version option;
      creation: version option;
      expansion: version option;
      deprecation: version option;
      deletion: version option;
    }
    type point =
      | Refinement
      | Creation
      | Expansion
      | Deprecation
      | Deletion
      | Future

    let next = function
      | Refinement -> Creation
      | Creation -> Expansion
      | Expansion -> Deprecation
      | Deprecation -> Deletion
      | Deletion -> Deletion
      | Future -> Future

    let prev = function
      | Refinement -> Refinement
      | Creation -> Refinement
      | Expansion -> Creation
      | Deprecation -> Expansion
      | Deletion -> Deprecation
      | Future -> Future

    let get r = function
      | Refinement -> r.refinement
      | Creation -> r.creation
      | Expansion -> r.expansion
      | Deprecation -> r.deprecation
      | Deletion -> r.deletion
      | Future -> None

    let rec after r p =
      if p = Deletion then None
      else match get r p with
        | None -> after r (next p)
        | Some x -> Some (p, x)

    let rec last_change r p =
      if p = Refinement then Refinement
        else match get r p with
          | None -> last_change r (prev p)
          | Some _ -> p

    let rec stage v current r =
      if current = Deletion then current else
      match after r (next current) with
      | None -> current
      | Some (p,v1) ->
          if v < v1 then p else stage v p r


  end

  let stage r = Lifetime.(last_change r Deletion)

  let stage_at v r =
    let open Lifetime in
    match v, after r Refinement with
    | Some _, None -> assert false
    | None, _ -> Creation
    | Some v, Some (p,v1) ->
        if v < v1 then Future else Lifetime.stage v p r


  let range ?deprecation ?deletion ?expansion creation ={
    Lifetime.refinement = None; creation=Some creation;
    expansion; deprecation; deletion
  }

  let prerange ?deprecation ?deletion ?expansion ?creation r = {
    Lifetime.refinement=Some r; creation;
    expansion; deprecation; deletion
  }


  type error =
    | Duplicate_key of string
    | Time_travel of t * t
    | Inconsistent_change of Lifetime.t * string
    | Invalid_constructor_expansion of string
    | Sealed_version of t

  type base_event =
    | Refinement of {base_name:string; new_name:string; typ:string}
    | Creation
    | New_key of {name:string; typ:string}
    | Make_required of string
    | Expansion of {name:string; expansion:string}
    | Deprecation of string
    | Deletion of string
    | Seal
    | Error of error

  type event =
    { scheme: string; version:t; event:base_event }
  type _ history = {
    mutable current: t;
    events: event Dynarray.t
  }

  type 'a update = {
    v:t;
    history:'a history;
    minor_update:bool;
  }
  let v x = x.v

  let register_event update scheme event =
    let h = update.history in
    Dynarray.add_last h.events { scheme; version=update.v; event}

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
      Dynarray.add_last history.events event
    end;
    let minor_update = version.major = sv.major in
    history.current <- version;
    { v=version; minor_update; history }

  let current_version history = history.current
  let events history = Dynarray.to_seq history.events

  let pp ppf x = Format.fprintf ppf "v%d.%d" x.major x.minor

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
  | Custom: {
      id :'b extension;
      pull: (Version.t option -> 'b -> 'a);
      default: 'a typ
    } -> 'b typ
and ('a,'b) field = {
  name:string;
  typ:'a typ;
  id: 'a Type.Id.t;
  range:Version.Lifetime.t
}
and 'a bound_field = Field: ('a,'b) field * 'a -> 'b bound_field
and 'id sum = Constr: { name:string; typ:'a typ; arg: 'a} -> 'id sum
and any_typ = T: 'a typ -> any_typ
and label_metadata = {
  ltyp: any_typ;
  optional: bool;
  status:Version.Lifetime.t;
}
and 'a def = {
  scheme_name: string;
  mutable keys: (Keys.key * label_metadata) list;
  polarity: polarity;
}
and 'a record = 'a bound_field Keys.t ref

type ppf_with_close =
  {
    initialized: bool ref;
    ppf: Format.formatter ref;
    close: unit -> unit;
  }

type 'a log =
  {
      mutable redirections: ppf_with_close Keys.t;
      version: version option;
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

let destruct (Constr c) f = f c.name (V(c.typ,c.arg))
let scheme_name x = x.scheme_name
let field_infos d = d.keys
let field_names d = List.map fst d.keys
let log_scheme log = log.scheme
let log_version log = log.version

type 'a t = 'a log

let (.!()<-) scheme name metadata =
  scheme.keys <-
    (name, metadata) ::
    List.filter (fun (k,_) -> k <> name) scheme.keys

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

let label_metadata ~optional update typ = {
    status = Version.range (Version.v update);
    optional;
    ltyp = T typ
  }

let register_label_metadata ~optional update scheme name typ =
  begin match scheme.polarity with
  | Positive -> ()
  | Negative -> Version.breaking_change update scheme.scheme_name
  end;
  if List.mem_assoc name scheme.keys then
    Version.(error update scheme.scheme_name (Duplicate_key name));
  let metadata = label_metadata ~optional update typ in
  scheme.!(name) <- metadata;
  Version.register_event update scheme.scheme_name
    (New_key {
        name;
        typ=Format.asprintf "%s%a" (if optional then "?" else "") pp_typ typ
      };
    )

type _ extension += Version: version extension

let version_ty =
  let pull _ v = v.Version.major, v.Version.minor in
  Custom { id = Version; pull; default = Pair (Int,Int) }


module type Def = sig
  type id
  type vl
  type 'a label
  type definition
  type scheme = id def
  type raw_type = definition
  type t = id log

  val scheme: scheme
  val raw_type: raw_type typ

  val deprecate: vl Version.update -> 'a label -> 'a label
  val delete: vl Version.update -> 'a label -> 'a label
  val seal: vl Version.update -> unit
end

module type Record = sig
  type id
  type nonrec 'a field = ('a,id) field
  include Def
    with type id := id
     and type definition := id record
     and type 'a label :='a field
  val new_field:
    ?opt:bool -> vl Version.update  -> string -> 'a typ -> 'a field
  val new_field_opt: vl Version.update  -> string -> 'a typ -> 'a field
  val make_required: vl Version.update -> 'a field -> unit
end

type ('elt,'id) constructor =
  { cname: string;
    typ: 'elt typ;
    projection: ('elt,'id) constructor_projection option;
  }
and ('current,'id) constructor_projection =
  | Proj: {
      map: 'current -> 'old;
      old: ('old,'id) constructor;
      version: Version.t;
    } -> ('current,'id) constructor_projection

let is_expansion c (Proj p) = c.cname = p.old.cname

let rec project: type t id. Version.t -> (t,id) constructor -> t -> id sum =
  fun v c x ->
  match c.projection with
  | None -> Constr { name = c.cname; typ=c.typ; arg=x}
  | Some (Proj p) ->
      if v >= p.version then  Constr { name = c.cname; typ=c.typ; arg=x}
      else project v p.old (p.map x)

let app v c x =
  match v with
  | None -> Constr {name=c.cname; typ=c.typ; arg=x}
  | Some v -> project v c x

module type Sum = sig
  type id
  type 'a constructor
  include Def
    with type id := id
     and type definition := id sum
     and type 'a label := 'a constructor
  val app: Version.t option -> 'a constructor -> 'a -> raw_type

  val refine:
    vl Version.update -> 'a constructor -> ('b -> 'a)
    -> string -> 'b typ -> 'b constructor
  val new_constr: vl Version.update -> string -> 'a typ -> 'a constructor
  val new_constr0: vl Version.update -> string -> unit constructor
  val publish: vl Version.update -> 'a constructor -> 'a constructor
  val expand:
    vl Version.update -> 'a constructor -> ('b->'a) -> 'b typ -> 'b constructor

end

module type Version_line = sig
  type id
  val history: id Version.history
  val v1: id Version.update
end


module New_local_def() = struct
  type id
  type scheme = id def
  type t = id log
end


module New_root() = struct
  type id
  let history = {
    Version.current = Version.zeroth;
    events = Dynarray.create ();
  }

  let v1 = Version.new_version history Version.first
end

let (.?()) scheme lbl = List.assoc_opt lbl scheme.keys

module Lv = Version.Lifetime
 let inconsistent_if_not_deprecated u scheme_name key range =
   match range.Version.Lifetime.deprecation, range.Lv.deletion with
   | Some _ , None -> ()
   | None, _ | _, Some _ ->
       Version.error u scheme_name (Inconsistent_change (range,key))

 let inconsistent_if_inactive u scheme_name key range =
   match range.Lv.deprecation with
   | None -> ()
   | Some _ ->  Version.error u scheme_name (Inconsistent_change (range,key))

let (let&?) x f = Option.iter f x

let make_required u f scheme =
  let&? kmd = scheme.?(f.name) in
  inconsistent_if_inactive u scheme.scheme_name f.name kmd.status;
  Version.register_event u scheme.scheme_name (Make_required f.name);
  scheme.!(f.name) <- { kmd with optional = false }

let register_constructor_expansion u old new_typ scheme =
  let&? kmd = scheme.?(old.cname) in
  inconsistent_if_inactive u scheme.scheme_name old.cname kmd.status;
  begin match old.projection with
  | None -> ()
  | Some p ->
      if is_expansion old p then
        Version.error u scheme.scheme_name
          (Invalid_constructor_expansion old.cname)
  end;
  Version.register_event u scheme.scheme_name
    (Expansion {name=old.cname;
                expansion = Format.asprintf "%a" pp_typ new_typ});
  let status = { kmd.status with expansion = Some (Version.v u) } in
  scheme.!(old.cname) <- { kmd with status; ltyp=T new_typ }


let register_constructor_refinement u old new_name new_typ scheme =
  let&? kmd = scheme.?(old.cname) in
  inconsistent_if_inactive u scheme.scheme_name old.cname kmd.status;
  Version.register_event u scheme.scheme_name
    (Refinement {
        base_name=old.cname;
        new_name;
        typ = Format.asprintf "%a" pp_typ new_typ
      }
    );
  let status = Version.prerange (Version.v u) in
  let lmd = label_metadata ~optional:false u new_typ in
  scheme.!(new_name) <- { lmd with status }


let deprecate_lbl u lbl scheme =
  let&? kmd = scheme.?(lbl) in
  inconsistent_if_inactive u scheme.scheme_name lbl kmd.status;
  Version.register_event u scheme.scheme_name (Deprecation lbl);
  let status = { kmd.status with deprecation = Some (Version.v u) } in
  scheme.!(lbl) <- { kmd with status }

let delete_lbl u lbl scheme =
  let&? kmd = scheme.?(lbl) in
  inconsistent_if_not_deprecated u scheme.scheme_name lbl kmd.status;
  Version.register_event u scheme.scheme_name (Deletion lbl);
  let status = { kmd.status with deletion = Some (Version.v u) } in
  scheme.!(lbl) <- { kmd with status }

let seal update scheme =
  Version.register_event update scheme.scheme_name Seal

module type Info = sig
  type vl
  val name: string
  val update: vl Version.update
end

module Record = struct
  type 'a bfield = Version.t option -> 'a bound_field option
  let (^=) f x v =
    match Version.stage_at v f.range with
    | Refinement | Creation | Expansion | Deprecation -> Some (Field(f,x))
    | Future | Deletion -> None
  let (^=?) f x v = match x with
    | None -> None
    | Some x -> (f ^= x) v

  let field_name (Field (f,_)) = f.name
  let make v fields =
    let fields = List.fold_left (fun fields field ->
        match field v with
        | None -> fields
        | Some field ->  Keys.add (field_name field) field fields
      ) Keys.empty fields
    in
    ref fields
  let fields x = !x
end


module New_record(Vl:Version_line)(Info:Info with type vl:=Vl.id)() = struct
  include New_local_def ()
  type nonrec 'a field = ('a,id) field
  type raw_type = id record
  let scheme = {
    scheme_name = Info.name;
    keys = [];
    polarity=Positive;
  }
  let raw_type = Record scheme

  let () = Version.register_event Info.update Info.name Creation

  let new_field ?(opt=false) (type t) v name (ty:t typ): t field =
    register_label_metadata ~optional:opt v scheme name ty;
    {
      name;
      typ = ty;
      id = Type.Id.make ();
      range = Version.range (Version.v v)
    }
  let new_field_opt v name ty = new_field ~opt:true v name ty
  let deprecate u f =
    deprecate_lbl u f.name scheme;
    let range = { f.range with deprecation = Some (Version.v u) } in
    { f with range }
  let delete u f =
    delete_lbl u f.name scheme;
    let range = { f.range with deletion = Some (Version.v u) } in
    { f with range }

  let make_required u f = make_required u f scheme
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
  type nonrec 'a constructor = ('a,id) constructor
  let () = Version.register_event Info.update Info.name Creation
  let new_constr u name (ty:'a typ): 'a constructor  =
    register_label_metadata ~optional:false u scheme name ty;
    { cname = name;
      typ = ty;
      projection = None;
    }
  let new_constr0 u name = new_constr u name Unit
  let app = app

  let expand u old map new_ty =
    let () = register_constructor_expansion u old new_ty scheme in
    let projection = Some(Proj {map;old;version=Version.v u}) in
    { old with typ=new_ty; projection }

  let refine u old map new_name new_ty =
    let () = register_constructor_refinement u old new_name new_ty scheme in
    let projection = Some(Proj {map;old;version=Version.v u}) in
    { cname=new_name; typ=new_ty; projection }

  let publish u c =
    let version = Version.v u in
    let () =
      let&? kmd= scheme.?(c.cname) in
      let status = { kmd.status with creation = Some version } in
      scheme.!(c.cname) <- { kmd with status }
    in
    { c with projection = None }

  let deprecate u c = deprecate_lbl u c.cname scheme; c
  let delete u c = delete_lbl u c.cname scheme; c
  let seal u = seal u scheme
end

(** {1:log_scheme_versionning  Current version of the log } *)

let version_range field = field.range

let is_optional r = r.optional

let rec is_empty: type a. a typ -> a -> bool = fun ty x ->
  match ty, x with
  | List _, [] -> true
  | Record _, x -> Keys.for_all is_empty_field !x
  | _ -> false
and is_empty_field: type a. _ -> a bound_field -> bool =
  fun _ (Field (kt,x)) -> is_empty kt.typ x

module Store = struct
  open Record
  let record:
    type ty s. s record -> Version.t option -> field:(ty,s) field -> ty -> unit
    = fun store v ~field x ->
        let name = field.name in
        Option.iter (fun field ->
        store := Keys.add name field !store
        ) ((field^=x) v)

  let get (type a b) (field: (a,b) field) (st:b record): a option =
    match Keys.find_opt field.name (fields st) with
    | None -> None
    | Some (Field(f,x)) ->
        match Type.Id.provably_equal f.id field.id with
        | None -> None
        | Some Type.Equal -> Some x

  let dynamic_get name st =
    Keys.find_opt name (fields st)
    |> Option.map (fun (Field(k,x)) -> V (k.typ,x))

  let cons: type ty s.
    s record -> Version.t option -> field:(ty list,s) field -> ty -> unit =
    fun store v ~field x ->
      let l = match get field store with
        | None -> [x]
        | Some l -> x :: l
      in
      let f = (field ^= l) v in
      Option.iter (fun bfield ->
          store := Keys.add field.name bfield (fields store)
        ) f

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
  let valid: Validity.raw_type field = new_field v1 "valid" Validity.raw_type
  let path = List String
  let invalid_paths = new_field ~opt:true v1 "invalid_paths" (List path)
  let deprecated_paths = new_field_opt v1 "deprecated_paths" (List path)
  let () = seal v1
  let universal_field () =
      {
        range = Version.range (Version.v v1);
        name = "metadata";
        typ = raw_type;
        id = Type.Id.make ()
      }
  let metakey =
    "metadata",
    label_metadata ~optional:false v1 raw_type
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
  let qualify name l = {
    deprecated = List.map (List.cons name) l.deprecated;
    invalid = List.map (List.cons name) l.invalid;
  }
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
      let v1 = Some (Version.v Metadata.v1) in
      let valid = app v1 valid () in
      let metadata =
        let open Record in
        make v1 [
          Metadata.version ^= version;
          Metadata.valid ^= valid;
          Metadata.invalid_paths ^= r.invalid;
          Metadata.deprecated_paths ^= r.deprecated;
        ]
      in
      Store.record st v1 ~field:(Metadata.universal_field ()) metadata
    end;
    r
  and fields: type id.
    version:version -> (Keys.key * label_metadata) list -> id bound_field Keys.t
    -> report_paths = fun ~version metadata data ->
    concat_map (fun (k, kmd) ->
        match Version.stage_at (Some version) kmd.status with
        | Future | Deletion -> invalid [k]
        | Deprecation ->
            deprecated [k]  @^
            field  ~version ~optional:(is_optional kmd) k (Keys.find_opt k data)
        | Refinement | Creation | Expansion ->
            field  ~version ~optional:(is_optional kmd) k (Keys.find_opt k data)
      ) metadata
  and field: type a.
    version:version -> optional:bool -> string -> a bound_field option
    -> report_paths = fun ~version ~optional name k ->
    match optional, k with
    | true, None -> none
    | false, None -> invalid [name]
    | _, Some (Field (k,v)) ->
        qualify name (value ~version v k.typ)
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
        let Constr c = v in
        match def.?(c.name) with
        | None -> none
        | Some lmd ->
            begin match Version.stage_at (Some version) lmd.status with
            | Refinement | Creation | Expansion -> value ~version c.arg c.typ
            | Future | Deletion -> invalid [c.name]
            | Deprecation -> deprecated [c.name] @^ value ~version c.arg c.typ
            end
end


let make ~structured ~printer settings version scheme ppf =
  let mode =
    let out = {initialized=ref false; ppf; close=ignore} in
    if structured then Store {data=Record.make version []; out= Some out}
    else Direct out
  in
  {
    redirections = Keys.empty;
    settings;
    version=Some version;
    printer;
    mode;
    scheme;
  }

let redirect log field ?(close=ignore) ppf  =
  log.redirections <-
    Keys.add field.name {initialized=ref false;ppf;close} log.redirections

let record_scheme: type a. a record typ -> a def  =
  function
  | Custom _ -> assert false
  | Record sch -> sch
  | _ -> .

let record_list_scheme: type a. a record list typ -> a def  =
  function
  | Custom _ -> assert false
  | List r -> record_scheme r
  | _ -> .

let generic_detach key_scheme ~store ~lift ~extract log (field: _ field) =
  let out = Keys.find_opt field.name log.redirections in
  let version = log.version in
  let mode = match log.mode with
    | Direct d ->
        let out = Option.value ~default:d out in
        Direct out
    | Store {data=st; out=st_out} ->
        let data =
          match Option.bind (Store.get field st) extract with
          | Some data -> data
          | None ->
              let data = Record.make version [] in
              store st version ~field (lift data); data
        in
        let out = match out with
          | Some _ -> out
          | _ -> st_out
        in Store { data; out }
  in
  let child =
    { scheme=key_scheme field.typ;
      mode;
      printer=log.printer;
      version = log.version;
      settings = log.settings;
      redirections = Keys.empty;
    } in
  child

let some x = Some x
let detach log key =
  generic_detach record_scheme
    ~store:Store.record ~lift:Fun.id ~extract:some log key
let detach_item log key =
  generic_detach record_list_scheme
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

(** *)
let set (field: _ field) x log =
  let version = log.version in
  match log.mode with
  | Store st -> Store.record st.data version ~field x
  | Direct d ->
      let status = match log.scheme.?(field.name) with
        | Some lmd -> Version.stage_at version lmd.status
        | None -> Lv.Deletion
      in
      match status with
      | Deletion | Future -> ()
      | Refinement | Creation | Expansion | Deprecation ->
          let r = Keys.find_opt field.name log.redirections in
          let out = Option.value ~default:d r in
          let ppf = !(out.ppf) in
          if not !(d.initialized) then
            (Fmt.init log.settings out.ppf ; d.initialized := true);
          Format.fprintf ppf "@[<v>%a@,@]%!"
            log.printer.item (field.name, V(field.typ,x))

let cons field x log =
  match log.mode with
  | Direct _-> set field [x] log
  | Store st -> Store.cons st.data log.version ~field x

let (.%[]<-) log field x = set field x log

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
        Option.map (fun version ->
            Validation.record ~toplevel:true ~version log.scheme st.data
          )
          log.version
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

let tmp scheme =
  {
  settings = None;
  redirections = Keys.empty;
  version=None;
  scheme;
  printer = { record = (fun _ _ -> ()); item = (fun _ _ -> ()) };
  mode = Store { out=None; data=Record.make None [] }
}



let log_if dlog key flag printer x =
  if flag then
    Format.kasprintf (fun s -> dlog.%[key] <- s) "%a" printer x
