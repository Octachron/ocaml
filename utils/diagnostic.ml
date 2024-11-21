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

module Label_map = Misc.Stdlib.String.Map
module H = Diagnostic_history

type empty = Empty_tag
type polarity = Positive | Negative
type _ extension = ..

type version = Diagnostic_history.version = { major:int; minor: int}
type 'a update = 'a Diagnostic_history.update

type 'a typ =
  | Unit: unit typ
  | Bool: bool typ
  | Int: int typ
  | Float: float typ
  | String: string typ
  | List: 'a typ -> 'a list typ
  | Pair: 'a typ * 'b typ -> ('a * 'b) typ
  | Triple: 'a typ * 'b typ * 'c typ -> ('a * 'b * 'c) typ
  | Quadruple: 'a typ * 'b typ * 'c typ * 'd typ ->
      ('a * 'b * 'c * 'd) typ

  | Sum: 'a t -> 'a sum typ
  | Record: 'id t -> 'id record typ
  | Custom: {
      id :'b extension;
      pull: (version option -> 'b -> 'a);
      default: 'a typ
    } -> 'b typ
and ('a,'b) field = {
  name:string;
  typ:'a typ;
  opt:bool;
  id: 'a Type.Id.t;
  range:Diagnostic_history.Lifetime.t
}
and 'a bound_field = F: ('a,'b) field * 'a -> 'b bound_field
and 'id sum =
    Constr: { name:string; typ:'a typ; arg:'a; approx: 'id sum option }
      -> 'id sum
and any_typ = T: 'a typ -> any_typ
and label_metadata = {
  ltyp: any_typ;
  optional: bool;
  status:Diagnostic_history.Lifetime.t;
}
and 'a t = {
  scheme_name: string;
  mutable labels: (Label_map.key * label_metadata) list;
  polarity: polarity;
}
and 'a record = 'a bound_field Label_map.t ref

type typed_record = R: 'a t * 'a record -> typed_record
and typed_val = V: 'a typ * 'a -> typed_val

let version_range field = field.range
let field_name f = f.name
let field_type field = field.typ
let is_optional r = r.optional

let destruct c f =
  let rec expand (Constr c) =
    let head = c.name, V (c.typ,c.arg) in
      match c.approx with
      | None -> [head]
      | Some t -> head :: expand t
  in
  f (expand c)
let scheme_name x = x.scheme_name
let field_infos d = d.labels
let field_names d = List.map fst d.labels

let record_scheme: type a. a record typ -> a t  =
  function
  | Custom _ -> assert false
  | Record sch -> sch
  | _ -> .

let record_list_scheme: type a. a record list typ -> a t  =
  function
  | Custom _ -> assert false
  | List r -> record_scheme r
  | _ -> .


let (.!()<-) scheme name metadata =
  let rec update_if_present name metadata = function
    | [] -> None
    | (a_name, _ as a) :: q ->
        if a_name = name then Some ((name,metadata) :: q)
        else
          Option.map (List.cons a) (update_if_present name metadata q)
  in
  let updated = update_if_present name metadata scheme.labels in
  let labels = Option.value ~default:((name,metadata)::scheme.labels) updated in
  scheme.labels <- labels

let rec pp_typ: type a. Format.formatter -> a typ -> unit = fun ppf -> function
| Unit -> Format.pp_print_string ppf ""
| Int -> Format.pp_print_string ppf "i"
| Bool -> Format.pp_print_string ppf "b"
| Float -> Format.pp_print_string ppf "f"
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
    status = Diagnostic_history.(range @@ v update);
    optional;
    ltyp = T typ
  }

let register_label_metadata ~optional update scheme name typ =
  begin match scheme.polarity with
  | Positive -> ()
  | Negative -> Diagnostic_history.breaking_change update scheme.scheme_name
  end;
  if List.mem_assoc name scheme.labels then
    Diagnostic_history.(error update scheme.scheme_name (Duplicate_key name));
  let metadata = label_metadata ~optional update typ in
  scheme.!(name) <- metadata;
  Diagnostic_history.register_event update scheme.scheme_name
    (Creation {
        name;
        typ=Format.asprintf "%s%a" (if optional then "?" else "") pp_typ typ
      };
    )

type _ extension += Version: version extension

let version_ty =
  let pull _ v = v.major, v.minor in
  Custom { id = Version; pull; default = Pair (Int,Int) }


module type Def = sig
  type id
  type vl
  type 'a label
  type definition
  type scheme = id t
  type raw_type = definition

  val scheme: scheme
  val raw_type: raw_type typ

  val deprecate: vl update -> 'a label -> 'a label
  val delete: vl update -> 'a label -> 'a label
  val seal: vl update -> unit
end

module type Record = sig
  type id
  type nonrec 'a field = ('a,id) field
  include Def
    with type id := id
     and type definition := id record
     and type 'a label :='a field
  val new_field: ?opt:bool -> vl update  -> string -> 'a typ -> 'a field
  val new_field_opt: vl update  -> string -> 'a typ -> 'a field
  val make_required: vl update -> 'a field -> unit
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
      version: version;
    } -> ('current,'id) constructor_projection

let is_expansion c (Proj p) = c.cname = p.old.cname

let rec select_version:
  type t id. version -> (t,id) constructor -> t -> id sum =
  fun v c x ->
  match c.projection with
  | None -> Constr { name = c.cname; typ=c.typ; arg=x; approx = None}
  | Some (Proj p) ->
      if v >= p.version then
        Constr { name = c.cname; typ=c.typ; arg=x; approx=None}
      else select_version v p.old (p.map x)

let rec expand_all_approx: type t id. (t,id) constructor -> t -> id sum =
  fun c x ->
  match c.projection with
  | None -> Constr { name = c.cname; typ=c.typ; arg=x; approx = None}
  | Some (Proj p) ->
    let approx = Some (expand_all_approx p.old (p.map x)) in
    Constr { name = c.cname; typ=c.typ; arg=x; approx}

let app v c x =
  match v with
  | None -> expand_all_approx c x
  | Some v -> select_version v c x

module type Sum = sig
  type id
  type 'a constructor
  include Def
    with type id := id
     and type definition := id sum
     and type 'a label := 'a constructor
  val app: version option -> 'a constructor -> 'a -> raw_type

  val refine:
    vl update -> 'a constructor -> ('b -> 'a)
    -> string -> 'b typ -> 'b constructor
  val new_constr: vl update -> string -> 'a typ -> 'a constructor
  val new_constr0: vl update -> string -> unit constructor
  val publish: vl update -> 'a constructor -> 'a constructor
  val expand:
    vl update -> 'a constructor -> ('b->'a) -> 'b typ -> 'b constructor

end



module New_local_def() = struct
  type id
  type scheme = id t
end

let (.?()) scheme lbl = List.assoc_opt lbl scheme.labels
let field_info sch f = sch.?(f.name)

let (let&?) x f = Option.iter f x

let make_required u f scheme =
  let&? kmd = scheme.?(f.name) in
  H.inconsistent_if_inactive u ~scheme:scheme.scheme_name f.name kmd.status;
  H.register_event u scheme.scheme_name (Make_required f.name);
  scheme.!(f.name) <- { kmd with optional = false }

let register_constructor_expansion u old new_typ scheme =
  let&? kmd = scheme.?(old.cname) in
  H.inconsistent_if_inactive u ~scheme:scheme.scheme_name old.cname kmd.status;
  begin match old.projection with
  | None -> ()
  | Some p ->
      if is_expansion old p then
        H.invalid_constructor_expansion u ~scheme:scheme.scheme_name old.cname
  end;
  H.register_event u scheme.scheme_name
    (Expansion {name=old.cname;
                expansion = Format.asprintf "%a" pp_typ new_typ});
  let status = { kmd.status with expansion = Some (H.v u) } in
  scheme.!(old.cname) <- { kmd with status; ltyp=T new_typ }


let register_constructor_inception u old new_name new_typ scheme =
  let&? kmd = scheme.?(old.cname) in
  H.inconsistent_if_inactive u ~scheme:scheme.scheme_name old.cname kmd.status;
  H.register_event u scheme.scheme_name
    (Inception {
        base_name=old.cname;
        new_name;
        typ = Format.asprintf "%a" pp_typ new_typ
      }
    );
  let status = H.(prerange @@ v u) in
  let lmd = label_metadata ~optional:false u new_typ in
  scheme.!(new_name) <- { lmd with status }

let register_constructor_publication u name scheme =
  let&? kmd = scheme.?(name) in
  begin match H.stage kmd.status with
  | Inception -> ()
  | _ -> H.error u scheme.scheme_name (Invalid_publication name)
  end;
  H.register_event u scheme.scheme_name (Publication name);
  let status = { kmd.status with publication = Some (H.v u) } in
  scheme.!(name) <- { kmd with status }



let deprecate_lbl u lbl scheme =
  let&? kmd = scheme.?(lbl) in
  H.inconsistent_if_inactive u ~scheme:scheme.scheme_name lbl kmd.status;
  H.register_event u scheme.scheme_name (Deprecation lbl);
  let status = { kmd.status with deprecation = Some (H.v u) } in
  scheme.!(lbl) <- { kmd with status }

let delete_lbl u lbl scheme =
  let&? kmd = scheme.?(lbl) in
  H.inconsistent_if_not_deprecated u ~scheme:scheme.scheme_name lbl kmd.status;
  H.register_event u scheme.scheme_name (Deletion lbl);
  let status = { kmd.status with deletion = Some (H.v u) } in
  scheme.!(lbl) <- { kmd with status }

let seal update scheme =
  H.register_event update scheme.scheme_name Seal

module type Info = sig
  type vl
  val name: string
  val update: vl update
end

module Record = struct
  type 'a bfield = version option -> 'a bound_field option
  let field f x v =
    match H.stage_at v f.range with
    | Inception | Publication | Expansion | Deprecation -> Some (F(f,x))
    | Future | Deletion -> None
  let opt_field f x v = match x with
    | None -> None
    | Some x -> field f x v
  let (^=) = field
  let (^=?) = opt_field

  let field_name (F (f,_)) = f.name

  let make v fields =
    let fields = List.fold_left (fun fields field ->
        match field v with
        | None -> fields
        | Some field ->  Label_map.add (field_name field) field fields
      ) Label_map.empty fields
    in
    ref fields
  let fields x = !x
  let all_fields x = Seq.map snd @@ Label_map.to_seq (fields x)

  let set:
    type ty s.
      s record -> version option -> field:(ty,s) field -> ty -> unit
    = fun store v ~field:f x ->
        let name = f.name in
        Option.iter (fun field ->
        store := Label_map.add name field !store
        ) (field f x v)

  let get (type a b) (st:b record) (field: (a,b) field): a option =
    match Label_map.find_opt field.name (fields st) with
    | None -> None
    | Some (F(f,x)) ->
        match Type.Id.provably_equal f.id field.id with
        | None -> None
        | Some Type.Equal -> Some x

  let dynamic_get st name =
    Label_map.find_opt name (fields st)
    |> Option.map (fun (F(k,x)) -> V (k.typ,x))

  let cons: type ty s.
    s record -> version option -> field:(ty list,s) field -> ty -> unit =
    fun store v ~field:f x ->
      let l = match get store f with
        | None -> [x]
        | Some l -> x :: l
      in
      let bf = field f l v in
      Option.iter (fun bfield ->
          store := Label_map.add f.name bfield (fields store)
        ) bf

   let reset f = f := Label_map.empty
end


module New_record(Vl:H.S)(Info:Info with type vl:=Vl.id)() = struct
  include New_local_def ()
  type nonrec 'a field = ('a,id) field
  type raw_type = id record
  let scheme = {
    scheme_name = Info.name;
    labels = [];
    polarity=Positive;
  }
  let raw_type = Record scheme

  let () = H.register_event Info.update Info.name Declaration

  let new_field ?(opt=false) (type t) u name (ty:t typ): t field =
    register_label_metadata ~optional:opt u scheme name ty;
    {
      name;
      typ = ty;
      opt;
      id = Type.Id.make ();
      range = H.(range @@ v u)
    }
  let new_field_opt v name ty = new_field ~opt:true v name ty
  let deprecate u f =
    deprecate_lbl u f.name scheme;
    let range = { f.range with deprecation = Some (H.v u) } in
    { f with range }
  let delete u f =
    delete_lbl u f.name scheme;
    let range = { f.range with deletion = Some (H.v u) } in
    { f with range }

  let make_required u f = make_required u f scheme
  let seal u = seal u scheme
end

module New_sum(Vl:H.S)(Info:Info with type vl:=Vl.id)() = struct
  include New_local_def ()
  type raw_type = id sum
  let scheme = {
    scheme_name = Info.name;
    labels = [];
    polarity = Negative;
  }
  let raw_type = Sum scheme
  type nonrec 'a constructor = ('a,id) constructor
  let () = H.register_event Info.update Info.name Declaration
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
    let projection = Some(Proj {map;old;version=H.v u}) in
    { old with typ=new_ty; projection }

  let refine u old map new_name new_ty =
    let () = register_constructor_inception u old new_name new_ty scheme in
    let projection = Some(Proj {map;old;version=H.v u}) in
    { cname=new_name; typ=new_ty; projection }

  let publish u c =
    register_constructor_publication u c.cname scheme;
    c

  let deprecate u c = deprecate_lbl u c.cname scheme; c
  let delete u c = delete_lbl u c.cname scheme; c
  let seal u = seal u scheme
end

let fields labels r =
  let field rfields label =
    rfields
    |> Label_map.find_opt label
    |> Option.map (fun (F (k,v)) -> k.name, k.opt, V(k.typ,v))
  in
  List.filter_map (field @@ Record.fields r) (List.rev labels)

module Metadata_versions = H.Make()
module Metadata = struct
  let v1 = Metadata_versions.v1
  include New_record(Metadata_versions)(struct
      let name = "metadata"
      let update = v1
    end)()
  let version = new_field v1 "version" version_ty
  let downward_compatible = new_field v1 "downward_compatible" Bool
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
        range = H.(range @@ v v1);
        name = "metadata";
        opt=false;
        typ = raw_type;
        id = Type.Id.make ()
      }
  let metakey =
    "metadata",
    label_metadata ~optional:false v1 raw_type
end
let metakey = Metadata.metakey

type diagnostic_version =
  | Downward_compatible of version
  | Exact of version
let diagnostic_version (Exact v | Downward_compatible v) = v
let exact_version = function
  | Exact v -> Some v
  | Downward_compatible _ -> None
let downward_compatible = function
  | Downward_compatible _ -> true
  | _ -> false

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
    | Float -> false
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
    version:version -> id t -> id record -> report_paths =
    fun ~version sch st -> fields ~version sch.labels (Record.fields st)
  and fields: type id.
    version:version -> (Label_map.key * label_metadata) list
    -> id bound_field Label_map.t -> report_paths
    = fun ~version metadata data ->
    concat_map (fun (k, kmd) ->
        match H.stage_at (Some version) kmd.status with
        | Future | Deletion -> none (* those fields will be elided *)
        | Deprecation ->
            deprecated [k]  @^
            field  ~version ~optional:(is_optional kmd) k
              (Label_map.find_opt k data)
        | Inception | Publication | Expansion ->
            field  ~version ~optional:(is_optional kmd) k
              (Label_map.find_opt k data)
      ) metadata
  and field: type a.
    version:version -> optional:bool -> string -> a bound_field option
    -> report_paths = fun ~version ~optional name k ->
    match optional, k with
    | true, None -> none
    | false, None -> invalid [name]
    | _, Some (F (k,v)) ->
        qualify name (value ~version v k.typ)
  and value: type a. version:version -> a -> a typ -> report_paths =
    fun ~version v typ ->
    match typ with
    | Record m -> record ~version m v
    | Int -> none
    | Bool -> none
    | String -> none
    | Float -> none
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
            begin match H.stage_at (Some version) lmd.status with
            | Inception | Publication | Expansion -> value ~version c.arg c.typ
            | Future | Deletion -> invalid [c.name]
            | Deprecation -> deprecated [c.name] @^ value ~version c.arg c.typ
            end

  let diagnostic ~version:v sch st =
    let version = diagnostic_version v in
    let r = record ~version sch st in
      let valid = match r.deprecated, r.invalid with
        | [], [] -> Metadata.Validity.full
        | _::_, [] ->Metadata. Validity.deprecated
        | _, _ :: _  -> Metadata.Validity.invalid
      in
      let v1 = H.v Metadata.v1 in
      let valid = app (Some v1) valid () in
      let metadata =
        let open Record in
        make (Some v1) [
          Metadata.version ^= version;
          Metadata.downward_compatible ^= downward_compatible v;
          Metadata.valid ^= valid;
          Metadata.invalid_paths ^= r.invalid;
          Metadata.deprecated_paths ^= r.deprecated;
        ]
      in
      Record.set st None
        ~field:(Metadata.universal_field ())
        metadata;
      r
end
