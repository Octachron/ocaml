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




module Version = struct

  type t = { major:int; minor:int }

  type error =
    | Duplicate_key of string
    | Time_travel of t * t
    | Sealed_version of t
  exception Error of error
  let pp_error ppf = function
    | Time_travel _ ->
        Format.fprintf ppf "Key from the future"
    | Duplicate_key s ->
        Format.fprintf ppf "Duplicated key: %s" s
    | Sealed_version _ ->
        Format.fprintf ppf "Sealed version"

  let error e =
    Format.eprintf "Log error %a@." pp_error e;
    raise (Error e)


  type range = { introduction: t; deprecation: t option }

  type base_event =
    | Creation
    | New_key of string
    | Deprecation of string
    | Deletion of string
    | Seal

  type event =
    { scheme: string option; version:t; event:base_event }
  type _ history = {
    mutable current: t;
    mutable events: event list
  }

  type 'a update = {
    v:t;
    history:'a history;
    minor_update:bool;
  }

  let breaking_change update = if update.minor_update then
      error (Sealed_version update.history.current)

  let register_event update scheme event =
    let h = update.history in
    h.events <- { scheme; version=update.v; event} :: h.events

  let zeroth = { major = 0; minor = 0}
  let first = { major = 1; minor = 0 }

  let new_version history version =
    let sv = history.current in
    if version <= sv  then error (Time_travel (version, sv));
    let minor_update = version.major = sv.major in
    history.current <- version;
    { v=version; minor_update; history }

  let current_version history = history.current

  open Format
  let pp_version ppf x = fprintf ppf "v%d.%d" x.major x.minor

  let pp_base_event ppf =
    function
    | Creation -> fprintf ppf "Creation"
    | New_key name -> fprintf ppf "Key %S" name
    | Deprecation name -> fprintf ppf "Deprecation %S" name
    | Seal -> fprintf ppf "Seal"
    | Deletion name -> fprintf ppf "Deletion %S" name

  let pp_base ppf e =
    fprintf ppf "%a, %a, %a"
      (pp_print_option pp_print_string) e.scheme
      pp_version e.version
      pp_base_event e.event

  let group_history events =
    let module M = Map.Make(struct
        type nonrec t = string option * t
        let compare: t -> t -> int = Stdlib.compare
      end)
    in
    let add m e =
      let k = (e.scheme, e.version) in
      let prev = Option.value ~default:[] (M.find_opt k m) in
      M.add k (e.event::prev) m
    in
    let m = List.fold_left add M.empty events in
    let reconstruct (s,v) e = { event=e; scheme=n; version=v} in
    List.concat_map (fun (g,l) -> List.map (reconstruct g) l) (M.bindings m)

  let pp_history ppf h =
    fprintf ppf "@[<v>%a@]." (pp_print_list pp_base) (group_history h.events)


end
type version = Version.t = { major:int; minor:int }


type doc = Format.formatter -> unit

module K = struct type t = string let compare = compare end
module Keys = Map.Make(K)

type _ extension = ..

type empty = Empty_tag

type polarity = Positive | Negative

type 'a typ =
  | Unit: unit typ
  | Bool: bool typ
  | Int: int typ
  | String: string typ
  | Doc: doc typ
  | List: {optional:bool; elt:'a typ} -> 'a list typ
  | Option: 'a typ -> 'a option typ
  | Pair: 'a typ * 'b typ -> ('a * 'b) typ
  | Triple: 'a typ * 'b typ * 'c typ -> ('a * 'b * 'c) typ
  | Quadruple: 'a typ * 'b typ * 'c typ * 'd typ ->
      ('a * 'b * 'c * 'd) typ
  | Sum: 'a def -> 'a sum typ
  | Record: 'id def -> 'id prod typ
  | Custom: { id :'b extension; pull: ('b -> 'a); default: 'a typ} ->
      'b typ

and ('a,'b) key = {
  name: string;
  typ: 'a typ;
  id: 'a Type.Id.t;
}
and 'a sum =
  | Constr: ('a,'b) key * 'a -> 'b sum
  | Enum: (unit,'b) key -> 'b sum
and key_metadata =
    Key_metadata:
      { typ: 'a typ;
        version:version;
        deprecation: version option } ->
      key_metadata
and 'a def = {
  scheme_name: string;
  mutable keys: (Keys.key * key_metadata) list;
  polarity: polarity;
}
and 'a prod = {
    mutable fields: 'a sum Keys.t
  }

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
      mode: 'a mode
  }
and 'a mode =
  | Direct of ppf_with_close
  | Store of { data:'a prod; out:(ppf_with_close * printer) option }

and printer = { print: 'a. Format.formatter -> 'a prod -> unit; }


type 'a t = 'a log


let (.!()<-) scheme key metadata =
  scheme.keys <- (key.name, metadata) :: scheme.keys



let new_key update scheme name typ =
  begin match scheme.polarity with
  | Positive -> ()
  | Negative -> Version.breaking_change update
  end;
  if List.mem_assoc name scheme.keys then Version.(error (Duplicate_key name));
  let metadata = Key_metadata {
    version=update.Version.v;
    deprecation=None;
    typ
  }
  in
  let key = {
    name;
    typ; id = Type.Id.make ();
  } in
  scheme.!(key) <- metadata;
  Version.register_event update (Some scheme.scheme_name) (New_key name);
  key


type _ extension += Version: version extension

let version_ty =
  let pull v = Version.(v.major, v.minor) in
  Custom { id = Version; pull; default = Pair (Int,Int) }

let make_key typ name = { name; typ; id = Type.Id.make () }
let version_key = make_key version_ty "version"
let validity_key = make_key Bool "valid"

let metakey key =
  key.name,
  Key_metadata { typ = key.typ; version = Version.zeroth; deprecation = None}
let version_metakey = metakey version_key
let validity_metakey = metakey validity_key


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
  val new_key: vl Version.update  -> string -> 'a typ -> 'a key
end

module type Sum = sig
  include Def
  val new_constr: vl Version.update -> string -> 'a typ -> 'a key
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

let deprecate_key u key scheme =
  let Key_metadata r = scheme.!(key) in
  Version.register_event u (Some scheme.scheme_name) (Deprecation key.name);
  scheme.!(key) <-
    Key_metadata { r with deprecation = Some u.Version.v }

let delete_key u key scheme =
  let Key_metadata r = scheme.!(key) in
  Version.register_event u (Some scheme.scheme_name) (Deletion key.name);
  scheme.!(key) <-
    Key_metadata { r with deprecation = Some u.Version.v }

let seal update scheme =
  Version.register_event update (Some scheme.scheme_name) Seal

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
    let () = Version.register_event Info.update (Some (Info.name)) Creation
    let new_key v name ty = new_key v scheme name ty
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
    let () = Version.register_event Info.update (Some (Info.name)) Creation
    let new_constr u name ty = new_key u scheme name ty
    let deprecate u k = deprecate_key u k scheme
    let delete u k = delete_key u k scheme
    let seal u = seal u scheme
end



let enum key = Enum key
let constr key x = Constr(key,x)



(** {1:log_scheme_versionning  Current version of the log } *)



let version_range key scheme =
  let Key_metadata r =  scheme.!(key) in
  { Version.introduction = r.version; deprecation = r.deprecation }


module Record = struct
  let (=:) = constr
  let field_name = function    | Enum k -> k.name
    | Constr (k,_) -> k.name
  let make fields =
    let fields = List.fold_left (fun fields field ->

        Keys.add (field_name field) field fields
      ) Keys.empty fields
        in { fields }
end

(*
module V = New_def ()
let major = V.new_key "major" Int
let minor = V.new_key "minor" Int
type _ extension += Version: version extension
let () = seal_version V.scheme


let[@warning "-32"] version_typ =
  let pull v =
    Record.(make [ major =: v.major; minor =: v.minor ] )
  in
  Custom { pull; id = Version; default = Record V.scheme}
*)


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
        Keys.for_all is_empty_field x.fields
    | _ -> false
  and is_empty_field: type a. _ -> a sum -> bool = fun _ ->
    function
    | Enum _ -> false
    | Constr(kt,x) -> is_empty kt.typ x

module Store = struct

  let record:
    type ty s. s prod -> key:(ty,s) key -> ty -> unit =
      fun store ~key x ->
        store.fields <- Keys.add key.name (constr key x) store.fields

  let get (type a b) (key: (a,b) key) (st:b prod): a option =
    match Keys.find_opt key.name st.fields with
    | None -> None
    | Some (Enum _) -> None
    | Some (Constr(k,x)) ->
        match Type.Id.provably_equal k.id key.id with
        | None -> None
        | Some Type.Equal -> Some x

  let cons: type ty s. s prod -> key:(ty list,s) key -> ty -> unit =
    fun store ~key x ->
      let l = match get key store with
        | None -> [x]
        | Some l -> x :: l
      in
      store.fields <- Keys.add key.name (constr key l) store.fields

  let trim keys prod =
     let field (key,_)  =
        match Keys.find_opt key prod.fields with
        | Some (Enum _) as c -> c
        | None -> None
        | Some (Constr (kt,x)) as c ->
            if is_empty kt.typ x then None else c
      in
      List.filter_map field (List.rev keys)


  let rec validate: type id. ?toplevel:version -> id def -> id prod -> bool =
    fun ?toplevel sch st ->
    (* don't add validation metakeys to empty sublog*)
    let valid = validate_fields sch.keys st.fields in
    Option.iter (fun v ->
        record st ~key:version_key v;
        record st ~key:validity_key valid
      ) toplevel;
    valid


  and validate_fields: type id.
    (Keys.key * key_metadata) list -> id sum Keys.t -> bool =
    fun metadata data ->
    List.fold_left (fun valid (k,kmd) ->
       valid && validate_key (is_optional kmd) (Keys.find_opt k data)
      ) true metadata
  and validate_key: type a. bool -> a sum option -> bool  =
    fun opt k ->
    match opt, k with
    | true, None -> true
    | _, None -> false
    | _, Some (Enum _) -> true
    | _, Some (Constr (k,v)) -> validate_value v k.typ
  and validate_value: type a. a -> a typ -> bool = fun v typ ->
    match typ with
    | Record m -> validate m v
    | Int -> true
    | Bool -> true
    | Doc -> true
    | String -> true
    | Custom _ -> true
    | Unit -> true
    | List {elt; _} -> List.for_all (fun v -> validate_value v elt) v
    | Option m ->
        Option.value ~default:true
          (Option.map (fun v -> validate_value v m) v)
    | Pair (x,y) ->
        let vx, vy = v in
        validate_value vx x && validate_value vy y
    | Triple (x,y,z) ->
        let vx, vy, vz = v in
        validate_value vx x && validate_value vy y && validate_value vz z
    | Quadruple (x,y,z,w) ->
        let vx, vy, vz, vw = v in
        validate_value vx x
        && validate_value vy y
        && validate_value vz z
        && validate_value vw w
    | Sum _ ->
        begin match v with
        | Enum _ -> true
        | Constr(k, v) -> validate_value v k.typ
        end


end

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
    open_with_label: string printer;
    label_sep: pr;
    close_with_label: string printer
  }

  type conv = {
    string:string printer;
    doc:doc printer;
    assoc:assoc;
    list:list_convention;
  }

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
    conv.assoc.open_with_label ppf key;
    conv.assoc.label_sep ppf;
    elt ppf;
    conv.assoc.close_with_label ppf key

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
    | Bool -> Format.pp_print_bool ppf x
    | String -> conv.string ppf x
    | Doc -> conv.doc ppf x
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
    |  List { elt=e; _ } ->
        list conv (List.map (elt conv {extension} e) x) ppf
    | Sum _ ->
        begin match x with
        | Constr(kt,x) ->
            elt conv {extension} (Pair(String,kt.typ)) (kt.name,x) ppf
        | Enum kt ->
            elt conv {extension} String kt.name ppf
        end
    | Record m -> elt_record conv {extension} (m.keys,x) ppf
    | Option e ->
        begin match x with
        | None ->  conv.string ppf "None"
        | Some x -> elt conv {extension} e x ppf
        end

  and elt_item: type a.
    conv -> extension_printer -> key:string -> a typ -> a -> pr =
    fun conv ext ~key ty x ppf -> item conv ~key (elt conv ext ty x) ppf
  and fields: type p.
    conv -> extension_printer -> (_ * p prod)  -> pr list
    = fun conv ext (keys,prod) ->
      let fields = Store.trim keys prod in
      let pp_field = function
        | Enum kt -> elt_item conv ext ~key:kt.name kt.typ ()
        | Constr (kt,x) -> elt_item conv ext ~key:kt.name kt.typ x
      in
      List.map pp_field fields

  and elt_record: type p. conv -> extension_printer -> (_ * p prod) -> pr =
    fun conv ext x -> record conv (fields conv ext x)

  let direct = {
    string = Format.pp_print_string;
    doc = (|>);
    list = {
      list_open = ignore;
      list_close = ignore ;
      sep = Format.dprintf "@ ";
    };
    assoc = {
      assoc_open = Format.dprintf "@[<v>";
      assoc_close = Format.dprintf "@]";
      open_with_label = (fun _ -> ignore);
      label_sep = ignore;
      sep = (fun ppf () -> Format.fprintf ppf "@,");
      close_with_label = (fun _ -> ignore);
    }
  }

  let escaped_doc ppf doc = escape_string ppf (Format.asprintf "%t" doc)
  let sexp =
    let list_open = Format.dprintf "@[("
    and list_close = Format.dprintf ")@]"
    and sep = Format.dprintf "@ " in
    {
      string = escape_string;
      doc = escaped_doc;
      list = {list_open; list_close; sep };
      assoc = {
        assoc_open = list_open;
        assoc_close = list_close;
        open_with_label = (fun ppf -> Format.fprintf ppf "@[<b 2>(%s");
        sep = (fun ppf () -> sep ppf);
        label_sep = sep;
        close_with_label = (fun ppf _ -> Format.fprintf ppf ")@]");
      }
    }

  let json =
    {
      string = escape_string;
      doc = escaped_doc;
      list = {
        list_open=Format.dprintf "@[<b 2>[";
        list_close = Format.dprintf "@,]@]";
        sep = Format.dprintf ",@ ";
      };
      assoc = {
        assoc_open = Format.dprintf "@[<hv 2>{@ ";
        assoc_close = Format.dprintf "@;<0 -2>}@]";
        open_with_label = (fun ppf -> Format.fprintf ppf "@[<b 2>%S");
        label_sep = Format.dprintf "@ =@ ";
        sep = (fun ppf () -> Format.fprintf ppf ",@ ");
        close_with_label = (fun ppf _ -> Format.fprintf ppf "@]");
      }
    }


  let no_extension = { extension = fun _ -> None }
  let chain_extensions x y =
    let chain ext =
      match x.extension ext with
      | None -> y.extension ext
      |  Some _ as p -> p
    in
    { extension = chain }

  let extensions = ref no_extension
  let add_extension x =
    extensions := chain_extensions x !extensions


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

  let make color version ppf scheme ~with_schema:_ =
     {
      redirections = Keys.empty;
      settings=color;
      mode = Direct {initialized=ref false; ppf; close=ignore};
      version;
      scheme;
    }

end

let redirect log key ?(close=ignore) ppf  =
  log.redirections <-
    Keys.add key.name {initialized=ref false;ppf;close} log.redirections

let key_scheme: type a b. (a prod,b) key -> a def  = fun key ->
  match key.typ with
  | Custom _ -> assert false
  | Record sch -> sch

let item_key_scheme: type a b. (a prod list,b) key -> a def  = fun key ->
  match key.typ with
  | Custom _ -> assert false
  | List { elt=Custom _ ; _ } -> assert false
  | List { elt=Record sch; _ } -> sch
  | _ -> .

let option_key_scheme: type a b. (a prod option,b) key -> a def  = fun key ->
  match key.typ with
  | Custom _ -> assert false
  | Option (Custom _) -> assert false
  | Option (Record sch) -> sch
  | _ -> .

let generic_detach key_scheme store lift extract log key =
  let out = Keys.find_opt key.name log.redirections in
  let mode = match log.mode with
    | Direct d ->
        let out = Option.value ~default:d out in
        Direct out
    | Store st ->
        let data =
          match Option.bind (Store.get key st.data) extract with
          | Some data -> data
          | None ->
              let data = { fields = Keys.empty } in
              store st.data ~key (lift data); data
        in
        let out = match st.out, out with
          | Some (_,pr), Some out-> Some(out,pr)
          | x, _ -> x
        in Store { data; out }
  in
  let child =
    { scheme=key_scheme key;
      mode;
      version = log.version;
      settings = log.settings;
      redirections = Keys.empty;
    } in
  child

let some x = Some x
let detach log key = generic_detach key_scheme Store.record Fun.id some log key
let detach_item log key =
  generic_detach item_key_scheme Store.cons Fun.id (Fun.const None) log key
let detach_option log key =
  generic_detach option_key_scheme Store.record some Fun.id log key

let active_key log key =
  let Key_metadata m = log.scheme.!(key) in
  not (log.version < m.version) &&
  match m.deprecation with
  | None -> true
  | Some d -> d < log.version

let set key x log =
  if not (active_key log key) then () else
  match log.mode, Keys.find_opt key.name log.redirections with
  | Direct d, r ->
    let out = Option.value ~default:d r in
    let ppf = !(out.ppf) in
    if not !(d.initialized) then
      (Fmt.init log.settings out.ppf ; d.initialized := true);
    Format.fprintf ppf "@[<v>%t%a@]%!"
      Fmt.(elt_item direct !extensions ~key:key.name key.typ x)
     Fmt.direct.assoc.sep ();
  | Store _, Some out ->
      let ppf = !(out.ppf) in
      Fmt.(elt_item direct !extensions) ~key:key.name key.typ x ppf
  | Store st, None -> Store.record st.data ~key x

let cons key x log =
  match log.mode, Keys.find_opt key.name log.redirections with
  | Direct _, _ | _, Some _ -> set key [x] log
  | Store st, None -> Store.cons st.data ~key x

let (.%[]<-) log key x = set key x log

let f key log fmt = Format.kasprintf (fun s -> log.%[key] <- s) fmt
let itemf key log fmt = Format.kasprintf (fun s -> cons key s log) fmt

let d key log fmt = Format.kdprintf (fun s -> log.%[key] <- s) fmt
let itemd key log fmt = Format.kdprintf (fun s -> cons key s log) fmt

let o key log fmt = Format.kdprintf (fun s -> log.%[key] <- Some s) fmt


let flush: type a. a log -> unit = fun log ->
  begin match log.mode with
  | Direct d -> Fmt.flush d
  | Store st ->
      ignore (Store.validate ~toplevel:log.version log.scheme st.data);
      Option.iter (fun (out,{print}) ->
          let ppf = !(out.ppf) in
          print ppf st.data
        ) st.out;
        st.data.fields <- Keys.empty
  end;
  Keys.iter (fun _ -> Fmt.flush) log.redirections

let separate log = match log.mode with
  | Direct d -> Fmt.separate d
  | _ -> ()

let close: type a. a log -> unit = fun log ->
  begin match log.mode with
  | Direct d -> Fmt.close d
  | Store { out = Some (out,_)} -> out.close ()
  | Store _ -> ()
  end;
  Keys.iter (fun _ -> Fmt.close) log.redirections

let close log = flush log; close log

let replay source dest =
  match source.mode with
  | Direct _ -> ()
  | Store st ->
      Keys.iter (fun _ field ->
          match field with
          | Enum key ->  dest.%[key] <- ()
          | Constr(key,x) -> dest.%[key] <- x
          ) st.data.fields

(** {1:log_creation }*)

let tmp scheme = {
  settings = None;
  redirections = Keys.empty;
  version = {major=0; minor=0};
  scheme;
  mode = Store { out=None; data= {fields=Keys.empty} }
}


let slist = List { optional=true; elt=String }

module Compiler_log_version = New_root()
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
  let parsetree = new_key v1 "parsetree" (Option String)
  let source = new_key v1 "source" (Option String)
  let typedtree = new_key v1 "typedtree" (Option String)
  let shape = new_key v1 "shape" (Option String)
  let instr = new_key v1 "instr" (Option String)
  let lambda = new_key v1 "lambda" (Option String)
  let raw_lambda = new_key v1 "raw_lambda" (Option String)
  let flambda = new_key v1 "flambda" slist
  let raw_flambda = new_key v1 "raw_flambda" slist
  let clambda = new_key v1 "clambda" slist
  let raw_clambda = new_key v1 "raw_clambda" slist
  let cmm = new_key v1 "cmm" slist
  let remove_free_vars_equal_to_args =
    new_key v1 "remove-free-vars-equal-to-args" slist
  let unbox_free_vars_of_closures =
    new_key v1 "unbox-free-vars-of-closures" slist
  let unbox_closures = new_key v1 "unbox-closures" slist
  let unbox_specialised_args = new_key v1 "unbox-specialised-args" slist
  let mach = new_key v1 "mach" slist
  let linear = new_key v1 "linear" slist
  let cmm_invariant = new_key v1 "cmm_invariant" (Option String)
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
  let debug = new_key v1  "debug" (Option (Record Debug.scheme))
end

let dloc = List { optional=true; elt = Doc}
module Toplevel = struct
  let v1 = Compiler_log_version.v1
  include New_record(Compiler_log_version)
      (struct
        let name = "toplevel"
        let update = v1
      end)
      ()
  let v1 = Compiler_log_version.v1
  let output = new_key v1 "output" Doc
  let backtrace = new_key v1 "backtrace" (Option Doc)
  let compiler_log =
    new_key v1 "compiler_log" (Option (Record Compiler.scheme))
  let errors = new_key v1 "errors" dloc
  let trace = new_key v1 "trace" dloc
  let () = seal v1
end

let log_if dlog key flag printer x =
  if flag then
    Format.kasprintf (fun s -> dlog.%[key] <- Some (s)) "%a" printer x


module Json_schema = struct
  open Fmt
  let string s ppf = Format.fprintf ppf "%S" s
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
    item ~key:"$ref" @@ Format.dprintf {|"#/$defs/%s"|} x.scheme_name

  let rec typ: type a b. a typ -> Format.formatter -> unit = function
    | Int -> tfield {|integer|}
    | Bool -> tfield {|bool|}
    | Unit -> tfield {|int|}
    | String -> tfield {|string|}
    | Doc -> tfield {|string|}
    | List e ->
        Format.dprintf "%t,@ %t"
          (tfield  {|array|})
          (item ~key:"items" @@ obj [typ e.elt] )
    | Option x -> typ x
    | Pair (x,y) -> tuple_typ [typ x; typ y]
    | Triple (x,y,z) -> tuple_typ [typ x; typ y; typ z]
    | Quadruple (x,y,z,w) -> tuple_typ [typ x;typ y; typ z; typ w]
    | Sum x -> sref x
    | Record x -> sref x
    | Custom x -> typ x.default
  and tuple_typ = fun l ->
    Format.dprintf "%t,@ %t"
      (tfield  {|array|})
      (item ~key:"items" @@ array @@
       List.map (fun x -> obj [x]) l
      )

  let const name = item ~key:"const" @@ string name
  let sum x =
    let constructor (name, Key_metadata kty) =
      obj [tuple_typ [const name; typ kty.typ]] in
    obj [ item ~key:"oneOf" (array (List.map constructor x.keys)) ]

  let fields x =
    List.map
      (fun (key, Key_metadata kty) -> item ~key (obj [typ kty.typ]))
      x

  let required_fields x =
    List.filter_map
      (fun (k, kinfo) -> if is_optional kinfo then None else Some(string k))
      x

  let obj_typ = item ~key:"type" (string "object")

  let schema_field =
    item ~key:"schema" @@ obj [obj_typ]

  let record_fields x =
    [
      obj_typ;
      item ~key:"properties" @@ obj (fields x);
      item ~key:"required" @@ array (required_fields x)
    ]

  let simple_record x = obj (record_fields x)

  let union a =
    List.fold_left (fun m acc -> Keys.union (fun _ x _ -> Some x) m acc)
      Keys.empty
      a
  let rec refs: type a. a typ -> (Format.formatter -> unit) Keys.t =
    fun ty -> match ty with
      | Sum x ->
          Keys.add x.scheme_name (sum x) (subrefs x)
      | Record x ->
          Keys.add x.scheme_name (simple_record x.keys) (subrefs x)
      | Doc -> Keys.empty
      | Int -> Keys.empty
      | Bool -> Keys.empty
      | String -> Keys.empty
      | Unit -> Keys.empty
      | List x -> refs x.elt
      | Option x -> refs x
      | Pair (x,y) -> union [refs x; refs y]
      | Triple (x,y,z) -> union [refs x; refs y; refs z]
      | Quadruple (x,y,z,w) -> union [refs x; refs y; refs z; refs w]
      | Custom t -> refs t.default
and subrefs: type a. a def -> (Format.formatter -> unit) Keys.t
    = fun sch ->
  union (List.map (fun (_,Key_metadata kty) -> refs kty.typ) sch.keys)

   let pp sch ppf =
     let keys = version_metakey :: validity_metakey :: sch.keys in
     let defs = match Keys.bindings (subrefs sch) with
       | [] -> []
       | defs ->
           let prs = List.map (fun (key,pr) -> item ~key pr) defs in
           [item ~key:"$defs" @@ obj prs]
     in
     obj (header @ defs @ schema_field :: record_fields keys) ppf

   let schema_item sch = item ~key:"schema" (pp sch)

  let pp_log ppf log =
    let sch = log.scheme in
    pp sch ppf

  end

module Backends = struct

  let with_conv conv settings version ?schema_printer ppf scheme ~with_schema =
    let print ppf r =
      if Keys.is_empty r.fields then () else
        let fields = Fmt.fields conv Fmt.no_extension (scheme.keys,r) in
        let fields = match with_schema, schema_printer with
          | false, _ | _, None -> fields
          | true, Some pr -> pr scheme :: fields
        in
        Format.fprintf ppf "%t@." (Fmt.record conv fields)
    in
    { version;
      settings;
      redirections = Keys.empty;
      mode = Store {
          data={fields=Keys.empty};
          out = Some ({initialized=ref false;ppf;close=ignore},{print})
        };
      scheme;
    }

  let sexp color version ppf sch = with_conv Fmt.sexp color version ppf sch
  let json color version ppf sch =
    with_conv Fmt.json ~schema_printer:Json_schema.schema_item color
      version ppf sch

  type t = {
    name:string;
    make: 'a. Misc.Color.setting option -> version -> Format.formatter ref
      -> 'a def -> with_schema:bool -> 'a log;
  }
  let fmt = { name="stderr"; make = Fmt.make }
  let sexp = { name="sexp" ; make = sexp }
  let json = { name = "json"; make = json }
end
