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

type version = { major:int; minor:int }
type version_range = { introduction: version; deprecation: version option }
let first_version = { major = 0; minor = 0 }

type doc = Format.formatter -> unit

module K = struct type t = string let compare = compare end
module Keys = Map.Make(K)

type _ extension = ..

type empty = Empty_tag

type scheme_status =
  | Sealed
  | Open of { minor_changes: bool; breaking_changes: bool }

type polarity = Positive | Negative

type 'a typ =
  | Unit: unit typ
  | Int: int typ
  | String: string typ
  | Doc: doc typ
  | List: 'a typ -> 'a list typ
  | Option: 'a typ -> 'a option typ
  | Pair: 'a typ * 'b typ -> ('a * 'b) typ
  | Triple: 'a typ * 'b typ * 'c typ -> ('a * 'b * 'c) typ
  | Quadruple: 'a typ * 'b typ * 'c typ * 'd typ ->
      ('a * 'b * 'c * 'd) typ
  | Sum: 'a def -> 'a sum typ
  | Record: 'id def -> 'id prod typ
  | Custom: { id :'b extension; pull: ('b -> 'a); default: 'a typ} ->
      'b typ


and ('a,'b) key = { name: string; typ: 'a typ; id: 'a Type.Id.t }
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
  scheme_version: version ref;
  scheme_status: scheme_status ref;
  mutable keys: key_metadata Keys.t;
  version_key: (version,'a) key option;
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
      mutable children: child_log list;
      version: version;
      scheme: 'a def;
      settings: Misc.Color.setting option;
      mode: 'a mode
  }
[@@warning "-69"]
and child_log = Child: 'a log -> child_log [@@unboxed]
and 'a mode =
  | Direct of ppf_with_close
  | Store of { data:'a prod; out:(ppf_with_close * printer) option }
[@@warning "-69"]

and printer = { print: 'a. Format.formatter -> 'a prod -> unit; }
[@@warning "-37"]

type 'a t = 'a log

type error =
  | Duplicate_key of string
  | Time_travel of version * version
  | Sealed_version of version
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

let (.!()<-) scheme key metadata =
  scheme.keys <- Keys.add key.name metadata scheme.keys


let minor_change scheme =
  match !(scheme.scheme_status) with
  | Sealed | Open { minor_changes = false; _ } ->
      error (Sealed_version !(scheme.scheme_version))
  | Open { minor_changes=true; _ } -> ()

let breaking_change scheme =
  match !(scheme.scheme_status) with
  | Sealed | Open { breaking_changes = false; _ } ->
      error (Sealed_version !(scheme.scheme_version))
  | Open { breaking_changes=true; _ } -> ()

let new_key version scheme name typ =
  begin match scheme.polarity with
  | Positive -> minor_change scheme
  | Negative -> breaking_change scheme
  end;
  if Keys.mem name scheme.keys then error (Duplicate_key name);
  let metadata = Key_metadata {
    version;
    deprecation=None;
    typ
  }
  in
  let key = { name; typ; id = Type.Id.make () } in
  scheme.!(key) <- metadata;
  key



type _ extension += Version: version extension
type 'a scheme_version = version

let version_ty =
  let pull v = v.major, v.minor in
  Custom { id = Version; pull; default = Pair (Int,Int) }

let version_key () =
  { name = "version"; typ = version_ty; id = Type.Id.make () }

module type Def = sig
  type id
  type scheme = id def
  type log = id t
  type nonrec 'a key = ('a,id) key
  val scheme: scheme
end

module type Record = sig
  type root
  include Def
  val new_key: 'a scheme_version  -> string -> 'a typ -> 'a key
end

module type Sum = sig
  type root
  include Def
  val new_constr: id scheme_version -> string -> 'a typ -> 'a key
end

module type Root = sig
  include Def
  val v1: id scheme_version
  val new_key: 'a scheme_version  -> string -> 'a typ -> 'a key
  val new_version: version -> id scheme_version
end


let major_scheme_update = Open { minor_changes = true; breaking_changes = true}
let minor_scheme_update = Open { minor_changes = true; breaking_changes = false}

module New_local_def() = struct
  type id
  type nonrec 'a key = ('a,id) key
  type scheme = id def
  type log = id t
end

module New_root_scheme() = struct
  include New_local_def ()
  let scheme =
    let version_key = Some (version_key ()) in
    let metakey =
      Key_metadata {
        version = first_version;
        deprecation = None;
        typ=version_ty
      }
    in
    { scheme_version = ref first_version;
      scheme_status = ref major_scheme_update;
      keys = Keys.singleton "version" metakey;
      version_key;
      polarity=Positive;
    }
    let new_key v name ty = new_key v scheme name ty

    let v1 = first_version
    let new_version version =
      let sv = !(scheme.scheme_version) in
      if version <= sv  then error (Time_travel (version, sv))
      else (
        scheme.scheme_version := version;
        let update =
          if version.major > sv.major then major_scheme_update
          else minor_scheme_update in
        scheme.scheme_status := update
      );
      version
end

module New_record(Root:Def)() = struct
  include New_local_def ()
  let scheme =
    { scheme_version = Root.scheme.scheme_version;
      scheme_status =  Root.scheme.scheme_status;
      keys = Keys.empty;
      version_key=None;
      polarity=Positive;
    }
    let new_key v name ty = new_key v scheme name ty
    let derived_version x = x
end

module New_sum(Root:Def)() = struct
  include New_local_def ()
  let scheme =
    { scheme_version = Root.scheme.scheme_version;
      scheme_status =  Root.scheme.scheme_status;
      keys = Keys.empty;
      version_key=None;
      polarity = Negative;
    }
    let new_constr v name ty = new_key v scheme name ty
end



let enum key = Enum key
let constr key x = Constr(key,x)

let (.!()) scheme key =
  match Keys.find_opt key.name scheme.keys with
  | Some x -> x
  | None -> assert false

let deprecate_key key scheme =
  let Key_metadata r = scheme.!(key) in
  scheme.!(key) <-
    Key_metadata { r with deprecation = Some !(scheme.scheme_version) }

(** {1:log_scheme_versionning  Current version of the log } *)

let version scheme = !(scheme.scheme_version)

let seal_version scheme =
  scheme.scheme_status := Sealed


let version_range key scheme =
  let Key_metadata r =  scheme.!(key) in
  { introduction = r.version; deprecation = r.deprecation }



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

module Store = struct

  let record:
    type ty s. s prod -> key:(ty,s) key -> ty -> unit =
      fun store ~key x ->
        let x =
          match key.typ with
          | List _ ->
              begin match Keys.find_opt key.name store.fields with
                | None -> x
                | Some (Enum _) -> x
                | Some (Constr(k,y)) ->
                    begin match Type.Id.provably_equal k.id key.id with
                    | None -> x
                    | Some Type.Equal -> (x @ y:ty)
                    end
              end
          | _ -> x
        in
        store.fields <- Keys.add key.name (constr key x) store.fields
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

  let rec elt : type a. conv -> extension_printer
    -> a typ -> Format.formatter -> a -> unit =
    fun conv {extension} typ ppf x ->
    match typ with
    | Unit -> Format.pp_print_int ppf 0
    | Int -> Format.pp_print_int ppf x
    | String -> conv.string ppf x
    | Doc -> x ppf
    | Pair (a,b) ->
        let x, y = x in
        Format.fprintf ppf "%t%a%t%a%t"
          conv.list.list_open
          (elt conv {extension} a) x
          conv.list.sep
          (elt conv {extension} b) y
          conv.list.list_close
    | Triple (a,b,c) ->
        let x, y, z = x in
        Format.fprintf ppf "%t%a%t%a%t%a%t"
          conv.list.list_open
          (elt conv {extension} a) x
          conv.list.sep
          (elt conv {extension} b) y
          conv.list.sep
          (elt conv {extension} c) z
          conv.list.list_close
    | Quadruple (a,b,c,d) ->
        let x, y, z ,w = x in
        conv.list.list_open ppf;
        (elt conv {extension} a) ppf x;
        conv.list.sep ppf;
        (elt conv {extension} b) ppf y;
        conv.list.sep ppf;
        (elt conv {extension} c) ppf z;
        conv.list.sep ppf;
        (elt conv {extension} d) ppf w;
        conv.list.list_close ppf
    | Custom {pull; default; id } -> begin
        match extension id with
        | Some pr -> pr ppf x
        | None -> elt  conv {extension} default ppf (pull x)
      end
    |  List e ->
        let pp_sep ppf () = conv.list.sep ppf in
        conv.list.list_open ppf;
        Format.pp_print_list ~pp_sep (elt conv {extension} e) ppf x;
        conv.list.list_close ppf;
    | Sum _ ->
        begin match x with
        | Constr(kt,x) ->
            elt conv {extension} (Pair(String,kt.typ)) ppf (kt.name,x)
        | Enum kt ->
            elt conv {extension} String ppf kt.name
        end
    | Record _ ->
        prod conv {extension} ppf x
    | Option e ->
        begin match x with
        | None ->  ()
        | Some x -> elt conv {extension} e ppf x
        end
  and prod: type p.
    conv -> extension_printer -> Format.formatter -> p prod -> unit
    = fun conv extension ppf prod ->
      let field ppf (key, field) =
        match field with
        | Enum kt -> item conv extension ~key kt.typ ppf ()
        | Constr (kt,x) ->
            item conv extension ~key kt.typ ppf x
      in
      conv.assoc.assoc_open ppf;
      Format.pp_print_seq ~pp_sep:conv.assoc.sep field ppf
        (Keys.to_seq prod.fields);
      conv.assoc.assoc_close ppf
  and item: type a. conv -> extension_printer ->
    key:string -> a typ -> Format.formatter -> a -> unit =
    fun conv extension ~key typ ppf x ->
    conv.assoc.open_with_label ppf key;
    conv.assoc.label_sep ppf;
    (elt conv extension typ) ppf x;
    conv.assoc.close_with_label ppf key

  let direct = {
    string = Format.pp_print_string;
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

  let sexp =
    let list_open = Format.dprintf "@[("
    and list_close = Format.dprintf ")@]"
    and sep = Format.dprintf "@ " in
    {
      string = escape_string;
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

  let make color version ppf scheme =
     {
      redirections = Keys.empty;
      settings=color;
      mode = Direct {initialized=ref false; ppf; close=ignore};
      version;
      scheme;
      children = [];
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
  | List (Custom _) -> assert false
  | List (Record sch) -> sch
  | _ -> .


let generic_detach key_scheme lift log key =
  let out = Keys.find_opt key.name log.redirections in
  let mode = match log.mode with
    | Direct d ->
        let out = Option.value ~default:d out in
        Direct out
    | Store st ->
        let data = {fields=Keys.empty} in
        let out = match st.out, out with
          | Some (_,pr), Some out-> Some(out,pr)
          | x, _ ->
              Store.record st.data ~key (lift data);
              x
        in
        Store { data; out }
  in
  let child =
    { scheme=key_scheme key;
      mode;
      version = log.version;
      settings = log.settings;
      redirections = Keys.empty;
      children = [];
    } in
  log.children <- Child child :: log.children;
  child

let detach log key = generic_detach key_scheme Fun.id log key
let detach_item log key = generic_detach item_key_scheme (fun x -> [x]) log key

let set key x log =
  match log.mode, Keys.find_opt key.name log.redirections with
  | Direct d, r ->
    let out = Option.value ~default:d r in
    let ppf = !(out.ppf) in
    if not !(d.initialized) then
      (Fmt.init log.settings out.ppf ; d.initialized := true);
    Format.fprintf ppf "@[<v>%a%a@]%!"
      Fmt.(item direct !extensions ~key:key.name key.typ) x
     Fmt.direct.assoc.sep ();
  | Store _, Some out ->
      let ppf = !(out.ppf) in
      Fmt.(item direct !extensions) ~key:key.name key.typ ppf x
  | Store st, None -> Store.record st.data ~key x

let (.%[]<-) log key x = set key x log

let f key log fmt =
  Format.kasprintf (fun s -> log.%[key] <- s ) fmt
let itemf key log fmt =
  Format.kasprintf (fun s -> log.%[key] <- [s] ) fmt

let d key log fmt =
  Format.kdprintf (fun s -> log.%[key] <- s ) fmt
let itemd key log fmt =
  Format.kdprintf (fun s -> log.%[key] <- [s] ) fmt


let rec flush: type a. a log -> unit = fun log ->
  begin match log.mode with
  | Direct d -> Fmt.flush d
  | Store st ->
      Option.iter (fun vk ->
          log.%[vk] <- !(log.scheme.scheme_version))
        log.scheme.version_key;
      Option.iter (fun (out,{print}) ->
          let ppf = !(out.ppf) in
          print ppf st.data
        ) st.out
  end;
  Keys.iter (fun _ -> Fmt.flush) log.redirections;
  List.iter (fun (Child c) -> flush c) log.children

let separate log = match log.mode with
  | Direct d -> Fmt.separate d
  | _ -> ()

let rec close: type a. a log -> unit = fun log ->
  begin match log.mode with
  | Direct d -> Fmt.close d
  | Store { out = Some (out,_)} -> out.close ()
  | Store _ -> ()
  end;
  Keys.iter (fun _ -> Fmt.close) log.redirections;
  List.iter (fun (Child c) -> close c) log.children

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

module Structured = struct

  let with_conv conv settings version scheme ppf =
    let print ppf r =
      if Keys.is_empty r.fields then () else
        Format.fprintf ppf "%a@."
          Fmt.(prod conv no_extension) r
    in
    { version;
      settings;
      redirections = Keys.empty;
      mode = Store {
          data={fields=Keys.empty};
          out = Some ({initialized=ref false;ppf;close=ignore},{print})
        };
      scheme;
      children = [];
    }

  let sexp color version ppf sch = with_conv Fmt.sexp color version sch ppf
  let json color version ppf sch = with_conv Fmt.json color version sch ppf

end

let tmp scheme = {
  settings = None;
  redirections = Keys.empty;
  children = [];
  version = {major=0; minor=0};
  scheme;
  mode = Store { out=None; data= {fields=Keys.empty} }
}

module Backends = struct
  type t = {
    name:string;
    make: 'a. Misc.Color.setting option -> version -> Format.formatter ref
      -> 'a def -> 'a log;
  }
  let fmt = { name="stderr"; make = Fmt.make }
  let sexp = { name="sexp" ; make = Structured.sexp }
  let json = { name = "json"; make = Structured.json }
end

module Compiler_root = New_root_scheme ()

module Debug = struct
  include New_record(Compiler_root)()
  let v1 = derived_version Compiler_root.v1
  let parsetree = new_key v1 "parsetree" String
  let source = new_key v1 "source" String
  let typedtree = new_key v1 "typedtree" String
  let shape = new_key v1 "shape" String
  let instr = new_key v1 "instr" String
  let lambda = new_key v1 "lambda" String
  let raw_lambda = new_key v1 "raw_lambda" String
  let flambda = new_key v1 "flambda" (List String)
  let raw_flambda = new_key v1 "raw_flambda" (List String)
  let clambda = new_key v1 "clambda" (List String)
  let raw_clambda = new_key v1 "raw_clambda" (List String)
  let cmm = new_key v1 "cmm" (List String)
  let remove_free_vars_equal_to_args =
    new_key v1 "remove-free-vars-equal-to-args" (List String)
  let unbox_free_vars_of_closures =
    new_key v1 "unbox-free-vars-of-closures" (List String)
  let unbox_closures =
    new_key v1 "unbox-closures" (List String)
  let unbox_specialised_args =
    new_key v1 "unbox-specialised-args" (List String)
  let mach = new_key v1 "mach" (List String)
  let linear = new_key v1 "linear" (List String)
  let cmm_invariant = new_key v1 "cmm_invariant" String
end

module Error = New_record (Compiler_root)()

module Compiler = struct
  include Compiler_root
  let debug = new_key v1 "debug" (Record Debug.scheme)
end


module Toplevel = struct
  include New_root_scheme()
  let output = new_key v1 "output" (List Doc)
  let compiler_log = new_key v1 "compiler_log" (List (Record Compiler.scheme))
  let errors = new_key v1 "errors" (List Doc)
  let trace = new_key v1 "trace" (List Doc)
end

let log_if dlog key flag printer x =
  if flag then f key dlog "%a" printer x;
  x
