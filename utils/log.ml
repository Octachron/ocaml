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

module Keys = Map.Make(struct type t = string let compare = compare end)

type _ extension = ..

type empty = Empty_tag

type 'a typ =
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


and ('a,'b) key = { name: string; typ: 'a typ }
and 'a sum = Constr: ('a,'b) key * 'a -> 'b sum
and key_metadata =
    Key_metadata:
      { typ: 'a typ;
        version:version;
        deprecation: version option } ->
      key_metadata
and 'a def = {
  mutable scheme_version: version;
  mutable open_scheme:bool;
  mutable keys: key_metadata Keys.t
}
and 'a prod = {
    fields: 'a sum Keys.t
  }

type 'a log = {
  device: device;
  version: version
}
and device = {
  print: 'a. key:string -> 'a typ -> 'a -> unit;
  sub: key:string -> device;
  flush: unit -> unit
}
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


let new_key name scheme typ =
  if not scheme.open_scheme then
    error (Sealed_version scheme.scheme_version)
  else if Keys.mem name scheme.keys then error (Duplicate_key name);
  let metadata = Key_metadata {
    version=scheme.scheme_version;
    deprecation=None;
    typ
  }
  in
  let key = { name; typ } in
  scheme.!(key) <- metadata;
  key

module type Def = sig
  type id
  type scheme = id def
  type log = id t
  type nonrec 'a key = ('a,id) key
  val scheme: scheme
  val new_key: string -> 'a typ -> 'a key
end
module New_def() : Def = struct
  type id
  type nonrec 'a key = ('a,id) key
  type scheme = id def
  type log = id t
  let scheme =
    { scheme_version = first_version;
      open_scheme = true;
      keys = Keys.empty
    }
    let new_key name ty = new_key name scheme ty
end

let flush log = log.device.flush ()


let constr key x = Constr(key,x)

let (.!()) scheme key =
  match Keys.find_opt key.name scheme.keys with
  | Some x -> x
  | None -> assert false



let deprecate_key key scheme =
  let Key_metadata r = scheme.!(key) in
  scheme.!(key) <-
    Key_metadata { r with deprecation = Some scheme.scheme_version }

(** {1:log_scheme_versionning  Current version of the log } *)

let version scheme = scheme.scheme_version

let seal_version scheme =
  scheme.open_scheme <- false

let name_version scheme version =
  let sv = scheme.scheme_version in
  if version <= sv  then error (Time_travel (version, sv))
  else (
    scheme.scheme_version <- version;
    scheme.open_scheme <- true
  )

let version_range key scheme =
  let Key_metadata r =  scheme.!(key) in
  { introduction = r.version; deprecation = r.deprecation }

(** {1:log_creation }*)

let create device version _scheme = { device; version }
let detach key (log: _ log) =
  let device = log.device.sub ~key:key.name in
  let version = log.version in
  { device; version }


module Record = struct
  let (=:) = constr
  let make fields =
    let fields = List.fold_left (fun fields (Constr(k,_) as field) ->
        Keys.add k.name field fields
      ) Keys.empty fields
        in { fields }
end

let set key x log =
  log.device.print ~key:key.name key.typ x

let (.%[]<-) log key x = set key x log

let fmt key log fmt =
  Format.kasprintf (fun s -> log.%[key] <- s ) fmt



module V = New_def ()
let major = V.new_key "major" Int
let minor = V.new_key "minor" Int
type _ extension += Version: version extension
let () = seal_version V.scheme

module Compiler = New_def ()

let version_typ =
  let pull v =
    Record.(make [ major =: v.major; minor =: v.minor ] )
  in
  Custom { pull; id = Version; default = Record V.scheme}

let _version_key =
  Compiler.new_key "version" version_typ

module Error = New_def ()
module Warnings = New_def ()



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
    open_with_label: string printer;
    label_sep: pr;
    close_with_label: string printer
  }

  type conv = {
    assoc:assoc;
    list:list_convention;
  }

  let rec elt : type a. conv -> extension_printer
    -> a typ -> Format.formatter -> a -> unit =
    fun conv {extension} typ ppf x ->
    match typ with
    | Int -> Format.pp_print_int ppf x
    | String -> Format.pp_print_string ppf x
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
        fmt_sum  conv {extension} ppf x
    | Record _ ->
        prod conv {extension} ppf x
    | Option e ->
        begin match x with
        | None ->  ()
        | Some x -> elt conv {extension} e ppf x
        end
  and fmt_sum: type s.
    conv -> extension_printer -> Format.formatter ->
    s sum -> unit
    =
    fun conv {extension} ppf (Constr (k,x)) ->
    conv.assoc.open_with_label ppf k.name;
    conv.assoc.label_sep ppf;
    (elt conv {extension} k.typ) ppf x;
    conv.assoc.close_with_label ppf k.name;
  and prod: type p.
    conv -> extension_printer -> Format.formatter -> p prod -> unit
    = fun conv extension ppf prod ->
      conv.assoc.assoc_open ppf;
      Keys.iter (fun key (Constr(kt,x)) ->
          item conv extension ~key kt.typ ppf x
        ) prod.fields;
      conv.assoc.assoc_close ppf
  and item: type a. conv -> extension_printer ->
    key:string -> a typ -> Format.formatter -> a -> unit =
    fun conv extension ~key typ ppf x ->
    conv.assoc.open_with_label ppf key;
    conv.assoc.label_sep ppf;
    (elt conv extension typ) ppf x;
    conv.assoc.close_with_label ppf key

  let direct = {
    list = {
      list_open = ignore;
      list_close = ignore;
      sep = Format.dprintf "@ ";
    };
    assoc = {
      assoc_open = ignore;
      assoc_close = ignore;
      open_with_label = (fun _ -> ignore);
      label_sep = ignore;
      close_with_label = (fun _ -> ignore);
    }
  }

  let sexp =
    let list_open = Format.dprintf "@[("
    and list_close = Format.dprintf ")@]"
    and sep = Format.dprintf "@ " in
    {
      list = {list_open; list_close; sep };
      assoc = {
        assoc_open = list_open;
        assoc_close = list_close;
        open_with_label = (fun ppf -> Format.fprintf ppf "@[(%s");
        label_sep = sep;
        close_with_label = (fun ppf _ -> Format.fprintf ppf "@]");
      }
    }

  let json =
    {
      list = {
        list_open=Format.dprintf "@[[";
        list_close = Format.dprintf "]@]";
        sep = Format.dprintf ",@ ";
      };
      assoc = {
        assoc_open = Format.dprintf "@[{";
        assoc_close = Format.dprintf "}@]";
        open_with_label = (fun ppf -> Format.fprintf ppf "@[%S");
        label_sep = Format.dprintf "@ =@ ";
        close_with_label = (fun ppf _ -> Format.fprintf ppf "@]");
      }
    }



  let no_extension = { extension = fun _ -> None }

  let rec gen version ?(ext=no_extension) proj ppf =

    let init = ref false in
    let initialize () =
      if !init then ()
      else begin
        init := true;
        let color = Misc.Style.enable_color !Clflags.color in
        Misc.Style.set_tag_handling ~color (proj ppf);
        Format.fprintf (proj ppf) "@[<v>"
      end in
    {
      flush = (fun () -> Format.fprintf (proj ppf) ")@]@." );
      sub = (fun ~key:_ -> gen version ~ext proj ppf);
      print = (fun ~key ty x ->
          initialize ();
          item direct ext ~key ty (proj ppf) x)
    }


  let make version ?ext ppf =
    gen version ?ext Fun.id ppf

  let make_ref version ?ext ppf =
    gen version ?ext (!) ppf
end
