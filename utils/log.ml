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
module Key_set = Set.Make(K)

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


and ('a,'b) key = { name: string; typ: 'a typ; id: 'a Type.Id.t }
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
    mutable fields: 'a sum Keys.t
  }

type ppf_with_close =
  { ppf: Format.formatter ref;
    close: unit -> unit;
  }

type 'a log =
  {
    default: ppf_with_close;
    mutable redirections: redirection Keys.t;
    version: version;
    mode: 'a mode;
  }
[@@warning "-69"]
and 'a mode =
  | Direct of Misc.Color.setting option
  | Store of 'a prod * printer
and printer = { print: 'a. Format.formatter -> 'a prod -> unit; }
and redirection =
  | Const of ppf_with_close
  | Detached: 'a prod Type.Id.t * 'a log -> redirection
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
  let key = { name; typ; id = Type.Id.make () } in
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



module Record = struct
  let (=:) = constr
  let make fields =
    let fields = List.fold_left (fun fields (Constr(k,_) as field) ->
        Keys.add k.name field fields
      ) Keys.empty fields
        in { fields }
end


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


module Store = struct

  let record:
    type ty s. s prod -> key:(ty,s) key -> ty -> unit =
      fun store ~key x ->
        let x =
          match key.typ with
          | List _ ->
              begin match Keys.find_opt key.name store.fields with
                | None -> x
                | Some (Constr(k,y)) ->
                    match Type.Id.provably_equal k.id key.id with
                    | None -> x
                    | Some Type.Equal -> (x @ y:ty)
              end
          | _ -> x
        in
        store.fields <- Keys.add key.name (constr key x) store.fields

  module Fmt_tbl = Hashtbl.Make(struct
      type t = Format.formatter * (unit -> unit)
      let equal (ppf, _c) (ppf', _c') = ppf == ppf'
      let hash _ppf = 0
    end)

  let split log fields =
    let default = !(log.default.ppf) in
    let add_to_partition (default,others) k ppf =
      let default = Key_set.remove k default in
      let set =
      match Fmt_tbl.find_opt others ppf with
      | None -> Key_set.singleton k
      | Some set -> Key_set.add k set
      in
      Fmt_tbl.replace others ppf set;
      default, others
   in
   let keys = fields |> Keys.to_seq |> Seq.map fst |> Key_set.of_seq in
   Keys.fold (fun k redirection partition ->
        match redirection with
        | Const c ->
            if !(c.ppf) == default then partition else
              add_to_partition partition k (!(c.ppf), c.close)
        | Detached _ -> assert false
     )
     log.redirections (keys, Fmt_tbl.create (Key_set.cardinal keys))

  let subrecord key_set {fields} =
    let st = { fields = Keys.empty } in
    Key_set.iter (fun key ->
        match Keys.find_opt key fields with
        | None -> ()
        | Some (Constr(key,x)) ->
            record st ~key x
      ) key_set;
    st
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
    assoc:assoc;
    list:list_convention;
  }

  let escape_string str =
    let buf = Buffer.create (String.length str * 5 / 4) in
    for i = 0 to String.length str - 1 do
      match str.[i] with
      | '\\' -> Buffer.add_string buf {|\\|}
      | '\"' -> Buffer.add_string buf {|\"|}
      | '\n' -> Buffer.add_string buf {|\n|}
      | '\t' -> Buffer.add_string buf {|\t|}
      | '\r' -> Buffer.add_string buf {|\r|}
      | '\b' -> Buffer.add_string buf {|\b|}
      | '\x00' .. '\x1F' | '\x7F' as c ->
          Printf.bprintf buf "\\u%04X" (Char.code c)
      | c -> Buffer.add_char buf c
    done;
    Buffer.contents buf


  let rec elt : type a. conv -> extension_printer
    -> a typ -> Format.formatter -> a -> unit =
    fun conv {extension} typ ppf x ->
    match typ with
    | Int -> Format.pp_print_int ppf x
    | String -> Format.fprintf ppf {|"%s"|} (escape_string x)
    | Doc ->
        let str = Format.asprintf "%t" x in
        Format.fprintf ppf {|"%s"|} (escape_string str)
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
        let Constr(kt,x) = x in
        elt conv {extension} (Pair(String,kt.typ)) ppf (kt.name,x)
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
      let field ppf (key, Constr(kt,x)) =
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
      sep = (fun _ -> ignore);
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
        open_with_label = (fun ppf -> Format.fprintf ppf "@[<b 2>(%s");
        sep = (fun ppf () -> sep ppf);
        label_sep = sep;
        close_with_label = (fun ppf _ -> Format.fprintf ppf ")@]");
      }
    }

  let json =
    {
      list = {
        list_open=Format.dprintf "@[<b 2>[";
        list_close = Format.dprintf "@,]@]";
        sep = Format.dprintf ",@ ";
      };
      assoc = {
        assoc_open = Format.dprintf "@[<hv 2>{@ ";
        assoc_close = Format.dprintf "@,}@]";
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


  let flush ppf = Format.fprintf ppf "@]@."


  let make color version ppf _sch =
    init color ppf;
    {
      default={ppf;close=ignore};
      redirections = Keys.empty;
      mode = Direct color;
      version;
    }

end

let ppf log key =
  match Keys.find_opt key.name log.redirections with
  | None -> !(log.default.ppf)
  | Some Const c -> !(c.ppf)
  | Some Detached (_,log) -> !(log.default.ppf)

let redirect log key ?(close=ignore) ppf  =
  if ppf != log.default.ppf then
    log.redirections <- Keys.add key.name (Const {ppf;close}) log.redirections

let fresh_detach log cppf key =
  let mode = match log.mode with
    | Direct _ as d -> d
    | Store (_, printer) -> Store({fields=Keys.empty}, printer)
  in
  let version = match key.typ with
    | Record sch -> sch.scheme_version
    | _ -> log.version
  in
  let log =
    {
      default = cppf;
      redirections = Keys.empty;
      mode;
      version
    }
  in
  log.redirections <-
    Keys.add key.name (Detached (key.id,log)) log.redirections;
  log

let detach (type id sub) (log: id log) (key: (sub prod, id) key) =
  match Keys.find_opt key.name log.redirections with
  | Some Detached (id,x) ->
      begin match Type.Id.provably_equal id key.id with
      | Some Type.Equal -> (x:sub log)
      | None -> assert false
      end
  | Some (Const cppf) -> fresh_detach log cppf key
  | None -> fresh_detach log {ppf=log.default.ppf; close=ignore} key


let set key x log =
  match log.mode with
  | Direct _ ->
    let ppf = ppf log key in
    Fmt.(item direct !extensions) ~key:key.name key.typ ppf x
  | Store (st, _ ) -> Store.record st ~key x

let (.%[]<-) log key x = set key x log

let f key log fmt =
  Format.kasprintf (fun s -> log.%[key] <- s ) fmt

let itemf key log fmt =
  Format.kasprintf (fun s -> log.%[key] <- [s] ) fmt

let flush log =
  let ppf = !(log.default.ppf) in
  match log.mode with
  | Direct _ -> Fmt.flush ppf;
  | Store (st, pr) ->
      let default, others = Store.split log st.fields in
      pr.print ppf (Store.subrecord default st);
      Store.Fmt_tbl.iter (fun (ppf,close) subset ->
          pr.print ppf (Store.subrecord subset st);
          close ()
        ) others;
      log.default.close ()

let replay source dest =
  match source.mode with
  | Direct _ -> ()
  | Store (st,_) ->
      Keys.iter (fun _ (Constr(key,x)) -> dest.%[key] <- x ) st.fields

(** {1:log_creation }*)

module Structured = struct

  let with_conv conv version ppf =
    let print ppf r =
      Format.fprintf ppf "%a@."
      Fmt.(prod conv no_extension) r
    in
    { version;
      default= {ppf; close=ignore};
      redirections = Keys.empty;
      mode = Store ({fields=Keys.empty}, {print})
    }

  let sexp _color version ppf _sch = with_conv Fmt.sexp version ppf
  let json _color version ppf _sch = with_conv Fmt.json version ppf

end

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

module Debug = struct
  include New_def ()
  let parsetree = new_key "parsetree" String
  let source = new_key "source" String
  let typedtree = new_key "typedtree" String
  let shape = new_key "shape" String
  let instr = new_key "instr" String
  let lambda = new_key "lambda" String
  let raw_lambda = new_key "raw_lambda" String
  let flambda = new_key "flambda" (List String)
  let raw_flambda = new_key "raw_flambda" (List String)
  let clambda = new_key "clambda" (List String)
  let raw_clambda = new_key "raw_clambda" (List String)
  let cmm = new_key "cmm" (List String)
  let remove_free_vars_equal_to_args =
    new_key "remove-free-vars-equal-to-args" (List String)
  let unbox_free_vars_of_closures =
    new_key "unbox-free-vars-of-closures" (List String)
  let unbox_closures =
    new_key "unbox-closures" (List String)
  let unbox_specialised_args =
    new_key "unbox-specialised-args" (List String)
  let mach = new_key "mach" (List String)
  let linear = new_key "linear" (List String)
  let cmm_invariant = new_key "cmm_invariant" String
end

module Error = New_def ()
module Warnings = New_def ()


module Compiler = struct
  include New_def ()
  let debug = new_key "debug" (Record Debug.scheme)
end

let log_if dlog key flag printer x =
  if flag then f key dlog "%a" printer x;
  x
