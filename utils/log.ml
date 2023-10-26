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
    mutable fields: 'a sum Keys.t
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

type format_extension_printer =
  { extension: 'b. 'b extension -> (Format.formatter -> 'b -> unit) option}

let rec fmt_elt : type a. format_extension_printer
  -> a typ -> Format.formatter -> a -> unit =
  fun {extension} typ ppf x ->
  match typ with
  | Int -> Format.pp_print_int ppf x
  | String -> Format.pp_print_string ppf x
  | Doc -> x ppf
  | Pair (a,b) ->
      let x, y = x in
      Format.fprintf ppf "@[(%a,@ %a)@]"
        (fmt_elt {extension} a) x
        (fmt_elt {extension} b) y
  | Triple (a,b,c) ->
      let x, y, z = x in
      Format.fprintf ppf "@[(%a,@ %a,@ %a)@]"
        (fmt_elt {extension} a) x
        (fmt_elt {extension} b) y
        (fmt_elt {extension} c) z
  | Quadruple (a,b,c,d) ->
      let x, y, z ,w = x in
      Format.fprintf ppf "@[(%a,@ %a,@ %a,@ %a)@]"
        (fmt_elt {extension} a) x
        (fmt_elt {extension} b) y
        (fmt_elt {extension} c) z
        (fmt_elt {extension} d) w
  | Custom {pull; default; id } -> begin
      match extension id with
      | Some pr -> pr ppf x
      | None -> fmt_elt {extension} default ppf (pull x)
      end
  |  List elt ->
      Format.fprintf ppf "@[(%a)@]"
      (Format.pp_print_list (fmt_elt {extension} elt)) x
  | Sum _ ->
      fmt_sum {extension} ppf x
  | Record _ ->
      fmt_prod {extension} ppf x
  | Option elt ->
      begin match x with
      | None ->  Format.fprintf ppf "()"
      | Some x ->
          fmt_elt {extension} elt ppf x
      end
and fmt_sum: type s.
  format_extension_printer -> Format.formatter ->
  s sum -> unit
  =
   fun {extension} ppf (Constr (k,x)) ->
   Format.fprintf ppf "@[(%s %a)@]" k.name
         (fmt_elt {extension} k.typ) x
and fmt_prod: type p.
  format_extension_printer -> Format.formatter -> p prod -> unit
  = fun extension ppf prod ->
  Keys.iter (fun key (Constr(kt,x)) ->
      fmt_print extension ~key kt.typ ppf x
    ) prod.fields
and fmt_print: type a. format_extension_printer -> key:string -> a typ -> Format.formatter -> a -> unit =
  fun extension ~key typ ppf x ->
  Format.fprintf ppf "(%s %a)" key (fmt_elt extension typ) x

module Record = struct
  let make _scheme = { fields = Keys.empty }
  let set key f  x= f.fields <- Keys.add key.name (Constr(key,x)) f.fields
  let (.%[]<-) f key x = set key f x
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
    let open Record in
    let r = Record.make V.scheme in
    r.%[major] <- v.major;
    r.%[minor] <- v.minor;
    r
  in
  Custom { pull; id = Version; default = Record V.scheme}

let _version_key =
  Compiler.new_key "version" version_typ

module Error = New_def ()
module Warnings = New_def ()


let no_extension = { extension = fun _ -> None }

let rec make_fmt version ?(ext=no_extension) ppf =
  let init = ref false in
  let initialize () =
    if !init then ()
    else begin
      init := true;
      Format.fprintf ppf "@[<v>(@ %a"
        (fmt_elt ext version_typ) version
  end in
  {
  flush = (fun () -> Format.fprintf ppf ")@]@." );
  sub = (fun ~key:_ -> make_fmt version ~ext ppf);
  print = (fun ~key ty x -> initialize (); fmt_print ext ~key ty ppf x)
}
