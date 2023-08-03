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
  | Custom: { id :'b extension; pull: ('b -> 'a); default: 'a typ} ->
      'b typ
  | Sublog: 'id def -> 'id log typ


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
and 'a log = {
  device: device;
  version: version
}
and device = {
  print: 'a. key:string -> 'a typ -> 'a -> unit;
  sub: key:string -> device;
  flush: unit -> unit
}


module type Def = sig
  type id
  type scheme = id def
  val scheme: scheme
end
module New_def() : Def = struct
  type id
  type scheme = id def
  let scheme =
    { scheme_version = first_version;
      open_scheme = true;
      keys = Keys.empty
    }
end

let flush log = log.device.flush ()

type error =
  | Duplicate_key of string
  | Time_travel of version * version
  | Sealed_version of version
exception Error of error
let error e = raise (Error e)

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

let rec fmt_print : type a. format_extension_printer
  -> key:string -> a typ -> Format.formatter -> a -> unit =
  fun {extension} ~key printer ppf x ->
  match printer with
  | Int -> Format.pp_print_int ppf x
  | String -> Format.pp_print_string ppf x
  | Doc -> x ppf
  | Pair (a,b) ->
      let x, y = x in
      Format.fprintf ppf "(%a,@ %a)"
        (fmt_print {extension} ~key a) x
        (fmt_print {extension} ~key b) y
  | Triple (a,b,c) ->
      let x, y, z = x in
      Format.fprintf ppf "(%a,@ %a,@ %a)"
        (fmt_print {extension} ~key a) x
        (fmt_print {extension} ~key b) y
        (fmt_print {extension} ~key c) z
  | Quadruple (a,b,c,d) ->
      let x, y, z ,w = x in
      Format.fprintf ppf "(%a,@ %a,@ %a,@ %a)"
        (fmt_print {extension} ~key a) x
        (fmt_print {extension} ~key b) y
        (fmt_print {extension} ~key c) z
        (fmt_print {extension} ~key d) w
  | Custom {pull; default; id } -> begin
      match extension id with
      | Some pr -> pr ppf x
      | None -> fmt_print {extension} ~key default ppf (pull x)
      end
  |  List elt ->
      Format.pp_print_list (fmt_print {extension} ~key elt) ppf x
  | Sum _ ->
      fmt_sum {extension} ~key ppf x
  | Sublog _ -> ()
  | Option elt ->
      begin match x with
      | None -> ()
      | Some x ->
          fmt_print {extension} ~key elt ppf x
      end
and fmt_sum: type s.
  format_extension_printer -> key:string -> Format.formatter ->
  s sum -> unit
  =
   fun {extension} ~key ppf (Constr (k,x)) ->
   Format.fprintf ppf "(%s %a)" k.name
         (fmt_print {extension} ~key k.typ) x

let rec make_fmt ext ppf = {
  flush = Format.pp_print_newline ppf;
  sub = (fun ~key:_ -> make_fmt ext ppf);
  print = (fun ~key ty x -> fmt_print ext ~key ty ppf x)
}


let set key x log =
  log.device.print ~key:key.name key.typ x

let (.%[]<-) log key x = set key x log

let fmt key log fmt =
  Format.kasprintf (fun s -> log.%[key] <- s ) fmt


module Compiler = New_def ()
module Error = New_def ()
