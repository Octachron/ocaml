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

(** The log module provides an unified interface for logging in
    a structured log
*)

type !'a def

module type Def = sig
  type id
  type scheme = id def
  val scheme: scheme
end
module New_def():Def


type doc = Format.formatter -> unit

type !'a log
type !'a sum

type empty = Empty_tag

type _ extension = ..

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

type ('a,'b) key


type device = {
  print: 'a. key:string -> 'a typ -> 'a -> unit;
  sub: key:string -> device;
  flush: unit -> unit
}


val new_key: string ->
  'id def -> 'a typ -> ('a,'id) key

val constr: ('a,'id) key -> 'a -> 'id sum

val deprecate_key: ('a,'id) key -> 'id def -> unit



(** {1:log_scheme_versionning  Current version of the log } *)
type version = { major:int; minor:int }

type version_range = { introduction: version; deprecation: version option }

val version: _ def -> version
val name_version: _ def -> version -> unit
val seal_version: _ def -> unit

val version_range: (_,'id) key -> 'id def -> version_range


(** {1:log_creation Log } *)


val flush: 'id log -> unit

val create: device -> version -> 'a def -> 'a log
val detach: ('b log, 'a) key -> 'a log ->  'b log



type format_extension_printer =
  { extension: 'b. 'b extension -> (Format.formatter -> 'b -> unit) option}

val make_fmt: format_extension_printer -> Format.formatter -> device

val set: ('a,'b) key  -> 'a -> 'b log -> unit
val (.%[]<-): 'b log -> ('a,'b) key -> 'a -> unit


val fmt : (string,'a) key -> 'a log -> ('b, Format.formatter, unit) format -> 'b
  (** [logf key log fmt] records the output of [fmt] as
      a string at key [key] in [log].
  *)


(** Compiler logs *)
module Compiler: Def
module Error: Def
