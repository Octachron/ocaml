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

type 'a log_scheme

module type Log_scheme = sig
  type id
  type scheme = id log_scheme
  val scheme: id log_scheme
end
module New_scheme():Log_scheme



type doc = Format.formatter -> unit

type !'a log

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
  | Sum: 'a sum -> 'a sum_constr typ

  | Custom: { id :'b extension; pull: ('b -> 'a); default: 'a typ} ->
      'b typ
  | Sublog: 'id log_scheme -> 'id log typ

and 'a sum =
  | []: empty sum
  | (::): (string * 'a typ) * 'b sum -> ('a * 'b) sum

and ('a,'b) sum_index =
  | Z : ('a * 'b, 'a) sum_index
  | S: ('a, 'b) sum_index -> (_ * 'a,'b) sum_index

and 'a sum_constr = Constr: ('a,'elt) sum_index * 'elt -> 'a sum_constr

val c0: ('a* _, 'a) sum_index
val c1: ( _ * ('a * _), 'a) sum_index
val c2: ( _ * (_ * ('a * _)), 'a) sum_index
val c3: ( _ * (_ * (_ * ('a * _))), 'a) sum_index
val c4: ( _ * (_ * (_ * (_ * ('a * _)))), 'a) sum_index

type ('a,'b) key


type device = {
  print: 'a. key:string -> 'a typ -> 'a -> unit;
  sub: key:string -> device;
  flush: unit -> unit
}


val new_key: string ->
  'id log_scheme -> 'a typ -> ('a,'id) key

val deprecate_key: ('a,'id) key -> 'id log_scheme -> unit



(** {1:log_scheme_versionning  Current version of the log } *)
type version = { major:int; minor:int }

type version_range = { introduction: version; deprecation: version option }

val version: _ log_scheme -> version
val name_version: _ log_scheme -> version -> unit
val seal_version: _ log_scheme -> unit

val version_range: (_,'id) key -> 'id log_scheme -> version_range


(** {1:log_creation Log } *)


val flush: 'id log -> unit

val create: device -> version -> 'a log_scheme -> 'a log
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
module Compiler: Log_scheme
module Error: Log_scheme
