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
type !'a log
type 'a t = 'a log


type doc = Format.formatter -> unit

type !'a sum
type !'a prod

type empty = Empty_tag

type _ extension = ..

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

type ('a,'b) key
module type Def = sig
  type id
  type scheme = id def
  type log = id t
  type nonrec 'a key = ('a,id) key
  val scheme: scheme
  val new_key: string -> 'a typ -> 'a key
end
module New_def():Def


val new_key: string ->
  'id def -> 'a typ -> ('a,'id) key

val enum: (unit, 'id) key -> 'id sum
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

(*
val create: device -> version -> 'a def -> 'a log
val detach: ('b prod, 'a) key -> 'a log ->  'b log

module Store: sig
  val make: ('a prod -> unit -> unit) -> device
end
*)
module Fmt: sig
  type 'a printer = Format.formatter -> 'a -> unit
  type extension_printer =
    { extension: 'b. 'b extension -> 'b printer option}
  val add_extension: extension_printer -> unit
end

module Backends : sig
  type t = {
    name:string;
    make:
      'a. Misc.Color.setting option -> version -> Format.formatter ref ->
      'a def -> 'a log;
  }
  val fmt: t
  val json: t
  val sexp: t
end

val set: ('a,'b) key  -> 'a -> 'b log -> unit
val redirect: 'id log -> ('a,'id) key ->
  ?close:(unit -> unit) -> Format.formatter ref -> unit
val (.%[]<-): 'b log -> ('a,'b) key -> 'a -> unit
val replay: 'a log -> 'a log -> unit
val detach: 'id log -> ('id2 prod, 'id) key -> 'id2 log

val f : (string,'a) key -> 'a log -> ('b, Format.formatter, unit) format -> 'b
  (** [fmt key log ppf] records the output of [ppf] as
      a string at key [key] in [log].
  *)

val itemf :
  (string list,'a) key -> 'a log -> ('b, Format.formatter, unit) format -> 'b





module Record: sig
  val (=:): ('a,'b) key -> 'a -> 'b sum
  val make: 'a sum list -> 'a prod
end

(** Compiler logs *)
module Debug: sig
  include Def
  val source: string key
  val parsetree: string key
  val typedtree: string key
  val shape: string key
  val instr: string key
  val raw_lambda: string key
  val lambda: string key
  val flambda: string list key
  val raw_flambda: string list key
  val clambda: string list key
  val raw_clambda: string list key
  val cmm: string list key
  val remove_free_vars_equal_to_args: string list key
  val unbox_free_vars_of_closures: string list key
  val unbox_closures:string list key
  val unbox_specialised_args:string list key
  val mach: string list key
  val linear: string list key
  val cmm_invariant: string key
end

module Compiler: sig
  include Def
  val debug: Debug.id prod key
end
module Error: Def
module Warnings: Def

val log_if:
  Debug.log -> string Debug.key -> bool ->
  (Format.formatter -> 'a -> unit) -> 'a -> 'a
