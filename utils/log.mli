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



(** {1:log_scheme_versionning  Current version of the log } *)
type version = { major:int; minor:int }


type !'a sum
type !'a prod

type empty = Empty_tag

type _ extension = ..

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

type 'a scheme_version
type ('a,'b) key
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
  val new_key: root scheme_version  -> string -> 'a typ -> 'a key
end

module type Sum = sig
  type root
  include Def
  val new_constr: root scheme_version -> string -> 'a typ -> 'a key
end

module type Root = sig
  include Def
  val v1: id scheme_version
  val new_key: id scheme_version  -> string -> 'a typ -> 'a key
  val new_version: version -> id scheme_version
end

module type Name = sig val name: string end
module New_root_scheme: Name -> () -> Root
module New_record(Root:Def): Name -> () -> (Record with type root := Root.id)
module New_sum(Root:Def): Name -> () -> (Sum with type root := Root.id)

val new_key: 'id scheme_version -> 'id def -> string -> 'a typ -> ('a,'id) key

val enum: (unit, 'id) key -> 'id sum
val constr: ('a,'id) key -> 'a -> 'id sum

val deprecate_key: ('a,'id) key -> 'id def -> unit


type version_range = { introduction: version; deprecation: version option }

val version: _ def -> version
val seal_version: _ def -> unit

val version_range: (_,'id) key -> 'id def -> version_range


(** {1:log_creation Log } *)


val flush: 'id log -> unit
val separate: 'id log -> unit
val close: 'id log -> unit


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

val tmp: 'a def -> 'a log

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

module Json_schema:sig
  val pp: Format.formatter -> 'a def -> unit
end

val set: ('a,'b) key  -> 'a -> 'b log -> unit
val cons: ('a list, 'b) key -> 'a -> 'b log -> unit

val redirect: 'id log -> ('a,'id) key ->
  ?close:(unit -> unit) -> Format.formatter ref -> unit
val (.%[]<-): 'b log -> ('a,'b) key -> 'a -> unit
val replay: 'a log -> 'a log -> unit

val detach: 'id log -> ('id2 prod, 'id) key -> 'id2 log
val detach_item: 'id log -> ('id2 prod list, 'id) key -> 'id2 log
val detach_option: 'id log -> ('id2 prod option, 'id) key -> 'id2 log


val f : (string,'a) key -> 'a log -> ('b, Format.formatter, unit) format -> 'b
  (** [fmt key log ppf] records the output of [ppf] as
      a string at key [key] in [log].
  *)

val d : (doc,'a) key -> 'a log -> ('b, Format.formatter, unit) format -> 'b
  (** [fmt key log ppf] records the formatted message at key [key] in [log].
  *)

val o :
  (doc option,'a) key -> 'a log -> ('b, Format.formatter, unit) format -> 'b
  (** [fmt key log ppf] records the formatted message at key [key] in [log].
  *)



val itemf :
  (string list,'a) key -> 'a log -> ('b, Format.formatter, unit) format -> 'b

val itemd :
  (doc list,'a) key -> 'a log -> ('b, Format.formatter, unit) format -> 'b




module Record: sig
  val (=:): ('a,'b) key -> 'a -> 'b sum
  val make: 'a sum list -> 'a prod
end

(** Compiler logs *)

module Compiler_root : Root

module Debug: sig
  include Record with type root := Compiler_root.id
  val source: string option key
  val parsetree: string option key
  val typedtree: string option key
  val shape: string option key
  val instr: string option key
  val raw_lambda: string option key
  val lambda: string option key
  val flambda: string list key
  val raw_flambda: string list key
  val clambda: string list key
  val raw_clambda: string list key
  val cmm: string list key
  val remove_free_vars_equal_to_args: string list key
  val unbox_free_vars_of_closures: string list key
  val unbox_closures:string list key
  val unbox_specialised_args:string list  key
  val mach: string list key
  val linear: string list key
  val cmm_invariant: string option key
end

module Compiler: sig
  include Root
  val debug: Debug.id prod option key
end
module Error: Record with type root := Compiler.id

module Toplevel: sig
  include Root
  val output: doc key
  val backtrace: doc option key
  val compiler_log: Compiler.id prod option key
  val errors: doc list key
  val trace: doc list key
end

val log_if:
  'id log -> (string option, 'id) key -> bool ->
  (Format.formatter -> 'a -> unit) -> 'a -> unit
