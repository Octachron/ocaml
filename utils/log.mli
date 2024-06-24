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


(** {1:log_scheme_versionning  Current version of the log } *)
module Version: sig
  type t = { major:int; minor:int }
  type range = { creation: t; deprecation: t option; deletion:t option }
  val make: major:int -> minor:int -> t
  val pp: Format.formatter -> t -> unit

  type 'a history
  type error =
    | Duplicate_key of string
    | Time_travel of t * t
    | Inconsistent_change of range * string
    | Sealed_version of t
  type base_event =
    | Creation
    | New_key of {name:string; typ:string}
    | Make_required of string
    | Deprecation of string
    | Deletion of string
    | Seal
    | Error of error
  type event = { scheme: string; version:t; event:base_event }
  val events: 'a history -> event Seq.t
  val current_version: 'a history -> t

  type 'a update
  val new_version: 'a history -> t -> 'a update
  val v: 'a update -> t

end

type version = Version.t = { major:int; minor:int }

type !'id sum
type !'a field
type !'a record

type empty = Empty_tag

type _ extension = ..

type 'a typ =
  | Unit: unit typ
  | Bool: bool typ
  | Int: int typ
  | String: string typ
  | List: 'a typ -> 'a list typ
  | Pair: 'a typ * 'b typ -> ('a * 'b) typ
  | Triple: 'a typ * 'b typ * 'c typ -> ('a * 'b * 'c) typ
  | Quadruple: 'a typ * 'b typ * 'c typ * 'd typ ->
      ('a * 'b * 'c * 'd) typ
  | Sum: 'a def -> 'a sum typ
  | Record: 'id def -> 'id record typ
  | Custom: { id :'b extension; pull: ('b -> 'a); default: 'a typ} ->
      'b typ

type typed_val = V: 'a typ * 'a -> typed_val
type typed_record = R: 'a def * 'a record -> typed_record
type key_metadata =
    Key_metadata:
      { typ: 'a typ;
        optional: bool;
        status:Version.range
      } ->
      key_metadata

type printer = {
  record: Format.formatter -> typed_record -> unit;
  item: Format.formatter -> string * typed_val -> unit;
}

val destruct: 'a sum -> (string -> typed_val -> 'b) -> 'b

val field_infos: 'a def -> (string * key_metadata) list
val field_names: 'a def -> string list

val scheme_name: 'a def -> string
val fields: string list -> 'a record -> (string * typed_val) List.t
val is_optional: key_metadata -> bool

val log_scheme: 'a log -> 'a def
val log_version: 'a log -> Version.t

val make:
  structured:bool -> printer:printer -> Misc.Color.setting option ->
  Version.t -> 'a def -> Format.formatter ref -> 'a log

val metakey: string * key_metadata

module type Version_line = sig
  type id
  val history: id Version.history
  val v1: id Version.update
end

type ('a,'b) key
module type Def = sig
  type vl
  type id
  type definition

  type scheme = id def
  type raw_type = definition
  type t = id log
  type nonrec 'a key = ('a,id) key

  val scheme: scheme
  val raw_type: definition typ

  val deprecate: vl Version.update -> 'a key -> unit
  val delete: vl Version.update -> 'a key -> unit
  val seal: vl Version.update -> unit
end

module type Record = sig
  type id
  include Def with type id := id and type definition := id record
  val make_required: vl Version.update -> 'a key -> unit
  val new_field: vl Version.update  -> string -> 'a typ -> 'a key
  val new_field_opt: vl Version.update  -> string -> 'a typ -> 'a key
end

module type Sum = sig
  type id
  include Def with type id := id and type definition := id sum
  val new_constr: vl Version.update -> string -> 'a typ -> 'a -> id sum
  val new_constr0: vl Version.update -> string -> id sum
end

module type Info = sig
  type vl
  val name: string
  val update: vl Version.update
end

module New_root: () -> Version_line
module New_record (Vl:Version_line):
  (Info with type vl:=Vl.id)-> () -> (Record with type vl := Vl.id)
module New_sum (Vl:Version_line):
  (Info with type vl:=Vl.id) -> () -> (Sum with type vl := Vl.id)

val version_range: (_,'id) key -> 'id def -> Version.range


(** {1:log_creation Log } *)


val flush: 'id log -> unit
val separate: 'id log -> unit
val close: 'id log -> unit


val tmp: 'a def -> 'a log

val set: ('a,'b) key  -> 'a -> 'b log -> unit
val (.%[]<-): 'b log -> ('a,'b) key -> 'a -> unit
val cons: ('a list, 'b) key -> 'a -> 'b log -> unit

val get: ('a,'b) key  -> 'b log -> 'a option
val dynamic_get: string  -> 'b log -> typed_val option

val redirect: 'id log -> ('a,'id) key ->
  ?close:(unit -> unit) -> Format.formatter ref -> unit
val replay: 'a log -> 'a log -> unit

val detach: 'id log -> ('id2 record, 'id) key -> 'id2 log
val detach_item: 'id log -> ('id2 record list, 'id) key -> 'id2 log

val f : (string,'a) key -> 'a log -> ('b, Format.formatter, unit) format -> 'b
  (** [fmt key log ppf] records the output of [ppf] as
      a string at key [key] in [log].
  *)

val d :
  (Format_doc.t,'a) key -> 'a log -> ('b, Format_doc.formatter, unit) format
  -> 'b
  (** [fmt key log ppf] records the formatted message at key [key] in [log].
  *)

val itemf :
  (string list,'a) key -> 'a log -> ('b, Format.formatter, unit) format -> 'b

val itemd :
  (Format_doc.t list,'a) key -> 'a log
  -> ('b, Format_doc.formatter, unit) format -> 'b


module Record: sig
  val (^=): ('a,'b) key -> 'a -> 'b field
  val (^=?): ('a,'b) key -> 'a option -> 'b field list
  val make: 'a field list -> 'a record
end


val log_if:
  'id log -> (string, 'id) key -> bool ->
  (Format.formatter -> 'a -> unit) -> 'a -> unit

(** Metada module *)
module Metadata_versions: Version_line
module Metadata: Record with type vl := Metadata_versions.id
