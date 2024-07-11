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
type ('a,'b) field
val field_name: _ field -> string

(** {1:log_scheme_versionning  Current version of the log } *)
module Version: sig
  type t = { major:int; minor:int }
  type version = t
  module Lifetime: sig
    type t = {
      refinement: version option;
      creation: version option;
      expansion: version option;
      deprecation: version option;
      deletion: version option
    }
    type point =
      | Refinement
      | Creation
      | Expansion
      | Deprecation
      | Deletion
      | Future
  end
  val make: major:int -> minor:int -> t
  val pp: Format.formatter -> t -> unit

  type 'a history
  type error =
    | Duplicate_key of string
    | Time_travel of t * t
    | Inconsistent_change of Lifetime.t * string
    | Invalid_constructor_expansion of string
    | Sealed_version of t
  type base_event =
    | Refinement of {base_name:string; new_name:string; typ:string}
    | Creation
    | New_key of {name:string; typ:string}
    | Make_required of string
    | Expansion of {name:string; expansion:string}
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

  val stage: Lifetime.t -> Lifetime.point
  val stage_at: version option -> Lifetime.t -> Lifetime.point

end

type version = Version.t = { major:int; minor:int }

type !'id sum
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

  | Custom: {
      id :'b extension;
      pull: (Version.t option -> 'b -> 'a);
      default: 'a typ
    } -> 'b typ

type any_typ = T: 'a typ -> any_typ
type typed_val = V: 'a typ * 'a -> typed_val
type typed_record = R: 'a def * 'a record -> typed_record
type label_metadata = {
  ltyp: any_typ;
  optional: bool;
  status:Version.Lifetime.t
}
type printer = {
  record: Format.formatter -> typed_record -> unit;
  item: Format.formatter -> string * typed_val -> unit;
}

val destruct: 'a sum -> (string -> typed_val -> 'b) -> 'b

val field_infos: 'a def -> (string * label_metadata) list
val field_names: 'a def -> string list

val scheme_name: 'a def -> string
val fields: string list -> 'a record -> (string * typed_val) List.t
val is_optional: label_metadata -> bool

val log_scheme: 'a log -> 'a def
val log_version: 'a log -> Version.t option

val make:
  structured:bool -> printer:printer -> Misc.Color.setting option ->
  Version.t -> 'a def -> Format.formatter ref -> 'a log

val metakey: string * label_metadata

module type Version_line = sig
  type id
  val history: id Version.history
  val v1: id Version.update
end

module type Def = sig
  type vl
  type id
  type 'a label
  type definition

  type scheme = id def
  type raw_type = definition
  type t = id log

  val scheme: scheme
  val raw_type: definition typ

  val deprecate: vl Version.update -> 'a label -> 'a label
  val delete: vl Version.update -> 'a label -> 'a label
  val seal: vl Version.update -> unit
end

module type Record = sig
  type id
  type nonrec 'a field = ('a,id) field
  include Def
    with type id := id
     and type definition := id record
     and type 'a label := 'a field
  val new_field:
    ?opt:bool -> vl Version.update  -> string -> 'a typ -> 'a field
  val new_field_opt: vl Version.update  -> string -> 'a typ -> 'a field
  val make_required: vl Version.update -> 'a field -> unit
end

module type Sum = sig
  type id
  type 'a constructor
  include Def
    with type id := id
     and type definition := id sum
     and type 'a label := 'a constructor
  val app: Version.t option -> 'a constructor -> 'a -> raw_type

  val refine:
    vl Version.update -> 'a constructor -> ('b -> 'a)
    -> string -> 'b typ -> 'b constructor
  val new_constr: vl Version.update -> string -> 'a typ -> 'a constructor
  val new_constr0: vl Version.update -> string -> unit constructor
  val publish: vl Version.update -> 'a constructor -> 'a constructor
  val expand:
    vl Version.update -> 'a constructor -> ('b->'a) -> 'b typ -> 'b constructor

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


(** {1:log_creation Log } *)


val flush: 'id log -> unit
val separate: 'id log -> unit
val close: 'id log -> unit

val version_range: (_,_) field -> Version.Lifetime.t

val tmp: 'a def -> 'a log

val set: ('a,'b) field  -> 'a -> 'b log -> unit
val (.%[]<-): 'b log -> ('a,'b) field -> 'a -> unit
val cons: ('a list, 'b) field -> 'a -> 'b log -> unit

val get: ('a,'b) field  -> 'b log -> 'a option
val dynamic_get: string  -> 'b log -> typed_val option

val redirect: 'id log -> ('a,'id) field ->
  ?close:(unit -> unit) -> Format.formatter ref -> unit
val replay: 'a log -> 'a log -> unit

val detach: 'id log -> ('id2 record, 'id) field -> 'id2 log
val detach_item: 'id log -> ('id2 record list, 'id) field -> 'id2 log

val f : (string,'a) field -> 'a log -> ('b, Format.formatter, unit) format -> 'b
  (** [fmt field log ppf] records the output of [ppf] as
      a string at field [field] in [log].
  *)

val d :
  (Format_doc.t,'a) field -> 'a log -> ('b, Format_doc.formatter, unit) format
  -> 'b
  (** [fmt field log ppf] records the formatted message at field [field] in
      [log]. *)

val itemf :
  (string list,'a) field -> 'a log -> ('b, Format.formatter, unit) format -> 'b

val itemd :
  (Format_doc.t list,'a) field -> 'a log
  -> ('b, Format_doc.formatter, unit) format -> 'b


module Record: sig
  type 'a bfield
  val (^=): ('a,'b) field -> 'a -> 'b bfield
  val (^=?): ('a,'b) field -> 'a option -> 'b bfield
  val make: Version.t option -> 'a bfield list -> 'a record
end


val log_if:
  'id log -> (string, 'id) field -> bool ->
  (Format.formatter -> 'a -> unit) -> 'a -> unit

(** Metada module *)
module Metadata_versions: Version_line
module Metadata: Record with type vl := Metadata_versions.id
