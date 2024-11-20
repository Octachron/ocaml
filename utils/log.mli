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

(** The {!Log} module provides an unified interface for logging structured data
    in a log. A log can be printed on a collection of {!Format} device.
*)

 (** The definition of a representation scheme for a type *)
type !'a def

type !'id log
type 'id t = 'id log
 (** A structured log with tag ['id]. *)

type ('id,'a) field
(** A field of type ['a] for the a ['id log]. *)

val field_name: _ field -> string



type version = Diagnostic_history.version = { major:int; minor:int }
type 'a update = 'a Diagnostic_history.update

type diagnostic_version =
  | Downward_compatible of version
  | Exact of version

val diagnostic_version: diagnostic_version -> version
val exact_version: diagnostic_version -> version option

type !'id sum
type !'a record

type empty = Empty_tag

type _ extension = ..

type 'a typ =
  | Unit: unit typ
  | Bool: bool typ
  | Int: int typ
  | Float: float typ
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
      pull: (Diagnostic_history.version option -> 'b -> 'a);
      default: 'a typ
    } -> 'b typ

type any_typ = T: 'a typ -> any_typ
type typed_val = V: 'a typ * 'a -> typed_val
type typed_record = R: 'a def * 'a record -> typed_record
type label_metadata = {
  ltyp: any_typ;
  optional: bool;
  status:Diagnostic_history.Lifetime.t
}
type printer = {
  record: Format.formatter -> typed_record -> unit;
  item: Format.formatter -> string * typed_val -> unit;
}

val destruct: 'a sum -> ((string * typed_val) list -> 'b) -> 'b

val field_infos: 'a def -> (string * label_metadata) list
val field_names: 'a def -> string list

val scheme_name: 'a def -> string
val fields: string list -> 'a record -> (string * bool * typed_val) List.t
val is_optional: label_metadata -> bool

val log_scheme: 'a log -> 'a def
val log_version: 'a log -> version option

type device
val make_device: ?on_close:(unit->unit) -> Format.formatter ref -> device
val out_channel_device: Out_channel.t -> device
val ppf: device -> Format.formatter
val err: device
val std: device

val make:
  structured:bool -> printer:printer -> Misc.Color.setting option ->
  diagnostic_version -> 'a def -> device -> 'a log

val metakey: string * label_metadata

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

  val deprecate: vl update -> 'a label -> 'a label
  val delete: vl update -> 'a label -> 'a label
  val seal: vl update -> unit
end

module type Record = sig
  type id
  type nonrec 'a field = ('a,id) field
  include Def
    with type id := id
     and type definition := id record
     and type 'a label := 'a field
  val new_field: ?opt:bool -> vl update  -> string -> 'a typ -> 'a field
  val new_field_opt: vl update  -> string -> 'a typ -> 'a field
  val make_required: vl update -> 'a field -> unit
end

module type Sum = sig
  type id
  type 'a constructor
  include Def
    with type id := id
     and type definition := id sum
     and type 'a label := 'a constructor
  val app: Diagnostic_history.version option -> 'a constructor -> 'a -> raw_type

  val refine:
    vl update -> 'a constructor -> ('b -> 'a)
    -> string -> 'b typ -> 'b constructor
  val new_constr: vl update -> string -> 'a typ -> 'a constructor
  val new_constr0: vl update -> string -> unit constructor
  val publish: vl update -> 'a constructor -> 'a constructor
  val expand:
    vl update -> 'a constructor -> ('b->'a) -> 'b typ -> 'b constructor

end

module type Info = sig
  type vl
  val name: string
  val update: vl update
end

module New_record (Vl:Diagnostic_history.S):
  (Info with type vl:=Vl.id)-> () -> (Record with type vl := Vl.id)
module New_sum (Vl:Diagnostic_history.S):
  (Info with type vl:=Vl.id) -> () -> (Sum with type vl := Vl.id)


(** {1:log_publication Log } *)


val flush: 'id log -> unit
val separate: 'id log -> unit
val close: 'id log -> unit

val version_range: (_,_) field -> Diagnostic_history.Lifetime.t

val tmp: 'a def -> 'a log

val set: ('a,'b) field  -> 'a -> 'b log -> unit
val (.%[]<-): 'b log -> ('a,'b) field -> 'a -> unit
val cons: ('a list, 'b) field -> 'a -> 'b log -> unit

val get: ('a,'b) field  -> 'b log -> 'a option
val dynamic_get: string  -> 'b log -> typed_val option

val redirect: 'id log -> ('a,'id) field -> device -> unit
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
  val make: Diagnostic_history.version option -> 'a bfield list -> 'a record
end


val log_if:
  'id log -> (string, 'id) field -> bool ->
  (Format.formatter -> 'a -> unit) -> 'a -> unit

(** Metada module *)
module Metadata_versions: Diagnostic_history.S
module Metadata: Record with type vl := Metadata_versions.id
