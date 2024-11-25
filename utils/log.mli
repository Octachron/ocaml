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

type !'id log
type 'id t = 'id log
 (** A structured log with tag ['id]. *)

type ('id,'a) field = ('id,'a) Diagnostic.field
(** A field of type ['a] for an ['id log]. *)

(** Lower-level device for log *)
type device
val make_device: ?on_close:(unit->unit) -> Format.formatter ref -> device
val out_channel_device: Out_channel.t -> device
val ppf: device -> Format.formatter
val err: device
val std: device

(** Backend printers *)
type printer = {
  record: Format.formatter -> Diagnostic.typed_record -> unit;
  item: Format.formatter -> string * Diagnostic.typed_val -> unit;
}

val log_version: _ log -> Diagnostic.version option
val log_scheme: 'id log -> 'id Diagnostic.t

val make:
  structured:bool -> printer:printer -> Misc.Color.setting option ->
  Diagnostic_validation.version -> 'a Diagnostic.t -> device -> 'a log

val tmp: 'a Diagnostic.t -> 'a log

(** {1:log_publication Log } *)


val flush: 'id log -> unit
val separate: 'id log -> unit
val close: 'id log -> unit
val redirect: 'id log -> ('a,'id) field -> device -> unit

val set: ('a,'b) field  -> 'a -> 'b log -> unit
val (.%[]<-): 'b log -> ('a,'b) field -> 'a -> unit
val cons: ('a list, 'b) field -> 'a -> 'b log -> unit

val get: ('a,'b) field  -> 'b log -> 'a option
val dynamic_get: string  -> 'b log -> Diagnostic.typed_val option

val replay: 'a log -> 'a log -> unit

val detach: 'id log -> ('id2 Diagnostic.record, 'id) field -> 'id2 log
val detach_item: 'id log -> ('id2 Diagnostic.record list, 'id) field -> 'id2 log

(** {1 Printing functions }*)

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

val log_if:
  'id log -> (string, 'id) field -> bool ->
  (Format.formatter -> 'a -> unit) -> 'a -> unit
