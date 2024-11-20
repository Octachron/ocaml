(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Florian Angeletti, projet Cambium, Inria Paris             *)
(*                                                                        *)
(*   Copyright 2024 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)


 (** {1:log_scheme_versionning  Current version of the log } *)

type version = { major:int; minor:int }

val version: major:int -> minor:int -> version
val pp: Format.formatter -> version -> unit

(** Lifetime description for fields and constructor *)
module Lifetime: sig

  (** Life cycle of fields and constructor in order *)
  type point =
    | Inception
    | Publication
    | Expansion
    | Deprecation
    | Deletion
    | Future

  type t = {
    inception: version option;
    publication: version option;
    expansion: version option;
    deprecation: version option;
    deletion: version option
  }
end

type 'id t
type error =
  | Duplicate_key of string
  | Time_travel of version * version
  | Inconsistent_change of Lifetime.t * string
  | Invalid_constructor_expansion of string
  | Invalid_publication of string
  | Sealed_version of version
type base_event =
  | Declaration
  | Inception of {base_name:string; new_name:string; typ:string}
  | Publication of string
  | Creation of {name:string; typ:string}
  | Make_required of string
  | Expansion of {name:string; expansion:string}
  | Deprecation of string
  | Deletion of string
  | Seal
  | Error of error
type event = { scheme: string; version:version; event:base_event }
val events: 'a t -> event Seq.t
val current_version: 'a t -> version


(** An ['id update] represents a new version in the history ['id History.t].*)
type 'a update
val new_version: 'a t -> version -> 'a update


(** {2 Versioning event }*)


(** {2 Versioning error }*)
val breaking_change: 'a update -> string -> unit
val inconsistent_if_not_deprecated:
  'a update -> scheme:string -> string -> Lifetime.t -> unit
val inconsistent_if_inactive:
  'a update -> scheme:string -> string -> Lifetime.t -> unit

val invalid_constructor_expansion:
  'a update -> scheme:string -> string -> unit
val invalid_publication: 'a update -> scheme:string -> string -> unit



val register_event: 'a update -> string -> base_event -> unit
val error: 'a update -> string -> error -> unit
val v: 'a update -> version

val range:
  ?deprecation:version ->
  ?deletion:version ->
  ?expansion:version ->
  version -> Lifetime.t
val prerange:
  ?deprecation:version ->
  ?deletion:version ->
  ?expansion:version ->
  ?publication:version ->
  version -> Lifetime.t

val stage: Lifetime.t -> Lifetime.point
val stage_at: version option -> Lifetime.t -> Lifetime.point

module type S = sig
  type id
  val history: id t
  val v1: id update
end

module Make: functor () -> S
