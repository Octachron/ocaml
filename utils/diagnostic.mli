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


 (** The definition of a representation scheme for a type *)
type 'a t

type 'a update = 'a Diagnostic_history.update
type version = Diagnostic_history.version

type ('id,'a) field
(** A field of type ['a] for the a ['id log]. *)


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
  | Sum: 'a t -> 'a sum typ
  | Record: 'id t -> 'id record typ

  | Custom: {
      id :'b extension;
      pull: (Diagnostic_history.version option -> 'b -> 'a);
      default: 'a typ
    } -> 'b typ

val field_name: _ field -> string
val field_type: ('ty,_) field -> 'ty typ
val version_range: _ field -> Diagnostic_history.Lifetime.t

val record_scheme: 'a record typ -> 'a t
val record_list_scheme: 'a record list typ -> 'a t

(** {2 Instrospection } *)

type any_typ = T: 'a typ -> any_typ
type typed_val = V: 'a typ * 'a -> typed_val
type typed_record = R: 'a t * 'a record -> typed_record
type 'id bound_field = F: ('ty,'id) field * 'ty -> 'id bound_field
type label_metadata = {
  ltyp: any_typ;
  optional: bool;
  status:Diagnostic_history.Lifetime.t
}

val destruct: 'a sum -> ((string * typed_val) list -> 'b) -> 'b
val field_infos: 'a t -> (string * label_metadata) list
val field_names: 'a t -> string list

val scheme_name: 'a t -> string
val fields: string list -> 'a record -> (string * bool * typed_val) List.t
val is_optional: label_metadata -> bool
val field_info: 'id t -> (_,'id) field -> label_metadata option

val metakey: string * label_metadata

module type Def = sig
  type vl
  type id
  type 'a label
  type definition

  type scheme = id t
  type raw_type = definition

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

module Record: sig
  type 'a bfield
  val (^=): ('a,'b) field -> 'a -> 'b bfield
  val (^=?): ('a,'b) field -> 'a option -> 'b bfield
  val all_fields: 'id record -> 'id bound_field Seq.t
  val make: Diagnostic_history.version option -> 'a bfield list -> 'a record
  val get: 'r record -> ('ty,'r) field -> 'ty option
  val dynamic_get: 'r record -> string -> typed_val option
  val set: 'r record -> version option -> field:('ty,'r) field -> 'ty -> unit
  val cons:
    'r record -> version option -> field:('ty list, 'r) field -> 'ty -> unit
  val reset: 'r record -> unit
end

(** Metada module *)
module Metadata_versions: Diagnostic_history.S
module Metadata: Record with type vl := Metadata_versions.id

type diagnostic_version =
  | Downward_compatible of Diagnostic_history.version
  | Exact of Diagnostic_history.version

val diagnostic_version: diagnostic_version -> Diagnostic_history.version
val exact_version: diagnostic_version -> Diagnostic_history.version option

module Validation: sig
  type path = string list
  type report_paths = { deprecated: path list; invalid: path list }
  val diagnostic:
    version:diagnostic_version -> 'a t -> 'a record -> report_paths
end
