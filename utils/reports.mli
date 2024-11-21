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

(** The {!Reports} module provides definition for the structured reports
    emitted by the compiler and toplevel
*)

module D := Diagnostic
module V: Diagnostic_history.S
module type Record = D.Record with type vl := V.id
module type Sum = D.Sum with type vl := V.id

type doc = Format_doc.doc
module Structured_text: sig
  module Format_tag: Sum
  type _ D.extension += Doc: Format_doc.Doc.t D.extension

  val register_tag:
    Obj.Extension_constructor.t
    -> (Log.version option -> Format.stag -> Format_tag.id D.sum)
    -> unit
  val register_tag0:
    V.id Log.update -> Obj.Extension_constructor.t
    -> unit

  val typ: doc D.typ
end

module Debug: sig
  include Record
  val source: string field
  val parsetree: string field
  val typedtree: string field
  val shape: string field
  val instr: string field
  val raw_lambda: string field
  val lambda: string field
  val flambda: string list field
  val raw_flambda: string list field
  val clambda: string list field
  val raw_clambda: string list field
  val cmm: string list field
  val remove_free_vars_equal_to_args: string list field
  val unbox_free_vars_of_closures: string list field
  val unbox_closures:string list field
  val unbox_specialised_args:string list  field
  val mach: string list field
  val linear: string list field
  val cmm_invariant: string field
end

module Compiler: sig
  include Record
  val debug: Debug.id D.record field
end
module Error: Record

module Toplevel: sig
  include Record
  val output: doc field
  val backtrace: doc field
  val compiler_log: Compiler.id D.record field
  val errors: doc list field
  val trace: doc list field
end

(** Access to configuration values *)
module Config_versions: Diagnostic_history.S
module Config: sig
  include D.Record with type vl := Config_versions.id

  val print_config : id Log.t -> unit
  val config_var : string -> string option
  (** the configuration value of a variable, if it exists *)

  (** {1 Displaying configuration variables} *)

  val show_config_variable_and_exit : string -> unit
  (** Display the value of the given configuration variable,
      then exit the program with code 0. *)

end
