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


module V: Log.Version_line
module type Record = Log.Record with type vl := V.id
module type Sum = Log.Sum with type vl := V.id

type doc = Format_doc.doc
module Structured_text: sig
  module Format_tag: Sum
  type _ Log.extension += Doc: Format_doc.Doc.t Log.extension

  val register_tag:
    Obj.Extension_constructor.t -> (Format.stag -> Format_tag.id Log.sum)
    -> unit
  val register_tag0:
    V.id Log.Version.update -> Obj.Extension_constructor.t
    -> unit

  val typ: doc Log.typ
end

module Debug: sig
  include Record
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
  val unbox_specialised_args:string list  key
  val mach: string list key
  val linear: string list key
  val cmm_invariant: string key
  val profile: string key
end

module Compiler: sig
  include Record
  val debug: Debug.id Log.record key
end
module Error: Record

module Toplevel: sig
  include Record
  val output: doc key
  val backtrace: doc key
  val compiler_log: Compiler.id Log.record key
  val errors: doc list key
  val trace: doc list key
end
