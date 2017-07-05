(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Printing functions *)

open Format
open Types
open Outcometree

val longident: formatter -> Longident.t -> unit
val ident: formatter -> Ident.t -> unit

val tpath: Path.t -> out_ident
val path: formatter -> Path.t -> unit
val string_of_path: Path.t -> string
val raw_type_expr: formatter -> type_expr -> unit
val string_of_label: Asttypes.arg_label -> string

val wrap_printing_env: Env.t -> (unit -> 'a) -> 'a
    (* Call the function using the environment for type path shortening *)
    (* This affects all the printing functions below *)

val reset: unit -> unit
val mark_loops: type_expr -> unit
val reset_and_mark_loops: type_expr -> unit
val reset_and_mark_loops_list: type_expr list -> unit
val type_expr: formatter -> type_expr -> unit
val constructor_arguments: constructor_arguments -> out_type list
val type_scheme: type_expr -> out_type
val print_type: Format.formatter -> Decorated.out_type -> unit
val type_sch : formatter -> type_expr -> unit
val pp_type_scheme: formatter -> type_expr -> unit
(* Maxence *)
val reset_names: unit -> unit
val type_scheme_max: ?b_reset_names: bool ->
        formatter -> type_expr -> unit
(* End Maxence *)
val print_sigitem: formatter -> Decorated.out_sig_item -> unit
val value_description: Ident.t -> value_description -> out_sig_item
val type_declaration: Ident.t -> type_declaration -> out_sig_item
val extension_constructor:
    Ident.t -> extension_constructor -> out_sig_item
val module':
  Ident.t -> (*?ellipsis:bool ->*) module_type -> rec_status -> out_sig_item
val modtype:  module_type -> out_module_type
val print_modtype: Format.formatter -> Decorated.out_module_type -> unit
val modtype_declaration:
    Ident.t -> modtype_declaration -> out_sig_item
val signature: Types.signature -> out_sig_item list
val print_signature: Format.formatter -> Decorated.out_sig_item list -> unit
val typexp: bool -> type_expr -> out_type

val class_declaration:
    Ident.t -> class_declaration -> out_sig_item
val cltype_declaration:
    Ident.t -> class_type_declaration -> out_sig_item

val class_type: class_type -> out_class_type
val print_class_type: Format.formatter -> Decorated.out_class_type -> unit

val print_constructor_args: Format.formatter -> out_type list -> unit


val type_expansion:
  type_expr -> type_expr -> (Outcometree.out_type as 'ty) * 'ty option

val type_diff: (type_expr * type_expr as 'pair) -> 'pair ->
  ((Decorated.out_type as 'ty) * 'ty option as 'diffpair) * 'diffpair

val print_expansion:
  Format.formatter -> (Decorated.out_type as 'ty) * 'ty option -> unit

val prepare_expansion: type_expr * type_expr -> type_expr * type_expr
val trace:
    bool -> bool-> string -> formatter -> (type_expr * type_expr) list -> unit
val report_unification_error:
    formatter -> Env.t -> ?unif:bool -> (type_expr * type_expr) list ->
    (formatter -> unit) -> (formatter -> unit) ->
    unit
val report_subtyping_error:
    formatter -> Env.t -> (type_expr * type_expr) list ->
    string -> (type_expr * type_expr) list -> unit
val report_ambiguous_type_error:
    formatter -> Env.t -> (Path.t * Path.t) -> (Path.t * Path.t) list ->
    (formatter -> unit) -> (formatter -> unit) -> (formatter -> unit) -> unit

(* for toploop *)
val print_items: (Env.t -> signature_item -> 'a option) ->
  Env.t -> signature_item list -> (out_sig_item * 'a option) list
