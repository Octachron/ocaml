(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*      Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt          *)
(*                                                                        *)
(*   Copyright 2001 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Module [Outcometree]: results displayed by the toplevel *)

(* These types represent messages that the toplevel displays as normal
   results or errors. The real displaying is customisable using the hooks:
      [Toploop.print_out_value]
      [Toploop.print_out_type]
      [Toploop.print_out_sig_item]
      [Toploop.print_out_phrase] *)

type out_ident =
  | Oide_apply of out_ident * out_ident
  | Oide_dot of out_ident * string
  | Oide_ident of string

type out_string =
  | Ostr_string
  | Ostr_bytes

type out_attribute =
  { oattr_name: string }

type out_value =
  | Oval_array of out_value list
  | Oval_char of char
  | Oval_constr of out_ident * out_value list
  | Oval_ellipsis
  | Oval_float of float
  | Oval_int of int
  | Oval_int32 of int32
  | Oval_int64 of int64
  | Oval_nativeint of nativeint
  | Oval_list of out_value list
  | Oval_printer of (Format.formatter -> unit)
  | Oval_record of (out_ident * out_value) list
  | Oval_string of string * int * out_string (* string, size-to-print, kind *)
  | Oval_stuff of string
  | Oval_tuple of out_value list
  | Oval_variant of string * out_value option

type out_type =
  | Otyp_abstract
  | Otyp_open
  | Otyp_alias of out_type * string
  | Otyp_arrow of out_fn_arg * out_type
  | Otyp_class of bool * out_ident * out_type list
  | Otyp_constr of out_ident * out_type list
  | Otyp_manifest of out_type * out_type
  | Otyp_object of out_object_field list * bool option
  | Otyp_record of out_field list
  | Otyp_stuff of string
  | Otyp_sum of out_constructor list
  | Otyp_tuple of out_type list
  | Otyp_var of bool * string
  | Otyp_variant of
      bool * out_variant * bool * focusable list option
  | Otyp_poly of focusable list * out_type
  | Otyp_module of string * focusable list * out_type list
  | Otyp_attribute of out_type * out_attribute
  | Otyp_ellipsis
  | Otyp_focus of out_type

and out_fn_arg =
  | Ofa_arg of focusable * out_type
  | Ofa_ellipsis

and focusable =
  | Ofoc_simple of string
  | Ofoc_focused of string
  | Ofoc_ellipsis

and out_object_field =
  | Oof_field of bool * string * out_type
  | Oof_ellipsis

and out_constructor =
  | Oc_constr of {focus:bool; name:string; args:out_type list; ret:out_type option}
  | Oc_ellipsis

and out_variant =
  | Ovar_fields of out_var_field list
  | Ovar_typ of out_type

and out_var_field =
  | Ovf_field of {focus:bool; label:string; ampersand:bool; conj: out_type list}
  | Ovf_ellipsis

and out_field =
  | Of_field of {focus:bool; name:string; mut: bool; typ:out_type}
  | Of_ellipsis

type type_constraint =
  | Otc_constraint of {focus:bool; lhs:out_type;rhs:out_type}
  | Otc_ellipsis

type out_class_type =
  | Octy_constr of out_ident * out_type list
  | Octy_arrow of out_fn_arg * out_class_type
  | Octy_signature of out_type option * out_class_sig_item list
and out_class_sig_item =
  | Ocsg_constraint of type_constraint
  | Ocsg_method of string * bool * bool * out_type
  | Ocsg_value of string * bool * bool * out_type
  | Ocsg_focus of out_class_sig_item
  | Ocsg_ellipsis

type type_param =
  | Otp_param of {focus:bool;covariant:bool;contravariant:bool;name:string}
  | Otp_ellipsis

type out_module_type =
  | Omty_abstract
  | Omty_functor of string * out_module_type option * out_module_type
  | Omty_ident of out_ident
  | Omty_signature of out_sig_item list
  | Omty_alias of out_ident
and out_sig_item =
  | Osig_class of
      bool * string * type_param list * out_class_type *
        out_rec_status
  | Osig_class_type of
      bool * string * type_param list * out_class_type *
        out_rec_status
  | Osig_typext of out_extension_constructor * out_ext_status
  | Osig_modtype of string * out_module_type
  | Osig_module of string * out_module_type * out_rec_status
  | Osig_type of out_type_decl * out_rec_status
  | Osig_value of out_val_decl
  | Osig_focus of out_sig_item
  | Osig_ellipsis
and out_type_decl =
  { otype_name: string;
    otype_params: type_param list;
    otype_type: out_type;
    otype_private: Asttypes.private_flag;
    otype_immediate: bool;
    otype_unboxed: bool;
    otype_cstrs: type_constraint list }
and out_extension_constructor =
  { oext_name: string;
    oext_type_name: string;
    oext_type_params: string list;
    oext_args: out_type list;
    oext_ret_type: out_type option;
    oext_private: Asttypes.private_flag }
and out_type_extension =
  { otyext_name: string;
    otyext_params: string list;
    otyext_constructors: out_constructor list;
    otyext_private: Asttypes.private_flag }
and out_val_decl =
  { oval_name: string;
    oval_type: out_type;
    oval_prims: string list;
    oval_attributes: out_attribute list }
and out_rec_status =
  | Orec_not
  | Orec_first
  | Orec_next
and out_ext_status =
  | Oext_first
  | Oext_next
  | Oext_exception

type out_phrase =
  | Ophr_eval of out_value * out_type
  | Ophr_signature of (out_sig_item * out_value option) list
  | Ophr_exception of (exn * out_value)
