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


type highlight = On | Off

type 'a focusable =
  | Ofoc of highlight * 'a
  | Ofoc_ellipsis

type fstring = string focusable
type fbool = bool focusable
type 'a flist = 'a focusable list

type out_ident =
  | Oide_apply of out_ident focusable * out_ident focusable
  | Oide_dot of out_ident focusable * fstring
  | Oide_ident of fstring

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
  | Otyp_alias of out_type focusable * fstring
  | Otyp_arrow of out_labelled focusable * out_type focusable
  | Otyp_class of bool * out_ident focusable * out_type flist
  | Otyp_constr of out_ident focusable * out_type flist
  | Otyp_manifest of out_type focusable * out_type focusable
  | Otyp_object of out_labelled flist * bool option
  | Otyp_record of out_field flist
  | Otyp_stuff of string
  | Otyp_sum of out_constructor flist
  | Otyp_tuple of out_type flist
  | Otyp_var of fbool * fstring
  | Otyp_variant of
      fbool * out_variant * fbool * string flist option
  | Otyp_poly of string flist * out_type focusable
  | Otyp_module of fstring * string flist * out_type flist
  | Otyp_attribute of out_type focusable * out_attribute focusable

and out_labelled = (fstring * out_type focusable)

and out_constructor =
  {cname:fstring; args:out_type flist; ret:out_type focusable option}

and out_variant =
  | Ovar_fields of out_var_field flist
  | Ovar_typ of out_type focusable

and out_var_field = {tag:fstring; ampersand:fbool; conj: out_type flist}

and out_field = {label:fstring; mut: fbool; typ:out_type focusable}

type type_constraint = {lhs:out_type focusable ;rhs:out_type focusable}

type out_class_type =
  | Octy_constr of out_ident focusable * out_type flist
  | Octy_arrow of out_labelled focusable * out_class_type focusable
  | Octy_signature of out_type focusable option * out_class_sig_item flist
and out_class_sig_item =
  | Ocsg_constraint of type_constraint
  | Ocsg_method of fstring * fbool * fbool * out_type focusable
  | Ocsg_value of fstring * fbool * fbool * out_type focusable

type type_param = {covariant:fbool;contravariant:fbool;name:fstring}

type out_module_type =
  | Omty_abstract
  | Omty_functor of
      fstring * out_module_type focusable option * out_module_type focusable
  | Omty_ident of out_ident focusable
  | Omty_signature of out_sig_item flist
  | Omty_alias of out_ident focusable
and out_sig_item =
  | Osig_class of
      fbool * fstring * type_param flist * out_class_type focusable
      * out_rec_status focusable
  | Osig_class_type of
      fbool * fstring * type_param flist * out_class_type focusable
      * out_rec_status focusable
  | Osig_typext of out_extension_constructor * out_ext_status focusable
  | Osig_modtype of fstring * out_module_type focusable
  | Osig_module of fstring * out_module_type focusable * out_rec_status focusable
  | Osig_type of out_type_decl * out_rec_status focusable
  | Osig_value of out_val_decl
and out_type_decl =
  { otype_name: fstring;
    otype_params: type_param flist;
    otype_type: out_type focusable;
    otype_private: Asttypes.private_flag focusable;
    otype_immediate: fbool;
    otype_unboxed: fbool;
    otype_cstrs: type_constraint flist }
and out_extension_constructor =
  { oext_name: fstring;
    oext_type_name: fstring;
    oext_type_params: string flist;
    oext_args: out_type flist;
    oext_ret_type: out_type focusable option;
    oext_private: Asttypes.private_flag focusable }
and out_type_extension =
  { otyext_name: fstring;
    otyext_params: string flist;
    otyext_constructors: out_constructor flist;
    otyext_private: Asttypes.private_flag focusable }
and out_val_decl =
  { oval_name: fstring;
    oval_type: out_type focusable;
    oval_prims: string flist;
    oval_attributes: out_attribute flist }
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
