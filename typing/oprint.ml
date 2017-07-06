(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Projet Cristal, INRIA Rocquencourt                   *)
(*                                                                        *)
(*   Copyright 2002 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Format
open Outcometree
open Decorated
module H = Highlightable

exception Ellipsis

let ref_ellipsis = ref "..."
let times = ref "*"
let ellipsis ppf = fprintf ppf "%s" !ref_ellipsis

let cautious f ppf arg =
  try f ppf arg with
    Ellipsis -> ellipsis ppf

let prf pp ppf = function
  | H.Ellipsis n ->
      if n > 1 then
        fprintf ppf "%t %s %d" ellipsis !times n
      else ellipsis ppf
  | H.Item(H.Off, s) -> pp ppf s
  | H.Item(H.On, s) -> fprintf ppf "@{<focus>%a@}" pp s

let fbool name ppf = function
  | H.Ellipsis _ -> ()
  | H.Item(H.Off, b) ->
      if b then fprintf ppf name
  | H.Item(H.On, b) ->
      if b then fprintf ppf ("@{<focus>" ^^ name ^^ "@}")

let bool name ppf b = if b then fprintf ppf name

let string = Format.pp_print_string
let fstring x = prf string x


let rec print_ident ppf =
  function
    Oide_ident s -> fstring ppf s
  | Oide_dot (id, s) ->
      prf print_ident ppf id; pp_print_char ppf '.'; fstring ppf s
  | Oide_apply (id1, id2) ->
      fprintf ppf "%a(%a)" (prf print_ident) id1 (prf print_ident) id2

let parenthesized_ident name =
  (List.mem name ["or"; "mod"; "land"; "lor"; "lxor"; "lsl"; "lsr"; "asr"])
  ||
  (match name.[0] with
      'a'..'z' | 'A'..'Z' | '\223'..'\246' | '\248'..'\255' | '_' ->
        false
    | _ -> true)

let value_ident ppf name =
  if parenthesized_ident name then
    fprintf ppf "( %s )" name
  else
    pp_print_string ppf name

(* Values *)

let valid_float_lexeme s =
  let l = String.length s in
  let rec loop i =
    if i >= l then s ^ "." else
    match s.[i] with
    | '0' .. '9' | '-' -> loop (i+1)
    | _ -> s
  in loop 0

let float_repres f =
  match classify_float f with
    FP_nan -> "nan"
  | FP_infinite ->
      if f < 0.0 then "neg_infinity" else "infinity"
  | _ ->
      let float_val =
        let s1 = Printf.sprintf "%.12g" f in
        if f = float_of_string s1 then s1 else
        let s2 = Printf.sprintf "%.15g" f in
        if f = float_of_string s2 then s2 else
        Printf.sprintf "%.18g" f
      in valid_float_lexeme float_val

let parenthesize_if_neg ppf fmt v isneg =
  if isneg then pp_print_char ppf '(';
  fprintf ppf fmt v;
  if isneg then pp_print_char ppf ')'

let ext_to_constr focused cname args ret =
      H.Item(focused, {cname; args; ret })

let gather_extensions foc ext proj items =
  (* Gather together the extension constructors *)
  let rec gather_extensions acc items =
    match items with
    |  item :: items as all ->
        begin match proj item with
        | H.Item(foc, Osig_typext(ext, H.Item(_, Oext_next))) ->
            gather_extensions
              (ext_to_constr foc ext.oext_name ext.oext_args ext.oext_ret_type
               :: acc) items
        | _ -> (List.rev acc, all)
        end
    | _ -> (List.rev acc, items)
  in
  gather_extensions
    [ext_to_constr foc ext.oext_name ext.oext_args ext.oext_ret_type]
    items


let print_out_value ppf tree =
  let rec print_tree_1 ppf =
    function
    | Oval_constr (name, [param]) ->
        fprintf ppf "@[<1>%a@ %a@]" print_ident name print_constr_param param
    | Oval_constr (name, (_ :: _ as params)) ->
        fprintf ppf "@[<1>%a@ (%a)@]" print_ident name
          (print_tree_list print_tree_1 ",") params
    | Oval_variant (name, Some param) ->
        fprintf ppf "@[<2>`%s@ %a@]" name print_constr_param param
    | tree -> print_simple_tree ppf tree
  and print_constr_param ppf = function
    | Oval_int i -> parenthesize_if_neg ppf "%i" i (i < 0)
    | Oval_int32 i -> parenthesize_if_neg ppf "%lil" i (i < 0l)
    | Oval_int64 i -> parenthesize_if_neg ppf "%LiL" i (i < 0L)
    | Oval_nativeint i -> parenthesize_if_neg ppf "%nin" i (i < 0n)
    | Oval_float f -> parenthesize_if_neg ppf "%s" (float_repres f) (f < 0.0)
    | Oval_string (_,_, Ostr_bytes) as tree ->
      pp_print_char ppf '(';
      print_simple_tree ppf tree;
      pp_print_char ppf ')';
    | tree -> print_simple_tree ppf tree
  and print_simple_tree ppf =
    function
      Oval_int i -> fprintf ppf "%i" i
    | Oval_int32 i -> fprintf ppf "%lil" i
    | Oval_int64 i -> fprintf ppf "%LiL" i
    | Oval_nativeint i -> fprintf ppf "%nin" i
    | Oval_float f -> pp_print_string ppf (float_repres f)
    | Oval_char c -> fprintf ppf "%C" c
    | Oval_string (s, maxlen, kind) ->
       begin try
               let len = String.length s in
                fprintf ppf "%s%S%s"
                  (match kind with
                  | Ostr_string -> ""
                  | Ostr_bytes -> "Bytes.of_string ")
                  (if len > maxlen then String.sub s 0 maxlen else s)
                  (if len > maxlen then
                      asprintf
                        "%t (* string length %d; truncated *)" ellipsis len
                   else ""
                  )
          with
          Invalid_argument _ (* "String.create" *)-> fprintf ppf "<huge string>"
        end
    | Oval_list tl ->
        fprintf ppf "@[<1>[%a]@]" (print_tree_list print_tree_1 ";") tl
    | Oval_array tl ->
        fprintf ppf "@[<2>[|%a|]@]" (print_tree_list print_tree_1 ";") tl
    | Oval_constr (name, []) -> print_ident ppf name
    | Oval_variant (name, None) -> fprintf ppf "`%s" name
    | Oval_stuff s -> pp_print_string ppf s
    | Oval_record fel ->
        fprintf ppf "@[<1>{%a}@]" (cautious (print_fields true)) fel
    | Oval_ellipsis -> raise Ellipsis
    | Oval_printer f -> f ppf
    | Oval_tuple tree_list ->
        fprintf ppf "@[<1>(%a)@]" (print_tree_list print_tree_1 ",") tree_list
    | tree -> fprintf ppf "@[<1>(%a)@]" (cautious print_tree_1) tree
  and print_fields first ppf =
    function
      [] -> ()
    | (name, tree) :: fields ->
        if not first then fprintf ppf ";@ ";
        fprintf ppf "@[<1>%a@ =@ %a@]" print_ident name (cautious print_tree_1)
          tree;
        print_fields false ppf fields
  and print_tree_list print_item sep ppf tree_list =
    let rec print_list first ppf =
      function
        [] -> ()
      | tree :: tree_list ->
          if not first then fprintf ppf "%s@ " sep;
          print_item ppf tree;
          print_list false ppf tree_list
    in
    cautious (print_list true) ppf tree_list
  in
  cautious print_tree_1 ppf tree

let out_value = ref print_out_value

(* Types *)

let rec print_list pr sep ppf =
  function
    [] -> ()
  | [a] -> pr ppf a
  | a :: l -> pr ppf a; sep ppf; print_list pr sep ppf l

let pr_present =
  print_list
    (fun ppf s -> fprintf ppf "`%a" fstring s)
    (fun ppf -> fprintf ppf "@ ")

let pr_vars =
  print_list (fun ppf s -> fprintf ppf "'%a" fstring s)
    (fun ppf -> fprintf ppf "@ ")

let pp_nongen = fbool "_"

let pp_closed no_tags ppf closed =
  let pp ppf closed =
    fprintf ppf  "%s"
      (if closed then if no_tags then " " else "< "
       else if no_tags then "> " else "? ") in
  prf pp ppf closed

let print_private ppf = function
    Asttypes.Private -> fprintf ppf " private"
  | Asttypes.Public -> ()


let rec print_out_type ppf =
  function
  | Otyp_alias (ty, s) ->
      fprintf ppf "@[%a@ as '%a@]" (prf print_out_type) ty fstring s
  | Otyp_poly (sl, ty) ->
      fprintf ppf "@[<hov 2>%a.@ %a@]"
        pr_vars sl
        (prf print_out_type) ty
  | ty ->
      print_out_type_1 ppf ty

and print_out_type_1 ppf =
  function
    Otyp_arrow (arg, ty2) ->
      pp_open_box ppf 0;
      prf print_fn_arg ppf arg;
      pp_print_string ppf " ->";
      pp_print_space ppf ();
      prf print_out_type_1 ppf ty2;
      pp_close_box ppf ()
  | ty -> print_out_type_2 ppf ty
and print_fn_arg ppf (lab,ty1) =
        begin match lab with
        | H.Item(_, "") -> ()
        | _ ->
          (fstring ppf lab; pp_print_char ppf ':') end;
        prf print_out_type_2 ppf ty1

and print_out_type_2 ppf =
  function
    Otyp_tuple tyl ->
      fprintf ppf "@[<0>%a@]" (print_typlist print_simple_out_type " *") tyl
  | ty -> print_simple_out_type ppf ty
and print_simple_out_type ppf =
  function
    Otyp_class (ng, id, tyl) ->
      fprintf ppf "@[%a%a#%a@]" print_typargs tyl
        (fbool "_") ng
        (prf print_ident) id
  | Otyp_constr (id, tyl) ->
      pp_open_box ppf 0;
      print_typargs ppf tyl;
      prf print_ident ppf id;
      pp_close_box ppf ()
  | Otyp_object (fields, rest) ->
      fprintf ppf "@[<2>< %a >@]" (print_fields rest) fields
  | Otyp_stuff s -> pp_print_string ppf s
  | Otyp_var (ng, s) ->
      fprintf ppf "'%a%a" pp_nongen ng fstring s
  | Otyp_variant (non_gen, row_fields, closed, tags) ->
      let print_present ppf =
        function
          None | Some [] -> ()
        | Some l -> fprintf ppf "@;<1 -2>> @[<hov>%a@]" pr_present l
      in
      let print_fields ppf =
        function
          Ovar_fields fields ->
            print_list (prf print_row_field) (fun ppf -> fprintf ppf "@;<1 -2>| ")
              ppf fields
        | Ovar_typ typ -> prf print_simple_out_type ppf typ
      in
      fprintf ppf "%a[%a@[<hv>@[<hv>%a@]%a ]@]"
        pp_nongen non_gen
        (pp_closed (match tags with H.Item(_,None) -> true | _ -> false)) closed
        print_fields row_fields
        (prf print_present) tags
  | Otyp_alias _ | Otyp_poly _ | Otyp_arrow _ | Otyp_tuple _ as ty ->
      pp_open_box ppf 1;
      pp_print_char ppf '(';
      print_out_type ppf ty;
      pp_print_char ppf ')';
      pp_close_box ppf ()
  | Otyp_abstract | Otyp_open
  | Otyp_sum _ | Otyp_manifest (_, _) -> ()
  | Otyp_record lbls -> print_record_decl ppf lbls
  | Otyp_module (p, n, tyl) ->
      fprintf ppf "@[<1>(module %a" fstring p;
      let first = ref true in
      List.iter2
        (fun s t ->
          let sep = if !first then (first := false; "with") else "and" in
          fprintf ppf " %s type %a = %a" sep fstring s
            (prf print_out_type) t
        )
        n tyl;
      fprintf ppf ")@]"
  | Otyp_attribute (t, attr) ->
      let pr_attr ppf attr = fprintf ppf " [@@%s]" attr.oattr_name in
      fprintf ppf "@[<1>(%a%a)@]" (prf print_out_type) t (prf pr_attr) attr
and print_record_decl ppf lbls =
  fprintf ppf "{@ %a;@;<1 -2>}"
    (print_list (prf print_out_label) (fun ppf -> fprintf ppf ";@ ")) lbls
and print_field ppf (s,t) =
  fprintf ppf "%a : %a" fstring s (prf print_out_type) t
and print_fields rest ppf =
  function
    [] ->
      let pp_rest ppf = function
          Some non_gen -> fprintf ppf "%a.." (fbool "_") non_gen
        | None -> ()
      in
      prf pp_rest ppf rest
  | [a] -> prf print_field ppf a;
      begin match rest with
        H.Item(_, Some _) -> fprintf ppf ";@ "
      | H.Item(_,None) | H.Ellipsis _ -> ()
      end;
      print_fields rest ppf []
  | a :: l ->
      prf print_field ppf a;
      fprintf ppf ";@ %a" (print_fields rest) l
and print_row_field ppf f =
      let pr_of ppf =
        if f.conj <> [] then
          fprintf ppf " of%a@ " (fbool "@ &") f.ampersand
      in
      fprintf ppf "@[<hv 2>`%a%t%a@]"
        fstring f.tag pr_of (print_typlist print_out_type " &") f.conj
and print_typlist print_elem sep ppf =
  function
    [] -> ()
  | [ty] -> prf print_elem ppf ty
  | ty :: tyl ->
      prf print_elem ppf ty;
      pp_print_string ppf sep;
      pp_print_space ppf ();
      print_typlist print_elem sep ppf tyl
and print_typargs ppf =
  function
    [] -> ()
  | [ty1] -> prf print_simple_out_type ppf ty1; pp_print_space ppf ()
  | tyl ->
      pp_open_box ppf 1;
      pp_print_char ppf '(';
      print_typlist print_out_type "," ppf tyl;
      pp_print_char ppf ')';
      pp_close_box ppf ();
      pp_print_space ppf ()
and print_out_label ppf {label;mut;typ} =
      fprintf ppf "@[<2>%a%a :@ %a@]"
        (fbool "mutable ") mut fstring label
        (prf print_out_type) typ

let out_type = ref print_out_type

(* Class types *)
let pr_typename_0 ppf name =
  if name = "_" then fprintf ppf "_" else
    fprintf ppf "'%s" name

let pr_typename = prf pr_typename_0

let hmap f = function
  | H.Item(flag, s) -> H.Item(flag, f s)
  | H.Ellipsis _ as x -> x

let type_parameter ppf {covariant;contravariant;name} =
      fprintf ppf "%a%a%a"
        (fbool "+") (hmap not contravariant)
        (fbool "-") (hmap not covariant)
        pr_typename name

let print_out_class_params ppf =
  function
    [] -> ()
  | tyl ->
      fprintf ppf "@[<1>[%a]@]@ "
        (print_list (prf type_parameter) (fun ppf -> fprintf ppf ", "))
        tyl

let rec print_out_class_type ppf =
  function
    Octy_constr (id, tyl) ->
      let pr_tyl ppf =
        function
          [] -> ()
        | tyl ->
            fprintf ppf "@[<1>[%a]@]@ " (print_typlist !out_type ",") tyl
      in
      fprintf ppf "@[%a%a@]" pr_tyl tyl (prf print_ident) id
  | Octy_arrow (arg, cty) ->
      fprintf ppf "@[%a ->@ %a@]" (prf print_fn_arg) arg
        (prf print_out_class_type) cty
  | Octy_signature (self_ty, csil) ->
      let pr_param ppf =
        function
          Some ty -> fprintf ppf "@ @[(%a)@]" (prf !out_type) ty
        | None -> ()
      in
      fprintf ppf "@[<hv 2>@[<2>object%a@]@ %a@;<1 -2>end@]" (prf pr_param) self_ty
        (print_list (prf print_out_class_sig_item) (fun ppf -> fprintf ppf "@ "))
        csil
and print_out_class_sig_item ppf =
  function
    Ocsg_constraint c ->
      fprintf ppf "@[<2>constraint %a =@ %a@]" (prf !out_type) c.lhs
        (prf !out_type) c.rhs
  | Ocsg_method (name, priv, virt, ty) ->
      fprintf ppf "@[<2>method %a%a%a :@ %a@]"
        (fbool "private ") priv (fbool "virtual ") virt
        fstring name (prf !out_type) ty
  | Ocsg_value (name, mut, vr, ty) ->
      fprintf ppf "@[<2>val %a%a%a :@ %a@]"
        (fbool "mutable ") mut
        (fbool "virtual ") vr
        fstring name (prf !out_type) ty

let out_class_type = ref print_out_class_type

(* Signature *)

let out_module_type = ref (fun _ -> failwith "Oprint.out_module_type")
let out_sig_item = ref (fun _ -> failwith "Oprint.out_sig_item")
let out_signature = ref (fun _ -> failwith "Oprint.out_signature")
let out_type_extension = ref (fun _ -> failwith "Oprint.out_type_extension")

let rec print_out_functor funct ppf = function
  | Omty_functor (arg , mty_res) ->
      fprintf ppf "%a%a"
        (prf @@ print_arg funct) arg
        (prf @@ print_out_functor @@ as_funct arg) mty_res
  | m -> fprintf ppf "%a%a" (bool "->@ ") funct print_out_module_type m
and as_funct = function
  | H.Item(_, (_, H.Item(_, None))) -> true
  | H.Ellipsis _ -> false
  | H.Item(_, (H.Item(_, "_"), _)) -> false
  | _ -> true
and print_arg funct ppf (name,mty) =
    match mty with
    | H.Item(h, None) ->
        let arg ppf = prf string ppf (H.Item(h,"()")) in
        fprintf ppf "%a%t "
          (bool "functor@ ") (not funct) arg
    | H.Ellipsis n ->
        print_arg_name funct (H.Ellipsis n) ppf name
    | H.Item(h,Some f) ->
        let f = match f with
          | H.Item(H.Off,f) -> H.Item(h,f)
          | x -> x in
        print_arg_name funct f ppf name
and print_arg_name funct mty ppf = function
    | H.Item(_, "_") ->
        fprintf ppf "%a%a ->@ " (bool "->@ ") funct
          (prf print_out_module_type) mty
    | name ->
        fprintf ppf "%a(%a : %a) " (bool "functor@ ") (not funct)
          fstring name
          (prf print_out_module_type) mty

and print_out_module_type ppf =
  function
    Omty_abstract -> ()
  | Omty_functor _ as t ->
      fprintf ppf "@[<2>%a@]" (print_out_functor false) t
  | Omty_ident id -> fprintf ppf "%a" (prf print_ident) id
  | Omty_signature sg ->
      fprintf ppf "@[<hv 2>sig@ %a@;<1 -2>end@]" print_out_signature sg
  | Omty_alias id -> fprintf ppf "(module %a)" (prf print_ident) id
and print_out_signature ppf =
  function
    [] -> ()
  | [item] -> prf !out_sig_item ppf item
  | H.Item(foc, Osig_typext(ext, H.Item( _, Oext_first)))
    :: items ->
      let exts, items = gather_extensions foc ext (fun x -> x) items in
      let te =
        { otyext_name = ext.oext_type_name;
          otyext_params = ext.oext_type_params;
          otyext_constructors = exts;
          otyext_private = ext.oext_private }
      in
        fprintf ppf "%a@ %a" !out_type_extension te print_out_signature items
  | item :: items ->
      fprintf ppf "%a@ %a" (prf !out_sig_item) item print_out_signature items
and print_out_sig_item ppf =
  function
    Osig_class (vir_flag, name, params, clt, rs) ->
      let pp_recs ppf rs =
       fprintf ppf (if rs = Orec_next then "and" else "class") in
      fprintf ppf "@[<2>%a%a@ %a%a@ :@ %a@]" (prf pp_recs) rs
        (fbool " virtual") vir_flag print_out_class_params params
        fstring name (prf !out_class_type) clt
  | Osig_class_type (vir_flag, name, params, clt, rs) ->
      let pp_recs ppf rs =
        fprintf ppf (if rs = Orec_next then "and" else "class type") in
      fprintf ppf "@[<2>%a%a@ %a%a@ =@ %a@]"
        (prf pp_recs) rs
        (fbool " virtual") vir_flag print_out_class_params params
        fstring name (prf !out_class_type) clt
  | Osig_typext (ext, H.Item(foc,Oext_exception)) ->
      fprintf ppf "@[<2>exception %a@]"
        (prf print_out_constr)
        (ext_to_constr foc ext.oext_name ext.oext_args ext.oext_ret_type)
  | Osig_typext (ext, _es) ->
      print_out_extension_constructor ppf (H.Off,ext)
  | Osig_modtype (name, H.Item(_, Omty_abstract)) ->
      fprintf ppf "@[<2>module type %a@]" fstring name
  | Osig_modtype (name, mty) ->
      fprintf ppf "@[<2>module type %a =@ %a@]" fstring name
        (prf !out_module_type) mty
  | Osig_module (name, H.Item(_, Omty_alias id), _) ->
      fprintf ppf "@[<2>module %a =@ %a@]" fstring name (prf print_ident) id
  | Osig_module (name, mty, rs) ->
      let print_rec ppf rs = fprintf ppf
          (match rs with Orec_not -> "module"
                       | Orec_first -> "module rec"
                       | Orec_next -> "and") in
      fprintf ppf "@[<2>%a %a :@ %a@]" (prf print_rec) rs
        fstring name (prf !out_module_type) mty
  | Osig_type(td, rs) ->
      let print_recs ppf rs = fprintf ppf (match rs with
          | Orec_not   -> "type nonrec"
          | Orec_first -> "type"
          | Orec_next  -> "and") in
        print_out_type_decl (fun ppf -> prf print_recs ppf rs) ppf td
  | Osig_value vd ->
      let kwd = if vd.oval_prims = [] then "val" else "external" in
      let pr_prims ppf =
        function
          [] -> ()
        | s :: sl ->
            fprintf ppf "@ = \"%a\"" fstring s;
            List.iter (fun s -> fprintf ppf "@ \"%a\"" fstring s) sl
      in
      let pr_attr ppf a = fprintf ppf "@ [@@@@%s]" a.oattr_name in
      fprintf ppf "@[<2>%s %a :@ %a%a%a@]" kwd (prf value_ident)
        vd.oval_name
        (prf !out_type) vd.oval_type pr_prims vd.oval_prims
        (fun ppf -> List.iter (prf pr_attr ppf))
        vd.oval_attributes
  | Osig_ellipsis -> ellipsis ppf

and print_out_type_decl kwd ppf td =
  let print_constraint ppf {lhs;rhs} =
        fprintf ppf "@ @[<2>constraint %a =@ %a@]"
          (prf !out_type) lhs (prf !out_type) rhs in
  let print_constraints ppf =
    print_list (prf print_constraint) (fun ppf -> fprintf ppf "@ ") ppf
      td.otype_cstrs
  in
  let type_defined ppf =
    match td.otype_params with
      [] -> fstring ppf td.otype_name
    | [param] ->
        fprintf ppf "@[%a@ %a@]" (prf type_parameter) param fstring td.otype_name
    | _ ->
        fprintf ppf "@[(@[%a)@]@ %a@]"
          (print_list (prf type_parameter) (fun ppf -> fprintf ppf ",@ "))
          td.otype_params
          fstring td.otype_name
  in
  let print_manifest ppf =
    function
    | Otyp_manifest (ty, _) -> fprintf ppf " =@ %a" (prf !out_type) ty
    | _ -> ()
  in
  let print_name_params ppf =
    fprintf ppf "%t %t%a" kwd type_defined (prf print_manifest) td.otype_type
  in
  let ty =
    match td.otype_type with
    | H.Item(_, Otyp_manifest (_, ty)) -> ty
    | _ -> td.otype_type
  in
  let print_immediate ppf =
    fbool" [%@%@immediate]" ppf td.otype_immediate
  in
  let print_unboxed ppf =
    fbool " [%@%@unboxed]" ppf td.otype_unboxed
  in
  let print_out_tkind ppf = function
  | Otyp_abstract -> ()
  | Otyp_record lbls ->
      fprintf ppf " =%a %a"
        (prf print_private) td.otype_private
        print_record_decl lbls
  | Otyp_sum constrs ->
      fprintf ppf " =%a@;<1 2>%a"
        (prf print_private) td.otype_private
        (print_list (prf print_out_constr) (fun ppf -> fprintf ppf "@ | ")) constrs
  | Otyp_open ->
      fprintf ppf " = .."
  | ty ->
      fprintf ppf " =%a@;<1 2>%a"
        (prf print_private) td.otype_private
        !out_type ty
  in
  fprintf ppf "@[<2>@[<hv 2>%t%a@]%t%t%t@]"
    print_name_params
    (prf print_out_tkind) ty
    print_constraints
    print_immediate
    print_unboxed

and print_out_constr ppf {cname; args; ret} =
  let name =
    hmap (function
    | "::" -> "(::)"   (* #7200 *)
    | s -> s) cname
  in
  let pp_ret ppf = function
    | None ->
        begin match args with
        | [] ->
            fstring ppf name
        | _ ->
            fprintf ppf "@[<2>%a of@ %a@]" fstring name
              (print_typlist print_simple_out_type " *") args
        end
    | Some ret_type ->
        begin match args with
        | [] ->
            fprintf ppf "@[<2>%a :@ %a@]" fstring name
              (prf print_simple_out_type) ret_type
        | _ ->
            fprintf ppf "@[<2>%a :@ %a -> %a@]" fstring name
              (print_typlist print_simple_out_type " *")
              args (prf print_simple_out_type) ret_type
        end in
  prf pp_ret ppf ret

and print_out_extension_constructor ppf (foc,ext) =
  let print_extended_type ppf =
      match ext.oext_type_params with
        [] -> fprintf ppf "%a" fstring ext.oext_type_name
      | [ty_param] ->
        fprintf ppf "@[%a@ %a@]"
          pr_typename ty_param
          fstring ext.oext_type_name
      | _ ->
        fprintf ppf "@[(@[%a)@]@ %a@]"
          (print_list pr_typename (fun ppf -> fprintf ppf ",@ "))
          ext.oext_type_params
          fstring ext.oext_type_name
  in
  fprintf ppf "@[<hv 2>type %t +=%a@;<1 2>%a@]"
    print_extended_type
    (prf print_private) ext.oext_private
    (prf print_out_constr)
    (ext_to_constr foc ext.oext_name ext.oext_args ext.oext_ret_type)

and print_out_type_extension ppf te =
  let print_extended_type ppf =
    match te.otyext_params with
      [] -> fprintf ppf "%a" fstring te.otyext_name
    | [param] ->
      fprintf ppf "@[%a@ %a@]"
        pr_typename param
        fstring te.otyext_name
    | _ ->
        fprintf ppf "@[(@[%a)@]@ %a@]"
          (print_list pr_typename (fun ppf -> fprintf ppf ",@ "))
          te.otyext_params
          fstring te.otyext_name
  in
  fprintf ppf "@[<hv 2>type %t +=%a@;<1 2>%a@]"
    print_extended_type
    (prf print_private) te.otyext_private
    (print_list (prf print_out_constr) (fun ppf -> fprintf ppf "@ | "))
    te.otyext_constructors

let _ = out_module_type := print_out_module_type
let _ = out_signature :=
    (fun ppf x -> print_out_signature ppf (List.map (fun x -> H.Item(H.Off,x)) x))
let _ = out_sig_item := print_out_sig_item
let _ = out_type_extension := print_out_type_extension

(* Phrases *)

let print_out_exception ppf exn outv =
  match exn with
    Sys.Break -> fprintf ppf "Interrupted.@."
  | Out_of_memory -> fprintf ppf "Out of memory during evaluation.@."
  | Stack_overflow ->
      fprintf ppf "Stack overflow during evaluation (looping recursion?).@."
  | _ -> fprintf ppf "@[Exception:@ %a.@]@." !out_value outv

let rec print_items ppf =
  function
    [] -> ()
  | (H.Item(foc, Osig_typext(ext, H.Item(_,Oext_first))), None) :: items ->
      let exts, items = gather_extensions foc ext fst items in
      let te =
        { otyext_name = ext.oext_type_name;
          otyext_params = ext.oext_type_params;
          otyext_constructors = exts;
          otyext_private = ext.oext_private }
      in
        fprintf ppf "@[%a@]" !out_type_extension te;
        if items <> [] then fprintf ppf "@ %a" print_items items
  | (tree, valopt) :: items ->
      begin match valopt with
        Some v ->
          fprintf ppf "@[<2>%a =@ %a@]" (prf !out_sig_item) tree
            !out_value v
      | None -> fprintf ppf "@[%a@]" (prf !out_sig_item) tree
      end;
      if items <> [] then fprintf ppf "@ %a" print_items items

let print_out_phrase ppf =
  function
    Ophr_eval (outv, ty) ->
      fprintf ppf "@[- : %a@ =@ %a@]@." !out_type ty !out_value outv
  | Ophr_signature [] -> ()
  | Ophr_signature items ->
      fprintf ppf "@[<v>%a@]@." print_items
        (List.map (fun (x,y) -> H.Item(H.Off,x),y ) items )
  | Ophr_exception (exn, outv) -> print_out_exception ppf exn outv

let out_phrase = ref print_out_phrase
let ellipsis = ref_ellipsis
