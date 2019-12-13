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

(* Inclusion checks for the module language *)

open Misc
open Typedtree
open Types

type symptom =
    Missing_field of Ident.t * Location.t * string (* kind *)
  | Value_descriptions of Ident.t * value_description * value_description
  | Type_declarations of Ident.t * type_declaration
        * type_declaration * Includecore.type_mismatch
  | Extension_constructors of Ident.t * extension_constructor
        * extension_constructor * Includecore.extension_constructor_mismatch
  | Module_types of module_type * module_type
  | Modtype_infos of Ident.t * modtype_declaration * modtype_declaration
  | Modtype_permutation of Types.module_type * Typedtree.module_coercion
  | Interface_mismatch of string * string
  | Class_type_declarations of
      Ident.t * class_type_declaration * class_type_declaration *
      Ctype.class_match_failure list
  | Class_declarations of
      Ident.t * class_declaration * class_declaration *
      Ctype.class_match_failure list
  | Unbound_modtype_path of Path.t
  | Unbound_module_path of Path.t
  | Invalid_module_alias of Path.t

type pos =
  | Module of Ident.t
  | Modtype of Ident.t
  | Arg of functor_parameter
  | Body of functor_parameter


module E = struct
type ('a,'b) diff = {got:'a; expected:'a; symptom:'b}
type 'a core_diff =('a,unit) diff
let diff x y s = {got=x;expected=y; symptom=s}
let sdiff x y = {got=x; expected=y; symptom=()}

type core_sigitem_symptom =
  | Value_descriptions of value_description core_diff
  | Type_declarations of (type_declaration, Includecore.type_mismatch) diff
  | Extension_constructors of
      (extension_constructor, Includecore.extension_constructor_mismatch) diff
  | Class_type_declarations of
      (class_type_declaration, Ctype.class_match_failure list) diff
  | Class_declarations of
      (class_declaration, Ctype.class_match_failure list) diff

type core_module_type_symptom =
  | Not_a_signature
  | Not_an_alias
  | Not_an_identifier
  | Incompatible_aliases
  | Unbound_modtype_path of Path.t
  | Unbound_module_path of Path.t
  | Invalid_module_alias of Path.t

type module_type_symptom =
  | Mt_core of core_module_type_symptom
  | Signature of signature_symptom
  | Functor of functor_syndrom

and module_type_diff = (module_type, module_type_symptom) diff

and functor_syndrom =
  | Params of functor_params_diff
  | Result of module_type_diff

and functor_param_syndrom =
  | Incompatible_params of functor_parameter * functor_parameter
  | Mismatch of Ident.t option * Ident.t option * module_type_diff

and functor_params_diff = (functor_parameter list) core_diff

and signature_symptom = {
  env: Env.t;
  missings: signature_item list;
  incompatibles: (Ident.t * sigitem_symptom) list;
  oks: (int * module_coercion) list;
}
and sigitem_symptom =
  | Core of core_sigitem_symptom
  | Module_type_declaration of
      (modtype_declaration, module_type_declaration_symptom) diff
  | Module_type of module_type_diff

and module_type_declaration_symptom =
  | Illegal_permutation of Typedtree.module_coercion
  | Not_greater_than of module_type_diff
  | Not_less_than of module_type_diff
  | Incomparable of
      {less_than:module_type_diff; greater_than: module_type_diff}


type all =
  | In_Compilation_unit of (string, signature_symptom) diff
  | In_Signature of signature_symptom
  | In_Module_type of module_type_diff
  | In_Type_declaration of Ident.t * core_sigitem_symptom
  | In_Expansion of core_module_type_symptom


end

type error = pos list * Env.t * symptom

type mark =
  | Mark_both
  | Mark_positive
  | Mark_negative
  | Mark_neither

let negate_mark = function
  | Mark_both -> Mark_both
  | Mark_positive -> Mark_negative
  | Mark_negative -> Mark_positive
  | Mark_neither -> Mark_neither

let mark_positive = function
  | Mark_both | Mark_positive -> true
  | Mark_negative | Mark_neither -> false

(* All functions "blah env x1 x2" check that x1 is included in x2,
   i.e. that x1 is the type of an implementation that fulfills the
   specification x2. If not, Error is raised with a backtrace of the error. *)

(* Inclusion between value descriptions *)

let value_descriptions ~loc env ~mark subst id vd1 vd2 =
  Cmt_format.record_value_dependency vd1 vd2;
  if mark_positive mark then
    Env.mark_value_used (Ident.name id) vd1;
  let vd2 = Subst.value_description subst vd2 in
  try
    Ok (Includecore.value_descriptions ~loc env (Ident.name id) vd1 vd2)
  with Includecore.Dont_match ->
    Error E.(Core (Value_descriptions (sdiff vd1 vd2)))

(* Inclusion between type declarations *)

let type_declarations ~loc env ~mark ?old_env:_ subst id decl1 decl2 =
  let mark = mark_positive mark in
  if mark then
    Env.mark_type_used (Ident.name id) decl1;
  let decl2 = Subst.type_declaration subst decl2 in
  match
    Includecore.type_declarations ~loc env ~mark
      (Ident.name id) decl1 (Path.Pident id) decl2
  with
  | None -> Ok ()
  | Some err ->
      Error E.(Core(Type_declarations (diff decl1 decl2 err)))

(* Inclusion between extension constructors *)

let extension_constructors ~loc env ~mark  subst id ext1 ext2 =
  let mark = mark_positive mark in
  let ext2 = Subst.extension_constructor subst ext2 in
  match Includecore.extension_constructors ~loc env ~mark id ext1 ext2 with
  | None -> Ok ()
  | Some err ->
      Error E.(Core(Extension_constructors(diff ext1 ext2 err)))

(* Inclusion between class declarations *)

let class_type_declarations ~loc ~old_env:_ env  subst _id decl1 decl2 =
  let decl2 = Subst.cltype_declaration subst decl2 in
  match Includeclass.class_type_declarations ~loc env decl1 decl2 with
    []     -> Ok ()
  | reason ->
      Error E.(Core(Class_type_declarations(diff decl1 decl2 reason)))

let class_declarations ~old_env:_ env  subst _id decl1 decl2 =
  let decl2 = Subst.class_declaration subst decl2 in
  match Includeclass.class_declarations env decl1 decl2 with
    []     -> Ok ()
  | reason ->
     Error E.(Core(Class_declarations(diff decl1 decl2 reason)))

(* Expand a module type identifier when possible *)

let may_expand_module_path env path =
  try ignore (Env.find_modtype_expansion path env); true
  with Not_found -> false

let expand_module_path env path =
  try
    Ok (Env.find_modtype_expansion path env)
  with Not_found ->
    Error (E.Unbound_modtype_path path)

let expand_module_alias env path =
  try Ok ((Env.find_module path env).md_type)
  with Not_found ->
    Error (E.Unbound_module_path path)

(*
let rec normalize_module_path env  path =
  match expand_module_alias env  path with
    Mty_alias path' -> normalize_module_path env  path'
  | _ -> path
*)

(* Extract name, kind and ident from a signature item *)

type field_kind =
  | Field_value
  | Field_type
  | Field_exception
  | Field_typext
  | Field_module
  | Field_modtype
  | Field_class
  | Field_classtype



type field_desc = { name: string; kind: field_kind }

let kind_of_field_desc fd = match fd.kind with
  | Field_value -> "value"
  | Field_type -> "type"
  | Field_exception -> "exception"
  | Field_typext -> "extension constructor"
  | Field_module -> "module"
  | Field_modtype -> "module type"
  | Field_class -> "class"
  | Field_classtype -> "class type"

let field_desc kind id = { kind; name = Ident.name id }

(** Map indexed by both field types and names.
    This avoids name clashes between different sorts of fields
    such as values and types. *)
module FieldMap = Map.Make(struct
    type t = field_desc
    let compare = Stdlib.compare
  end)

let item_ident_name = function
    Sig_value(id, d, _) -> (id, d.val_loc, field_desc Field_value id)
  | Sig_type(id, d, _, _) -> (id, d.type_loc, field_desc Field_type  id )
  | Sig_typext(id, d, _, _) ->
      let kind =
        if Path.same d.ext_type_path Predef.path_exn
        then Field_exception
        else Field_typext
      in
      (id, d.ext_loc, field_desc kind id)
  | Sig_module(id, _, d, _, _) -> (id, d.md_loc, field_desc Field_module id)
  | Sig_modtype(id, d, _) -> (id, d.mtd_loc, field_desc Field_modtype id)
  | Sig_class(id, d, _, _) -> (id, d.cty_loc, field_desc Field_class id)
  | Sig_class_type(id, d, _, _) ->
      (id, d.clty_loc, field_desc Field_classtype id)

let is_runtime_component = function
  | Sig_value(_,{val_kind = Val_prim _}, _)
  | Sig_type(_,_,_,_)
  | Sig_module(_,Mp_absent,_,_,_)
  | Sig_modtype(_,_,_)
  | Sig_class_type(_,_,_,_) -> false
  | Sig_value(_,_,_)
  | Sig_typext(_,_,_,_)
  | Sig_module(_,Mp_present,_,_,_)
  | Sig_class(_,_,_,_) -> true

(* Print a coercion *)

let rec print_list pr ppf = function
    [] -> ()
  | [a] -> pr ppf a
  | a :: l -> pr ppf a; Format.fprintf ppf ";@ "; print_list pr ppf l
let print_list pr ppf l =
  Format.fprintf ppf "[@[%a@]]" (print_list pr) l

let rec print_coercion ppf c =
  let pr fmt = Format.fprintf ppf fmt in
  match c with
    Tcoerce_none -> pr "id"
  | Tcoerce_structure (fl, nl) ->
      pr "@[<2>struct@ %a@ %a@]"
        (print_list print_coercion2) fl
        (print_list print_coercion3) nl
  | Tcoerce_functor (inp, out) ->
      pr "@[<2>functor@ (%a)@ (%a)@]"
        print_coercion inp
        print_coercion out
  | Tcoerce_primitive {pc_desc; pc_env = _; pc_type}  ->
      pr "prim %s@ (%a)" pc_desc.Primitive.prim_name
        Printtyp.raw_type_expr pc_type
  | Tcoerce_alias (_, p, c) ->
      pr "@[<2>alias %a@ (%a)@]"
        Printtyp.path p
        print_coercion c
and print_coercion2 ppf (n, c) =
  Format.fprintf ppf "@[%d,@ %a@]" n print_coercion c
and print_coercion3 ppf (i, n, c) =
  Format.fprintf ppf "@[%s, %d,@ %a@]"
    (Ident.unique_name i) n print_coercion c

(* Simplify a structure coercion *)

let simplify_structure_coercion cc id_pos_list =
  let rec is_identity_coercion pos = function
  | [] ->
      true
  | (n, c) :: rem ->
      n = pos && c = Tcoerce_none && is_identity_coercion (pos + 1) rem in
  if is_identity_coercion 0 cc
  then Tcoerce_none
  else Tcoerce_structure (cc, id_pos_list)

let rec retrieve_functor_params env = function
  | Mty_ident p ->
      begin match expand_module_path env p with
      | Ok mty -> retrieve_functor_params env mty
      | Error _ -> []
      end
  | Mty_alias p ->
      begin match expand_module_alias env p with
      | Ok mty ->  retrieve_functor_params env mty
      | Error _ -> []
      end
  | Mty_functor (p, res) -> p :: retrieve_functor_params env res
  | Mty_signature _ -> []

(* Inclusion between module types.
   Return the restriction that transforms a value of the smaller type
   into a value of the bigger type. *)

let record_error (id:Ident.t) x (oks,errors) = match x with
  | Ok _ -> oks, errors
  | Error y -> oks , (id,y) :: errors

let result_cons pos (id:Ident.t) x (oks,errors) = match x with
  | Ok x -> (pos,x) :: oks, errors
  | Error y -> oks , (id,y) :: errors

let result_uncoerced_cons pos (id:Ident.t) x (oks,errors) = match x with
  | Ok _ -> (pos,Tcoerce_none) :: oks, errors
  | Error y -> oks , (id,y) :: errors

let rec modtypes ~loc env ~mark subst mty1 mty2 =
  let dont_match reason =
(*      match mty1, mty2 with
        Mty_alias _, _
      | _, Mty_alias _ -> err
      | _ -> *)
    let mty2 = Subst.modtype Make_local subst mty2 in
    Error E.(diff mty1 mty2 reason) in
  try_modtypes ~loc env ~mark dont_match subst mty1 mty2

and try_modtypes ~loc env ~mark dont_match subst mty1 mty2 =
 match mty1, mty2 with
  | Mty_alias p1, Mty_alias p2 ->
      if Env.is_functor_arg p2 env then
        Error E.(diff mty1 mty2 @@ Mt_core(Invalid_module_alias p2))
      else if not (Path.same p1 p2) then begin
        let p1 = Env.normalize_module_path None env p1
        and p2 = Env.normalize_module_path None env
            (Subst.module_path subst p2)
        in
        if not (Path.same p1 p2) then
          dont_match E.(Mt_core Incompatible_aliases)
        else Ok Tcoerce_none
      end
      else
        Ok Tcoerce_none
  | (Mty_alias p1, _) -> begin
      match
        Env.normalize_module_path (Some Location.none) env p1
      with
      | exception Env.Error (Env.Missing_module (_, _, path)) ->
        dont_match E.(Mt_core(Unbound_module_path path))
      | p1 ->
          begin match expand_module_alias env p1 with
          | Ok m ->
              let mty1 =
                Mtype.strengthen ~aliasable:true env m p1
              in
              modtypes ~loc env ~mark subst mty1 mty2
          | Error e -> dont_match (E.Mt_core e)
          end
    end
  | (Mty_ident p1, _) when may_expand_module_path env p1 ->
      begin match expand_module_path env p1 with
      | Ok m ->
          try_modtypes ~loc env ~mark dont_match subst m mty2
      | Error e -> dont_match (E.Mt_core e)
      end
  | (_, Mty_ident _) ->
      try_modtypes2 ~loc env ~mark dont_match mty1
        (Subst.modtype Keep subst mty2)
  | (Mty_signature sig1, Mty_signature sig2) ->
      begin match signatures ~loc env ~mark subst sig1 sig2 with
      | Ok _ as ok -> ok
      | Error e -> dont_match (E.Signature e)
      end
  | Mty_functor (param1, res1), Mty_functor (param2, res2) ->
      let cc_arg, env, subst =
        functor_param ~loc env ~mark:(negate_mark mark) subst param1 param2
      in
      let cc_res = modtypes ~loc env ~mark subst res1 res2 in
      begin match cc_arg, cc_res with
      | Ok Tcoerce_none, Ok Tcoerce_none -> Ok Tcoerce_none
      | Ok cc_arg, Ok cc_res -> Ok (Tcoerce_functor(cc_arg, cc_res))
      | _, Error {E.symptom = E.Functor E.Params res; _} ->
          let d = E.sdiff (param1::res.got) (param2::res.expected) in
          dont_match E.(Functor (Params d))
      | Error _, _ ->
          let params1 = retrieve_functor_params env res1 in
          let params2 = retrieve_functor_params env res2 in
          let d = E.sdiff (param1::params1) (param2::params2) in
          dont_match E.(Functor (Params d))
      | Ok _, Error res ->
          dont_match E.(Functor (Result res))
      end
  | Mty_functor _, _
  | _, Mty_functor _ ->
      let params1 = retrieve_functor_params env mty1 in
      let params2 = retrieve_functor_params env mty2 in
      let d = E.sdiff params1 params2 in
      dont_match E.(Functor (Params d))
  | _, Mty_signature _ ->
      dont_match (E.Mt_core E.Not_a_signature)
  | _, Mty_alias _ ->
      dont_match (E.Mt_core E.Not_an_alias)

and try_modtypes2 ~loc env ~mark dont_match mty1 mty2 =
  (* mty2 is an identifier *)
  match mty1, mty2 with
    | Mty_ident p1, Mty_ident p2
      when Path.same (Env.normalize_path_prefix None env p1)
          (Env.normalize_path_prefix None env p2) ->
          Ok Tcoerce_none
  | _, Mty_ident p2 ->
      if may_expand_module_path env p2 then
        match expand_module_path env p2 with
        | Ok mty2 ->
            try_modtypes ~loc env ~mark dont_match Subst.identity
              mty1 mty2
        | Error e -> dont_match (E.Mt_core e)
      else
        begin match mty1 with
        | Mty_functor _ ->
            let params1 = retrieve_functor_params env mty1 in
            let d = E.sdiff params1 [] in
            dont_match E.(Functor (Params d))
        | _ ->
            dont_match E.(Mt_core Not_an_identifier)
        end
  | _ -> assert false

(* Functor parameters *)

and functor_param ~loc env ~mark subst param1 param2 = match param1, param2 with
  | Unit, Unit ->
      Ok Tcoerce_none, env, subst
  | Named (name1, arg1), Named (name2, arg2) ->
      let arg2' = Subst.modtype Keep subst arg2 in
      let cc_arg =
        match modtypes ~loc env ~mark Subst.identity arg2' arg1 with
        | Ok cc -> Ok cc
        | Error err -> Error (E.Mismatch (name1, name2, err))
      in
      let env, subst =
        match name1, name2 with
        | Some p1, Some p2 ->
            Env.add_module p1 Mp_present arg2' env,
            Subst.add_module p2 (Path.Pident p1) subst
        | None, Some p2 ->
            Env.add_module p2 Mp_present arg2' env, subst
        | Some p1, None ->
            Env.add_module p1 Mp_present arg2' env, subst
        | None, None ->
            env, subst
      in
      cc_arg, env, subst
  | _, _ ->
      Error (E.Incompatible_params (param1, param2)), env, subst

(* Inclusion between signatures *)

and signatures ~loc env ~mark subst sig1 sig2 =
  (* Environment used to check inclusion of components *)
  let new_env =
    Env.add_signature sig1 (Env.in_signature true env) in
  (* Keep ids for module aliases *)
  let (id_pos_list,_) =
    List.fold_left
      (fun (l,pos) -> function
          Sig_module (id, Mp_present, _, _, _) ->
            ((id,pos,Tcoerce_none)::l , pos+1)
        | item -> (l, if is_runtime_component item then pos+1 else pos))
      ([], 0) sig1 in
  (* Build a table of the components of sig1, along with their positions.
     The table is indexed by kind and name of component *)
  let rec build_component_table pos tbl = function
      [] -> pos, tbl
    | (Sig_value (_, _, Hidden)
      |Sig_type (_, _, _, Hidden)
      |Sig_typext (_, _, _, Hidden)
      |Sig_module (_, _, _, _, Hidden)
      |Sig_modtype (_, _, Hidden)
      |Sig_class (_, _, _, Hidden)
      |Sig_class_type (_, _, _, Hidden)
      ) as item :: rem ->
        let pos = if is_runtime_component item then pos + 1 else pos in
        build_component_table pos tbl rem (* do not pair private items. *)
    | item :: rem ->
        let (id, _loc, name) = item_ident_name item in
        let pos, nextpos =
          if is_runtime_component item then pos, pos + 1
          else -1, pos
        in
        build_component_table nextpos
                              (FieldMap.add name (id, item, pos) tbl) rem in
  let len1, comps1 =
    build_component_table 0 FieldMap.empty sig1 in
  let len2 =
    List.fold_left
      (fun n i -> if is_runtime_component i then n + 1 else n)
      0
      sig2
  in
  (* Pair each component of sig2 with a component of sig1,
     identifying the names along the way.
     Return a coercion list indicating, for all run-time components
     of sig2, the position of the matching run-time components of sig1
     and the coercion to be applied to it. *)
  let rec pair_components subst paired unpaired = function
      [] ->
        let oks, errors =
          signature_components ~loc env ~mark new_env subst (List.rev paired) in
        begin match unpaired, errors, oks with
            | [], [], cc ->
                if len1 = len2 then (* see PR#5098 *)
                  Ok (simplify_structure_coercion cc id_pos_list)
                else
                  Ok (Tcoerce_structure (cc, id_pos_list))
            | missings, incompatibles, cc ->
                Error { env=new_env; E.missings; incompatibles; oks=cc }
        end
    | item2 :: rem ->
        let (id2, _loc, name2) = item_ident_name item2 in
        let name2, report =
          match item2, name2 with
            Sig_type (_, {type_manifest=None}, _, _), {name=s; kind=Field_type}
            when Btype.is_row_name s ->
              (* Do not report in case of failure,
                 as the main type will generate an error *)
              { kind=Field_type; name=String.sub s 0 (String.length s - 4) },
              false
          | _ -> name2, true
        in
        begin try
          let (id1, item1, pos1) = FieldMap.find name2 comps1 in
          let new_subst =
            match item2 with
              Sig_type _ ->
                Subst.add_type id2 (Path.Pident id1) subst
            | Sig_module _ ->
                Subst.add_module id2 (Path.Pident id1) subst
            | Sig_modtype _ ->
                Subst.add_modtype id2 (Mty_ident (Path.Pident id1)) subst
            | Sig_value _ | Sig_typext _
            | Sig_class _ | Sig_class_type _ ->
                subst
          in
          pair_components new_subst
            ((item1, item2, pos1) :: paired) unpaired rem
        with Not_found ->
          let unpaired =
            if report then
              item2 :: unpaired
            else unpaired in
          pair_components subst paired unpaired rem
        end in
  (* Do the pairing and checking, and return the final coercion *)
  pair_components subst [] [] sig2

(* Inclusion between signature components *)

and signature_components ~loc old_env ~mark env subst paired =
  let comps_rec rem =
    signature_components ~loc old_env ~mark env subst rem
  in
  match paired with
    [] -> [], []
  | (Sig_value(id1, valdecl1, _) , Sig_value(_id2, valdecl2, _), pos) :: rem ->
      let cc =
        value_descriptions ~loc env ~mark subst id1 valdecl1 valdecl2
      in
      begin match valdecl2.val_kind with
        Val_prim _ -> record_error id1 cc (comps_rec rem)
      | _ -> result_cons pos id1 cc (comps_rec rem)
      end
  | (Sig_type(id1, tydecl1, _, _), Sig_type(_id2, tydecl2, _, _), _pos) :: rem
    ->
      record_error id1
        (type_declarations ~loc ~old_env env ~mark subst id1 tydecl1 tydecl2)
      (comps_rec rem)
  | (Sig_typext(id1, ext1, _, _), Sig_typext(_id2, ext2, _, _), pos)
    :: rem ->
      result_uncoerced_cons pos id1
        (extension_constructors ~loc env ~mark  subst id1 ext1 ext2)
        (comps_rec rem)
  | (Sig_module(id1, pres1, mty1, _, _),
     Sig_module(_id2, pres2, mty2, _, _), pos) :: rem -> begin
      let cc = module_declarations ~loc env ~mark  subst id1 mty1 mty2 in
      let oks, errors = comps_rec rem in
      match cc with
      | Ok cc ->
          begin
            match pres1, pres2, mty1.md_type with
            | Mp_present, Mp_present, _ -> (pos, cc) :: oks, errors
            | _, Mp_absent, _ -> oks, errors
            | Mp_absent, Mp_present, Mty_alias p1 ->
                (pos, Tcoerce_alias (env, p1, cc)) :: oks, errors
            | Mp_absent, Mp_present, _ -> assert false
          end
      | Error diff -> oks, (id1, E.Module_type diff) :: errors
    end
  | (Sig_modtype(id1, info1, _), Sig_modtype(_id2, info2, _), _pos) :: rem ->
      record_error id1 (modtype_infos ~loc env ~mark  subst id1 info1 info2)
      (comps_rec rem)
  | (Sig_class(id1, decl1, _, _), Sig_class(_id2, decl2, _, _), pos) :: rem ->
      let item = class_declarations ~old_env env  subst id1 decl1 decl2 in
      result_uncoerced_cons pos id1 item (comps_rec rem)
  | (Sig_class_type(id1, info1, _, _),
     Sig_class_type(_id2, info2, _, _), _pos) :: rem ->
      let item =
        class_type_declarations ~loc ~old_env env subst id1 info1 info2 in
      record_error id1 item (comps_rec rem)
  | _ ->
      assert false

and module_declarations ~loc env ~mark  subst id1 md1 md2 =
  Builtin_attributes.check_alerts_inclusion
    ~def:md1.md_loc
    ~use:md2.md_loc
    loc
    md1.md_attributes md2.md_attributes
    (Ident.name id1);
  let p1 = Path.Pident id1 in
  if mark_positive mark then
    Env.mark_module_used (Ident.name id1) md1.md_loc;
  modtypes ~loc env ~mark subst
    (Mtype.strengthen ~aliasable:true env md1.md_type p1) md2.md_type

(* Inclusion between module type specifications *)

and modtype_infos ~loc env ~mark subst id info1 info2 =
  Builtin_attributes.check_alerts_inclusion
    ~def:info1.mtd_loc
    ~use:info2.mtd_loc
    loc
    info1.mtd_attributes info2.mtd_attributes
    (Ident.name id);
  let info2 = Subst.modtype_declaration Keep subst info2 in
  let r =
    match (info1.mtd_type, info2.mtd_type) with
      (None, None) -> Ok ()
    | (Some _, None) -> Ok ()
    | (Some mty1, Some mty2) ->
        check_modtype_equiv ~loc env ~mark mty1 mty2
    | (None, Some mty2) ->
        check_modtype_equiv ~loc env ~mark (Mty_ident(Path.Pident id)) mty2 in
  match r with
  | Ok _ as ok -> ok
  | Error e -> Error E.(Module_type_declaration (diff info1 info2 e))

and check_modtype_equiv ~loc env ~mark mty1 mty2 =
  match
    (modtypes ~loc env ~mark Subst.identity mty1 mty2,
     modtypes ~loc env ~mark:(negate_mark mark) Subst.identity mty2 mty1)
  with
    (Ok Tcoerce_none, Ok Tcoerce_none) -> Ok ()
  | (Ok c1, Ok _c2) ->
      (* Format.eprintf "@[c1 = %a@ c2 = %a@]@."
        print_coercion _c1 print_coercion _c2; *)
      Error E.(Illegal_permutation c1)
  | Ok _, Error e -> Error E.(Not_greater_than e)
  | Error e, Ok _ -> Error E.(Not_less_than e)
  | Error less_than, Error greater_than ->
      Error E.(Incomparable {less_than; greater_than})


(* Simplified inclusion check between module types (for Env) *)

let can_alias env path =
  let rec no_apply = function
    | Path.Pident _ -> true
    | Path.Pdot(p, _) -> no_apply p
    | Path.Papply _ -> false
  in
  no_apply path && not (Env.is_functor_arg path env)



type explanation = Env.t * E.all
exception Error of explanation
exception Apply_error of
    Location.t * Env.t * Longident.t * Path.t * Longident.t list

let check_modtype_inclusion ~loc env mty1 path1 mty2 =
  let aliasable = can_alias env path1 in
  modtypes ~loc env ~mark:Mark_both Subst.identity
    (Mtype.strengthen ~aliasable env mty1 path1) mty2

let () =
  Env.check_functor_application :=
    (fun ~errors ~loc (lid0, path_f, args) env mty1 path1 mty2 ->
       match
         check_modtype_inclusion ~loc env mty1 path1 mty2
       with Error _errs ->
         if errors then
           raise (Apply_error(loc, env, lid0, path_f, args))
         else
           raise Not_found
          | Ok _ -> ()
    )

let check_modtype_inclusion ~loc env mty1 path1 mty2 =
  match check_modtype_inclusion ~loc env mty1 path1 mty2 with
  | Ok _ -> None
  | Error e -> Some (env, E.In_Module_type e)

(* Check that an implementation of a compilation unit meets its
   interface. *)

let compunit env ?(mark=Mark_both) impl_name impl_sig intf_name intf_sig =
  match
    signatures ~loc:(Location.in_file impl_name) env ~mark Subst.identity
      impl_sig intf_sig
  with Result.Error reasons ->
    let cdiff = E.In_Compilation_unit(E.diff impl_name intf_name reasons) in
    raise(Error(env, cdiff))
  | Ok x -> x


let drop_inserted_suffix patch =
  let rec drop = function
    | Diff.Insert _ :: q -> drop q
    | rest -> List.rev rest in
  drop (List.rev patch)

let rec pp_list_diff f g side ppf patch = match side, patch with
  | _, [] -> ()
  | `Left, Diff.Insert _ :: t
  | `Right, Diff.Delete _ :: t ->
      pp_list_diff f g side ppf t
  | `Left, Diff.Delete c :: t ->
      Format.fprintf ppf "@{<error>%a@}@ " f c ;
      pp_list_diff f g side ppf t
  | `Right, Diff.Insert c :: t ->
      Format.fprintf ppf "@{<error>%a@}@ " g c ;
      pp_list_diff f g side ppf t
  | `Left, Diff.Keep (c,_,_) :: t ->
      Format.fprintf ppf "%a@ " f c;
      pp_list_diff f g side ppf t
  | `Right, Diff.Keep (_,c,_) :: t ->
      Format.fprintf ppf "%a@ " g c;
      pp_list_diff f g side ppf t
  | `Left, Diff.Change (c,_,_) :: t ->
      Format.fprintf ppf "@{<warning>%a@}@ " f c ;
      pp_list_diff f g side ppf t
  | `Right, Diff.Change (_,c,_) :: t ->
      Format.fprintf ppf "@{<warning>%a@}@ " g c ;
      pp_list_diff f g side ppf t
let pp_list_diff_without_suffix f g side ppf patch =
  pp_list_diff f g side ppf (drop_inserted_suffix patch)

module FunctorArgsDiff = struct
  open Diff

  let cutoff = 100
  let weight = function
    | Insert _ -> 10
    | Delete _ -> 10
    | Change _ -> 10
    | Keep (param1, param2, _) -> begin
        match param1, param2 with
        | Unit, Unit
        | Named (None, _), Named (None, _)
          -> 0
        | Named (Some n1, _), Named (Some n2, _)
          when String.equal (Ident.name n1) (Ident.name n2)
          -> 0
        | Named _, Named _ -> 1
        | Unit, Named _ | Named _, Unit -> assert false
      end

  let update d ((env, subst) as st) = match d with
    | Insert (Unit | Named (None,_))
    | Delete (Unit | Named (None,_))
    | Keep (Unit,_,_)
    | Keep (_,Unit,_)
    | Change (_,(Unit | Named (None,_)), _) ->
        st
    | Insert (Named (Some p, arg))
    | Delete (Named (Some p, arg))
    | Change (Unit, Named (Some p, arg), _) ->
        let arg' = Subst.modtype Keep subst arg in
        Env.add_module p Mp_present arg' env, subst
    | Keep (Named (name1, _), Named (name2, arg2), _)
    | Change (Named (name1, _), Named (name2, arg2), _) -> begin
        let arg2' = Subst.modtype Keep subst arg2 in
        match name1, name2 with
        | Some p1, Some p2 ->
            Env.add_module p1 Mp_present arg2' env,
            Subst.add_module p2 (Path.Pident p1) subst
        | None, Some p2 ->
            Env.add_module p2 Mp_present arg2' env, subst
        | Some p1, None ->
            Env.add_module p1 Mp_present arg2' env, subst
        | None, None ->
            env, subst
      end

  let diff env0 _ctxt l1 l2 =
    let test (env, subst) mty1 mty2 =
      let snap = Btype.snapshot () in
      let res, _, _ = functor_param
          ~loc:Location.none env ~mark:Mark_neither subst mty1 mty2
      in
      Btype.backtrack snap;
      res
    in
    let state0 = (env0, Subst.identity) in
    Diff.diff ~weight ~cutoff ~test ~update
      state0 (Array.of_list l1) (Array.of_list l2)
end

module FunctorAppDiff = struct
  open Diff

  let cutoff = 100
  let weight = function
    | Insert _ -> 10
    | Delete _ -> 10
    | Change _ -> 10
    | Keep _ -> 0

  let update d ((env, subst) as st) = match d with
    | Insert (Unit | Named (None,_))
    | Keep (_,Unit,_)
    | Change (_,(Unit | Named (None,_)), _)
        -> st
    | Delete _
        -> st
    | Insert (Named (Some p, arg)) ->
        let arg' = Subst.modtype Keep subst arg in
        Env.add_module p Mp_present arg' env, subst
    | Keep (lid1, Named (name2, _arg2), _)
    | Change (lid1, Named (name2, _arg2), _) -> begin
        let path1, _ = Env.find_module_by_name lid1 env in
        match name2 with
        | Some p2 ->
            env, Subst.add_module p2 path1 subst
        | None ->
            env, subst
      end

  let diff env0 args params =
    let loc = Location.none in
    let test (env, subst) lid1 param2 =
      let snap = Btype.snapshot () in
      let path1, md1 = Env.find_module_by_name lid1 env in
      let aliasable = can_alias env path1 in
      let mty1' = Mtype.strengthen ~aliasable env md1.md_type path1 in
      let res, _, _ =
        functor_param
          ~loc env ~mark:Mark_neither subst
          (Named (None, mty1')) param2
      in
      Btype.backtrack snap;
      res
    in
    let state0 = (env0, Subst.identity) in
    Diff.diff ~weight ~cutoff ~test ~update
      state0 (Array.of_list args) (Array.of_list params)

end



(* Hide the context and substitution parameters to the outside world *)

let modtypes ~loc env ?(mark=Mark_both) mty1 mty2 =
  match modtypes ~loc env ~mark Subst.identity mty1 mty2 with
  | Ok x -> x
  | Error reason -> raise (Error (env, E.(In_Module_type reason)))
let signatures env ?(mark=Mark_both) sig1 sig2 =
  match signatures ~loc:Location.none env ~mark Subst.identity sig1 sig2 with
  | Ok x -> x
  | Error reason -> raise (Error(env,E.(In_Signature reason)))

let type_declarations ~loc env ?(mark=Mark_both) id decl1 decl2 =
  match type_declarations ~loc env ~mark Subst.identity id decl1 decl2 with
  | Ok x -> x
  | Error (E.Core reason) ->
      raise (Error(env,E.(In_Type_declaration(id,reason))))
  | Error _ -> assert false


(*
let modtypes env m1 m2 =
  let c = modtypes env m1 m2 in
  Format.eprintf "@[<2>modtypes@ %a@ %a =@ %a@]@."
    Printtyp.modtype m1 Printtyp.modtype m2
    print_coercion c;
  c
*)

(* Error report *)

module Illegal_permutation = struct
  (** Extraction of information in case of illegal permutation
      in a module type *)

  (** When examining coercions, we only have runtime component indices,
      we use thus a limited version of {!pos}. *)
  type coerce_pos =
    | Item of int
    | InArg
    | InBody

  let either f x g y = match f x with
    | None -> g y
    | Some _ as v -> v

  (** We extract a lone transposition from a full tree of permutations. *)
  let rec transposition_under path = function
    | Tcoerce_structure(c,_) ->
        either
          (not_fixpoint path 0) c
          (first_non_id path 0) c
    | Tcoerce_functor(arg,res) ->
        either
          (transposition_under (InArg::path)) arg
          (transposition_under (InBody::path)) res
    | Tcoerce_none -> None
    | Tcoerce_alias _ | Tcoerce_primitive _ ->
        (* these coercions are not inversible, and raise an error earlier when
           checking for module type equivalence *)
        assert false
  (* we search the first point which is not invariant at the current level *)
  and not_fixpoint path pos = function
    | [] -> None
    | (n, _) :: q ->
        if n = pos then
          not_fixpoint path (pos+1) q
        else
          Some(List.rev path, pos, n)
  (* we search the first item with a non-identity inner coercion *)
  and first_non_id path pos = function
    | [] -> None
    | (_,Tcoerce_none) :: q -> first_non_id path (pos + 1) q
    | (_,c) :: q ->
        either
          (transposition_under (Item pos :: path)) c
          (first_non_id path (pos + 1)) q

  let transposition c =
    match transposition_under [] c with
    | None -> raise Not_found
    | Some x -> x

  let rec runtime_item k = function
    | [] -> raise Not_found
    | item :: q ->
        if not(is_runtime_component item) then
          runtime_item k q
        else if k = 0 then
          item
        else
          runtime_item (k-1) q

  (* Find module type at position [path] and convert the [coerce_pos] path to
     a [pos] path *)
  let rec find env ctx path mt = match mt, path with
    | (Mty_ident p | Mty_alias p), _ ->
        begin match (Env.find_modtype p env).mtd_type with
        | None -> raise Not_found
        | Some mt -> find env ctx path mt
        end
    | Mty_signature s , [] -> List.rev ctx, s
    | Mty_signature s, Item k :: q ->
        begin match runtime_item k s with
        | Sig_module (id, _, md,_,_) -> find env (Module id :: ctx) q md.md_type
        | _ -> raise Not_found
        end
    | Mty_functor(Named (_,mt) as arg,_), InArg :: q ->
        find env (Arg arg :: ctx) q mt
    | Mty_functor(arg, mt), InBody :: q ->
        find env (Body arg :: ctx) q mt
    | _ -> raise Not_found

  let find env path mt = find env [] path mt
  let item mt k = item_ident_name (runtime_item k mt)

  let pp_item ppf (id,_,kind) =
    Format.fprintf ppf "%s %S" (kind_of_field_desc kind) (Ident.name id)

  let pp ctx_printer env ppf (mty,c) =
    try
      let p, k, l = transposition c in
      let ctx, mt = find env p mty in
      Format.fprintf ppf
        "@[<hv 2>Illegal permutation of runtime components in a module type.@ \
         @[For example,@ %a@[the %a@ and the %a are not in the same order@ \
         in the expected and actual module types.@]@]"
        ctx_printer ctx pp_item (item mt k) pp_item (item mt l)
    with Not_found -> (* this should not happen *)
      Format.fprintf ppf
        "Illegal permutation of runtime components in a module type."

end



let show_loc msg ppf loc =
  let pos = loc.Location.loc_start in
  if List.mem pos.Lexing.pos_fname [""; "_none_"; "//toplevel//"] then ()
  else Format.fprintf ppf "@\n@[<2>%a:@ %s@]" Location.print_loc loc msg

let show_locs ppf (loc1, loc2) =
  show_loc "Expected declaration" ppf loc2;
  show_loc "Actual declaration" ppf loc1


open Format


let path_of_context = function
    Module id :: rem ->
      let rec subm path = function
        | [] -> path
        | Module id :: rem -> subm (Path.Pdot (path, Ident.name id)) rem
        | _ -> assert false
      in subm (Path.Pident id) rem
  | _ -> assert false


let rec context ppf = function
    Module id :: rem ->
      fprintf ppf "@[<2>module %a%a@]" Printtyp.ident id args rem
  | Modtype id :: rem ->
      fprintf ppf "@[<2>module type %a =@ %a@]"
        Printtyp.ident id context_mty rem
  | Body x :: rem ->
      fprintf ppf "functor (%s) ->@ %a" (argname x) context_mty rem
  | Arg x :: rem ->
      fprintf ppf "functor (%s : %a) -> ..." (argname x) context_mty rem
  | [] ->
      fprintf ppf "<here>"
and context_mty ppf = function
    (Module _ | Modtype _) :: _ as rem ->
      fprintf ppf "@[<2>sig@ %a@;<1 -2>end@]" context rem
  | cxt -> context ppf cxt
and args ppf = function
    Body x :: rem ->
      fprintf ppf "(%s)%a" (argname x) args rem
  | Arg x :: rem ->
      fprintf ppf "(%s :@ %a) : ..." (argname  x) context_mty rem
  | cxt ->
      fprintf ppf " :@ %a" context_mty cxt
and argname = function
  | Unit -> ""
  | Named (None, _) -> "_"
  | Named (Some id, _) -> Ident.name id

let alt_context ppf cxt =
  if cxt = [] then () else
  if List.for_all (function Module _ -> true | _ -> false) cxt then
    fprintf ppf "in module %a,@ " Printtyp.path (path_of_context cxt)
  else
    fprintf ppf "@[<hv 2>at position@ %a,@]@ " context cxt

let context ppf cxt =
  if cxt = [] then () else
  if List.for_all (function Module _ -> true | _ -> false) cxt then
    fprintf ppf "In module %a:@ " Printtyp.path (path_of_context cxt)
  else
    fprintf ppf "@[<hv 2>At position@ %a@]@ " context cxt



let buffer = ref Bytes.empty
let is_big obj =
  let size = !Clflags.error_size in
  size > 0 &&
  begin
    if Bytes.length !buffer < size then buffer := Bytes.create size;
    try ignore (Marshal.to_buffer !buffer 0 size obj []); false
    with _ -> true
  end

module Pp = struct
  open E
  let break ppf first =
    if not first then Format.pp_print_break ppf 1 0

  let core id ?(first=false) ppf x =
    break ppf first;
    match x with
    | Value_descriptions diff ->
        let t1 = Printtyp.tree_of_value_description id diff.got in
        let t2 = Printtyp.tree_of_value_description id diff.expected in
        Format.fprintf ppf
          "@[<hv 2>Values do not match:@ %a@;<1 -2>is not included in@ %a@]%a"
          !Oprint.out_sig_item t1
          !Oprint.out_sig_item t2
        show_locs (diff.got.val_loc, diff.expected.val_loc)
    | Type_declarations diff ->
        Format.fprintf ppf "@[<v>@[<hv>%s:@;<1 2>%a@ %s@;<1 2>%a@]%a%a@]"
          "Type declarations do not match"
          !Oprint.out_sig_item
          (Printtyp.tree_of_type_declaration id diff.got Trec_first)
          "is not included in"
          !Oprint.out_sig_item
          (Printtyp.tree_of_type_declaration id diff.expected Trec_first)
          (Includecore.report_type_mismatch
             "the first" "the second" "declaration") diff.symptom
          show_locs (diff.got.type_loc, diff.expected.type_loc)
    | Extension_constructors diff ->
        Format.fprintf ppf "@[<v>@[<hv>%s:@;<1 2>%a@ %s@;<1 2>%a@]@ %a%a@]"
          "Extension declarations do not match"
          !Oprint.out_sig_item
          (Printtyp.tree_of_extension_constructor id diff.got Text_first)
          "is not included in"
          !Oprint.out_sig_item
          (Printtyp.tree_of_extension_constructor id diff.expected Text_first)
          (Includecore.report_extension_constructor_mismatch
             "the first" "the second" "declaration") diff.symptom
          show_locs (diff.got.ext_loc, diff.expected.ext_loc)
    | Class_type_declarations diff ->
        Format.fprintf ppf
          "@[<hv 2>Class type declarations do not match:@ \
           %a@;<1 -2>does not match@ %a@]@ %a"
          !Oprint.out_sig_item
          (Printtyp.tree_of_cltype_declaration id diff.got Trec_first)
          !Oprint.out_sig_item
          (Printtyp.tree_of_cltype_declaration id diff.expected Trec_first)
          Includeclass.report_error diff.symptom
    | Class_declarations {got;expected;symptom} ->
        let t1 = Printtyp.tree_of_class_declaration id got Trec_first in
        let t2 = Printtyp.tree_of_class_declaration id expected Trec_first in
        Format.fprintf ppf
          "@[<hv 2>Class declarations do not match:@ \
           %a@;<1 -2>does not match@ %a@]@ %a"
          !Oprint.out_sig_item t1
          !Oprint.out_sig_item t2
          Includeclass.report_error symptom

  let missing_field ?(first=false) ppf item =
    let id, loc, kind = item_ident_name item in
    Format.fprintf ppf "%aThe %s `%a' is required but not provided%a"
      break first (kind_of_field_desc kind) Printtyp.ident id
    (show_loc "Expected declaration") loc

  let module_types ?(first=false) ppf {got=mty1; expected=mty2} =
    Format.fprintf ppf
      "%a@[<hv 2>Modules do not match:@ \
       %a@;<1 -2>is not included in@ %a@]"
      break first
      !Oprint.out_module_type (Printtyp.tree_of_modtype mty1)
      !Oprint.out_module_type (Printtyp.tree_of_modtype mty2)

  let eq_module_types ?(first=false) ppf {got=mty1; expected=mty2} =
    Format.fprintf ppf
      "%a@[<hv 2>Module types do not match:@ \
       %a@;<1 -2>is not equal to@ %a@]"
      break first
      !Oprint.out_module_type (Printtyp.tree_of_modtype mty1)
      !Oprint.out_module_type (Printtyp.tree_of_modtype mty2)


  let module_type_declarations id ?(first=false) ppf {got=d1 ; expected=d2} =
    Format.fprintf ppf
      "%a@[<hv 2>Module type declarations do not match:@ \
       %a@;<1 -2>does not match@ %a@]"
      break first
      !Oprint.out_sig_item (Printtyp.tree_of_modtype_declaration id d1)
      !Oprint.out_sig_item (Printtyp.tree_of_modtype_declaration id d2)

  let interface_mismatch ppf diff =
    Format.fprintf ppf
      "The implementation %s@ does not match the interface %s:@ "
      diff.got diff.expected

  let core_module_type_symptom ?(first=false) ppf x  =
    match x with
    | Not_a_signature
    | Not_an_alias
    | Not_an_identifier
    | Incompatible_aliases -> ()

    | Unbound_modtype_path path ->
        break ppf first;
        Format.fprintf ppf "Unbound module type %a" Printtyp.path path
    | Unbound_module_path path ->
        break ppf first;
        Format.fprintf ppf "Unbound module %a" Printtyp.path path
    | Invalid_module_alias path ->
        break ppf first;
        Format.fprintf ppf "Module %a cannot be aliased" Printtyp.path path

  (** Take a tree of difference and pick the simplest path to an error *)
  let with_context first ctx printer ppf diff =
        if ctx <> [] then break ppf first;
        Format.fprintf ppf "%a%a" context (List.rev ctx)
          (printer ?first:(Some(first || ctx<>[])))
          diff

  let with_context_and_elision first ctx printer ppf diff =
    if is_big (diff.got,diff.expected) then
      Format.fprintf ppf "..."
    else
      with_context first ctx printer ppf diff

  let rec module_type ?(first=false) ~eqmode env ctx ppf diff =
    match diff.symptom with
    | Mt_core (Invalid_module_alias _ as s) ->
        with_context first ctx core_module_type_symptom ppf s
    | _ ->
        let inner = if eqmode then eq_module_types else module_types in
        with_context_and_elision first ctx inner ppf diff
      ; module_type_symptom env ctx ppf diff.symptom

  and module_type_symptom env ctx ppf = function
    | Mt_core core -> core_module_type_symptom ppf core
    | Signature s -> signature env ctx ppf s
    | Functor f -> functor_symptom env ctx ppf f

  and functor_symptom env ctx ppf = function
    | Result res ->
        module_type ~eqmode:false env ctx ppf res
    | Params E.{got; expected; symptom=()} ->
        begin match FunctorArgsDiff.diff env ctx got expected with
        | None ->
            Format.fprintf ppf
              "@;@[<hv 2>Parameters do not match:@ \
               @[%a@]@;<0 -2>does not match@ @[%a@]@]"
              (Format.pp_print_list functor_param) got
              (Format.pp_print_list functor_param) expected
        | Some d ->
            Format.fprintf ppf
              "@;@[<hv 2>Parameters do not match:@ \
               @[%a@]@;<0 -2>does not match@ @[%a@]@]"
              (pp_list_diff functor_param functor_param `Left) d
              (pp_list_diff functor_param functor_param `Right) d
        end

  and functor_param ppf = function
    | Unit -> Format.fprintf ppf "()"
    | Named (None, mty) ->
      Format.fprintf ppf "%a"
        !Oprint.out_module_type (Printtyp.tree_of_modtype mty)
    | Named (Some p, mty) ->
      Format.fprintf ppf "(%s : %a)"
        (Ident.name p)
        !Oprint.out_module_type (Printtyp.tree_of_modtype mty)

  and signature ?(first=false) env ctx ppf sgs =
    Printtyp.wrap_printing_env ~error:true sgs.env (fun () ->
    match sgs.missings, sgs.incompatibles with
    | a :: _ , _ -> with_context first ctx missing_field ppf a
    | [], a :: _ -> sigitem ~first env ctx ppf a
    | [], [] -> assert false
      )
  and sigitem ~first env ctx ppf (name,s) = match s with
    | Core c -> with_context first ctx (core name) ppf c
    | Module_type diff ->
        module_type ~first ~eqmode:false env (Module name :: ctx) ppf diff
    | Module_type_declaration diff ->
        module_type_decl ~first env ctx name ppf diff
  and module_type_decl ?(first=false) env ctx id ppf diff =
    with_context_and_elision first ctx (module_type_declarations id) ppf diff;
    match diff.symptom with
    | Not_less_than mts ->
        Format.fprintf ppf
          "@ The first module type is not included in the second%a"
          (module_type ~eqmode:true env (Modtype id :: ctx)) mts
    | Not_greater_than mts ->
        Format.fprintf ppf
          "@ The second module type is not included in the first%a"
          (module_type ~eqmode:true env (Modtype id :: ctx)) mts
    | Incomparable mts ->
        (module_type ~eqmode:true env (Modtype id :: ctx)) ppf mts.less_than
    | Illegal_permutation c ->
        begin match diff.got.Types.mtd_type with
        | None -> assert false
        | Some mty ->
            with_context first (Modtype id::ctx)
              (fun ?(first=false) ->
                 break ppf first;
                 Illegal_permutation.pp alt_context env) ppf (mty,c)
        end


  let linear env ppf = function
    | In_Compilation_unit diff ->
      interface_mismatch ppf diff; signature env [] ppf diff.symptom
    | In_Type_declaration (id,reason) -> core ~first:true id ppf reason
    | In_Module_type diff ->
        module_type ~eqmode:false ~first:true env [] ppf diff
    | In_Signature diff -> signature ~first:true env [] ppf diff
    | In_Expansion cmts -> core_module_type_symptom ~first:true ppf cmts
end

let include_err ppf (env, err) =
  Printtyp.wrap_printing_env ~error:true env (fun () ->
    fprintf ppf "@[<v>%a@]" (Pp.linear env) err
    )


let report_error ppf error_tree =
  Printtyp.Conflicts.reset();
  fprintf ppf "@[<v>%a%t@]" include_err error_tree
    Printtyp.Conflicts.print_explanations

let report_apply_error env ppf (lid0, path_f, args) =
  let md_f = Env.find_module path_f env in
  let params = retrieve_functor_params env md_f.md_type in
  begin match FunctorAppDiff.diff env args params with
  | None ->
      Format.fprintf ppf
        "@;@[<hv 2>The functor application %a is ill-typed. These arguments:@ \
         @[%a@]@;<1 -2>do not match these parameters@ @[%a@]@]"
        Printtyp.longident lid0
        (Format.pp_print_list Printtyp.longident) args
        (Format.pp_print_list Pp.functor_param) params
  | Some d ->
      Format.fprintf ppf
        "@;@[<hv 2>The functor application %a is ill-typed. These arguments:@ \
         @[%a@]@;<1 -2>do not match these parameters@ @[%a@]@]"
        Printtyp.longident lid0
        (pp_list_diff Printtyp.longident Pp.functor_param `Left) d
        (pp_list_diff Printtyp.longident Pp.functor_param `Right) d
  end

(* We could do a better job to split the individual error items
   as sub-messages of the main interface mismatch on the whole unit. *)
let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error err)
      | Apply_error(loc, env, lid0, path_f, args) ->
          Some (Location.error_of_printer ~loc
                  (report_apply_error env) (lid0, path_f, args)
               )
      | _ -> None
    )


let expand_module_alias env path =
  match expand_module_alias env path with
  | Ok x -> x
  | Result.Error _ ->
      raise (Error(env,In_Expansion(E.Unbound_module_path path)))


(* Apply error diff *)
module FunctorAppDiffForTypeMod = struct
  open Diff

  let cutoff = 100
  let weight = function
    | Insert _ -> 10
    | Delete _ -> 10
    | Change _ -> 10
    | Keep _ -> 0

  let update d ((env, subst) as st) = match d with
    | Insert (Types.Unit | Types.Named (None,_))
    | Keep (_,Unit,_)
    | Change (_,(Unit | Named (None,_)), _)
        -> st
    | Delete _
        -> st
    | Insert (Types.Named (Some p, arg)) ->
        let arg' = Subst.modtype Keep subst arg in
        Env.add_module p Mp_present arg' env, subst
    | Keep _ | Change _ -> env, subst

  let diff env0 ~f ~args =
    let params = retrieve_functor_params env0 f.mod_type in
    let loc = Location.none in
    let test (env, subst) me param2 =
      let snap = Btype.snapshot () in
      let res, _, _ =
        functor_param
          ~loc env ~mark:Mark_neither subst
          (Named (None, me.mod_type )) param2
      in
      Btype.backtrack snap;
      res
    in
    let state0 = (env0, Subst.identity) in
    let patch =
      Diff.diff ~weight ~cutoff ~test ~update
        state0 (Array.of_list args) (Array.of_list params)
    in
    Option.to_result ~none:params patch

end

type functor_app_patch =
  (Typedtree.module_expr, Types.functor_parameter,
   Typedtree.module_coercion, E.functor_param_syndrom)
    Diff.patch
let functor_app_diff = FunctorAppDiffForTypeMod.diff
let pp_functor_app_patch =
  let pp_me ppf me = Printtyp.modtype ppf me.mod_type in
  pp_list_diff_without_suffix pp_me Pp.functor_param
