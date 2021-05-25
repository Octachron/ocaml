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

(* Inclusion checks for the core language *)

open Asttypes
open Path
open Types
open Typedtree

(* Inclusion between value descriptions *)

exception Dont_match

let value_descriptions ~loc env name
    (vd1 : Types.value_description)
    (vd2 : Types.value_description) =
  Builtin_attributes.check_alerts_inclusion
    ~def:vd1.val_loc
    ~use:vd2.val_loc
    loc
    vd1.val_attributes vd2.val_attributes
    name;
  if Ctype.moregeneral env true vd1.val_type vd2.val_type then begin
    match (vd1.val_kind, vd2.val_kind) with
        (Val_prim p1, Val_prim p2) ->
          if p1 = p2 then Tcoerce_none else raise Dont_match
      | (Val_prim p, _) ->
          let pc = {pc_desc = p; pc_type = vd2.Types.val_type;
                  pc_env = env; pc_loc = vd1.Types.val_loc; } in
          Tcoerce_primitive pc
      | (_, Val_prim _) -> raise Dont_match
      | (_, _) -> Tcoerce_none
  end else
    raise Dont_match

(* Inclusion between "private" annotations *)

let private_flags decl1 decl2 =
  match decl1.type_private, decl2.type_private with
  | Private, Public ->
      decl2.type_kind = Type_abstract &&
      (decl2.type_manifest = None || decl1.type_kind <> Type_abstract)
  | _, _ -> true

(* Inclusion between manifest types (particularly for private row types) *)

let is_absrow env ty =
  match ty.desc with
    Tconstr(Pident _, _, _) ->
      begin match Ctype.expand_head env ty with
        {desc=Tobject _|Tvariant _} -> true
      | _ -> false
      end
  | _ -> false

let type_manifest env ty1 params1 ty2 params2 priv2 =
  let ty1' = Ctype.expand_head env ty1 and ty2' = Ctype.expand_head env ty2 in
  match ty1'.desc, ty2'.desc with
    Tvariant row1, Tvariant row2 when is_absrow env (Btype.row_more row2) ->
      let row1 = Btype.row_repr row1 and row2 = Btype.row_repr row2 in
      Ctype.equal env true (ty1::params1) (row2.row_more::params2) &&
      begin match row1.row_more with
        {desc=Tvar _|Tconstr _|Tnil} -> true
      | _ -> false
      end &&
      let r1, r2, pairs =
        Ctype.merge_row_fields row1.row_fields row2.row_fields in
      (not row2.row_closed ||
       row1.row_closed && Ctype.filter_row_fields false r1 = []) &&
      List.for_all
        (fun (_,f) -> match Btype.row_field_repr f with
          Rabsent | Reither _ -> true | Rpresent _ -> false)
        r2 &&
      let to_equal = ref (List.combine params1 params2) in
      List.for_all
        (fun (_, f1, f2) ->
          match Btype.row_field_repr f1, Btype.row_field_repr f2 with
            Rpresent(Some t1),
            (Rpresent(Some t2) | Reither(false, [t2], _, _)) ->
              to_equal := (t1,t2) :: !to_equal; true
          | Rpresent None, (Rpresent None | Reither(true, [], _, _)) -> true
          | Reither(c1,tl1,_,_), Reither(c2,tl2,_,_)
            when List.length tl1 = List.length tl2 && c1 = c2 ->
              to_equal := List.combine tl1 tl2 @ !to_equal; true
          | Rabsent, (Reither _ | Rabsent) -> true
          | _ -> false)
        pairs &&
      let tl1, tl2 = List.split !to_equal in
      Ctype.equal env true tl1 tl2
  | Tobject (fi1, _), Tobject (fi2, _)
    when is_absrow env (snd(Ctype.flatten_fields fi2)) ->
      let (fields2,rest2) = Ctype.flatten_fields fi2 in
      Ctype.equal env true (ty1::params1) (rest2::params2) &&
      let (fields1,rest1) = Ctype.flatten_fields fi1 in
      (match rest1 with {desc=Tnil|Tvar _|Tconstr _} -> true | _ -> false) &&
      let pairs, _miss1, miss2 = Ctype.associate_fields fields1 fields2 in
      miss2 = [] &&
      let tl1, tl2 =
        List.split (List.map (fun (_,_,t1,_,t2) -> t1, t2) pairs) in
      Ctype.equal env true (params1 @ tl1) (params2 @ tl2)
  | _ ->
      let rec check_super ty1 =
        Ctype.equal env true (ty1 :: params1) (ty2 :: params2) ||
        priv2 = Private &&
        try check_super
              (Ctype.try_expand_once_opt env (Ctype.expand_head env ty1))
        with Ctype.Cannot_expand -> false
      in check_super ty1

(* Inclusion between type declarations *)

type position = Ctype.Unification_trace.position = First | Second

let choose ord first second =
  match ord with
  | First -> first
  | Second -> second

let choose_other ord first second =
  match ord with
  | First -> choose Second first second
  | Second -> choose First first second

type label_mismatch =
  | Type
  | Mutability of position

type ('a,'b) change =
  | Name_mismatch of {pos:int; got:string; expected:string }
  | Type_mismatch of {pos:int; got:'a; expected:'a; reason:'b}
  | Swap of { got: int * string; expected:int * string }
  | Displacement of {name:string; got:int; expected:int}
  | Insert of {pos:int; insert:'a}
  | Delete of {pos:int; delete:'a}

type record_change = (Types.label_declaration, label_mismatch) change

type record_mismatch =
  | Label_mismatch of record_change list
  | Unboxed_float_representation of position

type constructor_mismatch =
  | Type
  | Arity
  | Inline_record of record_change list
  | Kind of position
  | Explicit_return_type of position

type extension_constructor_mismatch =
  | Constructor_privacy
  | Constructor_mismatch of Ident.t
                            * Types.extension_constructor
                            * Types.extension_constructor
                            * constructor_mismatch

type variant_change =
  (Types.constructor_declaration, constructor_mismatch) change


type type_mismatch =
  | Arity
  | Privacy
  | Kind
  | Constraint
  | Manifest
  | Variance
  | Record_mismatch of record_mismatch
  | Variant_mismatch of variant_change list
  | Unboxed_representation of position
  | Immediate of Type_immediacy.Violation.t

let prefix ppf x =
  let kind = match x with
    | Name_mismatch _ | Type_mismatch _ | Swap _ | Displacement _ -> Diffing.Modification
    | Insert _ -> Diffing.Insertion
    | Delete _ -> Diffing.Deletion
  in
  let style k ppf inner =
    let sty = Diffing.style k in
    Format.pp_open_stag ppf (Misc.Color.Style sty);
    Format.kfprintf (fun ppf -> Format.pp_close_stag ppf () ) ppf inner
  in
  match x with
  | Name_mismatch {pos; _ } | Type_mismatch {pos; _} | Insert { pos; _ } | Delete { pos; _ } ->
      style kind ppf "%i) " pos
  | Swap { got=got,_; expected=expected,_ } | Displacement { got; expected; _ } ->
      style kind ppf "%i-%i) " got expected


let report_label_mismatch first second ppf err =
  let pr fmt = Format.fprintf ppf fmt in
  match (err : label_mismatch) with
  | Type -> pr "The types are not equal."
  | Mutability ord ->
      pr "%s is mutable and %s is not."
        (String.capitalize_ascii  (choose ord first second))
        (choose_other ord first second)

let pp_record_diff first second prefix decl ppf (x : record_change) =
  match x with
  | Delete cd ->
      Format.fprintf ppf "%aAn extra field, %s, is provided in %s %s."
        prefix x (Ident.name cd.delete.ld_id) first decl
  | Insert cd ->
      Format.fprintf  ppf "%aA field, %s, is missing in %s %s."
        prefix x (Ident.name cd.insert.ld_id) first decl
  | Type_mismatch {got=lbl1; expected=lbl2; reason} ->
      Format.fprintf ppf
        "@[<hv>%aFields do not match:@;<1 2>\
         %a@ is not compatible with:\
         @;<1 2>%a@ %a@]"
        prefix x
        Printtyp.label lbl1
        Printtyp.label lbl2
        (report_label_mismatch first second) reason
  | Name_mismatch n ->
      Format.fprintf ppf "%aFields have different names, %s and %s."
        prefix x n.got n.expected
  | Swap {expected=_, name1; got=_, name2} ->
      Format.fprintf ppf "%aFields %s and %s have been swapped."
        prefix x name1 name2
  | Displacement {name; got:int; expected:int} ->
      Format.fprintf ppf "%aField %s has been moved from position %d to %d."
        prefix x name got expected



let report_patch pr_diff first second decl ppf patch =
  let nl ppf () = Format.fprintf ppf "@," in
  let no_prefix _ppf _ = () in
  match patch with
  | [ elt ] ->
      Format.fprintf ppf "@[<hv>%a@]"
        (pr_diff first second no_prefix decl) elt
  | _ ->
      let pp_diff = pr_diff first second prefix decl in
      Format.fprintf ppf "@[<hv>%a@]"
        (Format.pp_print_list ~pp_sep:nl pp_diff) patch

let report_record_mismatch first second decl ppf err =
  let pr fmt = Format.fprintf ppf fmt in
  match err with
  | Label_mismatch patch ->
      report_patch pp_record_diff first second decl ppf patch
  | Unboxed_float_representation ord ->
      pr "@[<hv>Their internal representations differ:@ %s %s %s.@]"
        (choose ord first second) decl
        "uses unboxed float representation"

let report_constructor_mismatch first second decl ppf err =
  let pr fmt  = Format.fprintf ppf fmt in
  match (err : constructor_mismatch) with
  | Type -> pr "The types are not equal."
  | Arity -> pr "They have different arities."
  | Inline_record err -> report_patch pp_record_diff first second decl ppf err
  | Kind ord ->
      pr "%s uses inline records and %s doesn't."
        (String.capitalize_ascii (choose ord first second))
        (choose_other ord first second)
  | Explicit_return_type ord ->
      pr "%s has explicit return type and %s doesn't."
        (String.capitalize_ascii (choose ord first second))
        (choose_other ord first second)

let pp_variant_diff first second prefix decl ppf (x : variant_change) =
  match x with
  | Delete cd ->
      Format.fprintf ppf  "%aAn extra constructor, %s, is provided in %s %s."
        prefix x (Ident.name cd.delete.cd_id) first decl
  | Insert cd ->
      Format.fprintf ppf "%aA constructor, %s, is missing in %s %s."
        prefix x (Ident.name cd.insert.cd_id) first decl
  | Type_mismatch {got; expected; reason} ->
      Format.fprintf ppf
        "@[<hv>%aConstructors do not match:@;<1 2>\
         %a@ is not compatible with:\
         @;<1 2>%a@ %a@]"
        prefix x
        Printtyp.constructor got
        Printtyp.constructor expected
        (report_constructor_mismatch first second decl) reason
  | Name_mismatch n ->
      Format.fprintf ppf
        "%aConstructors have different names, %s and %s."
        prefix x n.got n.expected
  | Swap {got=_,name1; expected=_, name2} ->
      Format.fprintf ppf
        "%aConstructors %s and %s have been swapped."
        prefix x name1 name2
  | Displacement {name; got; expected} ->
      Format.fprintf ppf
        "%aConstructor %s has been moved to position %d from position %d."
        prefix x name got expected



let report_extension_constructor_mismatch first second decl ppf err =
  let pr fmt = Format.fprintf ppf fmt in
  match (err : extension_constructor_mismatch) with
  | Constructor_privacy -> pr "A private type would be revealed."
  | Constructor_mismatch (id, ext1, ext2, err) ->
      pr "@[<hv>Constructors do not match:@;<1 2>%a@ is not compatible with:\
          @;<1 2>%a@ %a@]"
        (Printtyp.extension_only_constructor id) ext1
        (Printtyp.extension_only_constructor id) ext2
        (report_constructor_mismatch first second decl) err

let report_type_mismatch0 first second decl ppf err =
  let pr fmt = Format.fprintf ppf fmt in
  match err with
  | Arity -> pr "They have different arities."
  | Privacy -> pr "A private type would be revealed."
  | Kind -> pr "Their kinds differ."
  | Constraint -> pr "Their constraints differ."
  | Manifest -> ()
  | Variance -> pr "Their variances do not agree."
  | Record_mismatch err -> report_record_mismatch first second decl ppf err
  | Variant_mismatch err ->
      report_patch pp_variant_diff first second decl ppf err
  | Unboxed_representation ord ->
      pr "Their internal representations differ:@ %s %s %s."
         (choose ord first second) decl
         "uses unboxed representation"
  | Immediate violation ->
      let first = StringLabels.capitalize_ascii first in
      match violation with
      | Type_immediacy.Violation.Not_always_immediate ->
          pr "%s is not an immediate type." first
      | Type_immediacy.Violation.Not_always_immediate_on_64bits ->
          pr "%s is not a type that is always immediate on 64 bit platforms."
            first

let report_type_mismatch first second decl ppf err =
  if err = Manifest then () else
  Format.fprintf ppf "@ %a" (report_type_mismatch0 first second decl) err

module Record_diffing = struct

  let compare_labels env params1 params2
      (ld1 : Types.label_declaration)
      (ld2 : Types.label_declaration) =
    if ld1.ld_mutable <> ld2.ld_mutable
    then
      let ord = if ld1.ld_mutable = Asttypes.Mutable then First else Second in
      Some (Mutability  ord)
    else
    if Ctype.equal env true (ld1.ld_type::params1) (ld2.ld_type::params2)
    then None
    else Some (Type : label_mismatch)

  let rec equal ~loc env params1 params2 n
      (labels1 : Types.label_declaration list)
      (labels2 : Types.label_declaration list) =
    match labels1, labels2 with
    | [], [] -> true
    | _ :: _ , [] | [], _ :: _ -> false
    | ld1 :: rem1, ld2 :: rem2 ->
        if Ident.name ld1.ld_id <> Ident.name ld2.ld_id
        then false
        else begin
          Builtin_attributes.check_deprecated_mutable_inclusion
            ~def:ld1.ld_loc
            ~use:ld2.ld_loc
            loc
            ld1.ld_attributes ld2.ld_attributes
            (Ident.name ld1.ld_id);
          match compare_labels env params1 params2 ld1 ld2 with
          | Some _ -> false
          (* add arguments to the parameters, cf. PR#7378 *)
          | None ->
              equal ~loc env
                (ld1.ld_type::params1) (ld2.ld_type::params2)
                (n+1)
                rem1 rem2
        end

  let update _ () = ()
  let test _loc env params1 params2 ()
      (n1, (lbl1:Types.label_declaration))
      (_,(lbl2:Types.label_declaration)) =
    if Ident.name lbl1.ld_id <> Ident.name lbl2.ld_id then
      Error (Name_mismatch {pos=n1; got=Ident.name lbl1.ld_id; expected=Ident.name lbl2.ld_id})
    else
      match compare_labels env params1 params2 lbl1 lbl2 with
      | Some reason ->
          Error (Type_mismatch {pos=n1; got=lbl1; expected=lbl2; reason})
      | None -> Ok ()

  module Swap = Stdlib.Map.Make(struct type t = string * string let compare: t -> t -> int = Stdlib.compare end)
  module Move = Stdlib.Map.Make(String)
  type 'a edge =
    | Left of 'a
    | Right of 'a
    | Both of 'a * 'a

  let key (_, x:_ * Types.label_declaration) = Ident.name x.ld_id
  let edge key x y =
    let kx, ky = key x, key y in
    if kx <= ky then
      (kx,ky), Left (x,y)
    else
      (ky,kx), Right(x,y)

  let add_edge ex ey = match ex, ey with
    | ex, None -> Some ex
    | Left l, Some Right r | Right r, Some Left l -> Some (Both (l,r))
    | Both _ as b, _ | _, Some (Both _ as b)  -> Some b
    | l, _ -> Some l

  let moves key changes =
    let add (swaps,moves) = function
      | Diffing.Change (x,y,_) ->
          let k, edge = edge key x y in
          Swap.update k (add_edge edge) swaps, moves
      | Diffing.Insert (n, _ as nx) ->
          let k = key nx in
          let edge = Left n in
          swaps, Move.update k (add_edge edge) moves
      | Diffing.Delete nx ->
          let k, edge = key nx, Right (fst nx) in
          swaps, Move.update k (add_edge edge) moves
      | _ -> swaps, moves
    in
    List.fold_left add (Swap.empty,Move.empty) changes


  let swap key test swaps x y =
    let kx, ky = key x, key y in
    let key = if kx <= ky then kx, ky else ky, kx in
    match Swap.find_opt key swaps with
    | None | Some (Left _ | Right _)-> None
    | Some Both ((ll,lr),(rl,rr)) ->
        match test () ll rr,  test () lr rl with
        | Ok _, Ok _ ->
            Some ((fst x,kx),(fst y,ky))
        | Error _, _ | _, Error _ -> None

  let move key moves x =
    let name = key x in
    match Move.find_opt name moves with
    | None | Some (Left _ | Right _)-> None
    | Some Both (got,expected) ->
        Some (Displacement {name; got; expected})

  let diffing loc env params1 params2 cstrs_1 cstrs_2 =
    let test = test loc env params1 params2 in
    let cstrs_1 = List.mapi (fun n x -> 1+n,x) cstrs_1 in
    let cstrs_2 = List.mapi (fun n x -> 1+n,x) cstrs_2 in
    let raw = Diffing.diff
        ~weight:Diffing.default_weight
        ~test
        ~update ()
        (Array.of_list cstrs_1)
        (Array.of_list cstrs_2)
    in
    let swaps, moves = moves key raw in
    let filter = function
      | Diffing.Keep _ -> None
      | Diffing.Insert (pos,insert as x) ->
          begin match move key moves x with
          | Some _ as displacement -> displacement
          | None -> Some (Insert {pos;insert})
          end
      | Diffing.Delete (pos,x) -> Some (Delete {pos;delete=x})
      | Diffing.Change(x,y, reason) ->
          match swap key test swaps x y with
          | Some (got,expected) -> if got <= expected then Some (Swap {got; expected}) else None
          | None -> Some reason
    in
    List.filter_map filter raw


  let compare ~loc env params1 params2 l r =
    if equal ~loc env params1 params2 0 l r then
      None
    else
      Some (diffing loc env params1 params2 l r)


  let compare_with_representation ~loc env params1 params2 l r rep1 rep2 =
    if equal ~loc env params1 params2 0 l r then
      if rep1 = rep2 then
        None
      else
        let pos = if rep1=Record_float then First else Second in
        Some (Unboxed_float_representation pos)
    else
      Some (Label_mismatch (diffing loc env params1 params2 l r))

end

module Variant_diffing = struct

  let compare_constructor_arguments ~loc env params1 params2 arg1 arg2 =
    match arg1, arg2 with
    | Types.Cstr_tuple arg1, Types.Cstr_tuple arg2 ->
        if List.length arg1 <> List.length arg2 then
          Some (Arity : constructor_mismatch)
        else if
          (* Ctype.equal must be called on all arguments at once, cf. PR#7378 *)
          Ctype.equal env true (params1 @ arg1) (params2 @ arg2)
        then None else Some Type
    | Types.Cstr_record l1, Types.Cstr_record l2 ->
        Option.map
          (fun rec_err -> Inline_record rec_err)
          (Record_diffing.compare env ~loc params1 params2 l1 l2)
    | Types.Cstr_record _, _ -> Some (Kind First : constructor_mismatch)
    | _, Types.Cstr_record _ -> Some (Kind Second : constructor_mismatch)

  let compare_constructors ~loc env params1 params2 res1 res2 args1 args2 =
    match res1, res2 with
    | Some r1, Some r2 ->
        if Ctype.equal env true [r1] [r2] then
          compare_constructor_arguments ~loc env [r1] [r2] args1 args2
        else Some Type
    | Some _, None -> Some (Explicit_return_type First)
    | None, Some _ -> Some (Explicit_return_type Second)
    | None, None ->
        compare_constructor_arguments ~loc env params1 params2 args1 args2

  let equal ~loc env params1 params2
      (cstrs1 : Types.constructor_declaration list)
      (cstrs2 : Types.constructor_declaration list) =
    List.length cstrs1 = List.length cstrs2 &&
    List.for_all2 (fun (cd1:Types.constructor_declaration)
                    (cd2:Types.constructor_declaration) ->
        Ident.name cd1.cd_id = Ident.name cd2.cd_id
        &&
        begin
          Builtin_attributes.check_alerts_inclusion
            ~def:cd1.cd_loc
            ~use:cd2.cd_loc
            loc
            cd1.cd_attributes cd2.cd_attributes
            (Ident.name cd1.cd_id)
          ;
        match compare_constructors ~loc env params1 params2
                cd1.cd_res cd2.cd_res cd1.cd_args cd2.cd_args with
        | Some _ -> false
        | None -> true
      end) cstrs1 cstrs2

  let update _ () = ()

  let test loc env params1 params2 ()
      (n1, cd1:int * Types.constructor_declaration)
      (_n2, cd2:int * Types.constructor_declaration) =
    if Ident.name cd1.cd_id <> Ident.name cd2.cd_id then
      Error (Name_mismatch {pos=n1; got=Ident.name cd1.cd_id;expected= Ident.name cd2.cd_id})
    else
      match compare_constructors ~loc env params1 params2
              cd1.cd_res cd2.cd_res cd1.cd_args cd2.cd_args with
      | Some reason ->
          Error (Type_mismatch {pos=n1; got=cd1; expected=cd2; reason})
      | None -> Ok (Ident.name cd1.cd_id)

  let diffing loc env params1 params2 cstrs_1 cstrs_2 =
    let test = test loc env params1 params2 in
    let cstrs_1 = List.mapi (fun n x -> 1 + n, x) cstrs_1 in
    let cstrs_2 = List.mapi (fun n x -> 1 + n, x) cstrs_2 in
    let raw = Diffing.diff
      ~weight:Diffing.default_weight
      ~test
      ~update ()
      (Array.of_list cstrs_1)
      (Array.of_list cstrs_2)
    in
    let key (_, x: int * Types.constructor_declaration) = Ident.name x.cd_id in
    let swaps, moves = Record_diffing.moves key raw in
    let filter = function
      | Diffing.Keep _ -> None
      | Diffing.Insert (pos,insert as x) ->
          begin match Record_diffing.move key moves x with
          | Some _ as displacement -> displacement
          | None -> Some (Insert {pos;insert})
          end
      | Diffing.Delete (pos,x) -> Some (Delete {pos;delete=x})
      | Diffing.Change(x,y, reason) ->
          match Record_diffing.swap key test swaps x y with
          | Some (got,expected) -> if got <= expected then Some (Swap {got; expected}) else None
          | None -> Some reason
    in
    List.filter_map filter raw



  let compare ~loc env params1 params2 l r =
    if equal ~loc env params1 params2 l r then
      None
    else
      Some (diffing loc env params1 params2 l r)

end

let type_declarations ?(equality = false) ~loc env ~mark name
      decl1 path decl2 =
  Builtin_attributes.check_alerts_inclusion
    ~def:decl1.type_loc
    ~use:decl2.type_loc
    loc
    decl1.type_attributes decl2.type_attributes
    name;
  if decl1.type_arity <> decl2.type_arity then Some Arity else
  if not (private_flags decl1 decl2) then Some Privacy else
  let err = match (decl1.type_manifest, decl2.type_manifest) with
      (_, None) ->
        if Ctype.equal env true decl1.type_params decl2.type_params
        then None else Some Constraint
    | (Some ty1, Some ty2) ->
        if type_manifest env ty1 decl1.type_params ty2 decl2.type_params
            decl2.type_private
        then None else Some Manifest
    | (None, Some ty2) ->
        let ty1 =
          Btype.newgenty (Tconstr(path, decl2.type_params, ref Mnil))
        in
        if Ctype.equal env true decl1.type_params decl2.type_params then
          if Ctype.equal env false [ty1] [ty2] then None
          else Some Manifest
        else Some Constraint
  in
  if err <> None then err else
  let err =
    match (decl2.type_kind, decl1.type_unboxed.unboxed,
           decl2.type_unboxed.unboxed) with
    | Type_abstract, _, _ -> None
    | _, true, false -> Some (Unboxed_representation First)
    | _, false, true -> Some (Unboxed_representation Second)
    | _ -> None
  in
  if err <> None then err else
  let err = match (decl1.type_kind, decl2.type_kind) with
      (_, Type_abstract) -> None
    | (Type_variant cstrs1, Type_variant cstrs2) ->
        if mark then begin
          let mark usage cstrs =
            List.iter (Env.mark_constructor_used usage) cstrs
          in
          let usage : Env.constructor_usage =
            if decl2.type_private = Public then Env.Exported
            else Env.Exported_private
          in
          mark usage cstrs1;
          if equality then mark Env.Exported cstrs2
        end;
        Option.map
          (fun var_err -> Variant_mismatch var_err)
          (Variant_diffing.compare ~loc env
             decl1.type_params
             decl2.type_params
             cstrs1
             cstrs2
          )
    | (Type_record(labels1,rep1), Type_record(labels2,rep2)) ->
        if mark then begin
          let mark usage lbls =
            List.iter (Env.mark_label_used usage) lbls
          in
          let usage : Env.label_usage =
            if decl2.type_private = Public then Env.Exported
            else Env.Exported_private
          in
          mark usage labels1;
          if equality then mark Env.Exported labels2
        end;
        Option.map (fun rec_err -> Record_mismatch rec_err)
          (Record_diffing.compare_with_representation ~loc env
             decl1.type_params decl2.type_params
             labels1 labels2
             rep1 rep2)
    | (Type_open, Type_open) -> None
    | (_, _) -> Some Kind
  in
  if err <> None then err else
  let abstr = decl2.type_kind = Type_abstract && decl2.type_manifest = None in
  (* If attempt to assign a non-immediate type (e.g. string) to a type that
   * must be immediate, then we error *)
  let err =
    if not abstr then
      None
    else
      match
        Type_immediacy.coerce decl1.type_immediate ~as_:decl2.type_immediate
      with
      | Ok () -> None
      | Error violation -> Some (Immediate violation)
  in
  if err <> None then err else
  let need_variance =
    abstr || decl1.type_private = Private || decl1.type_kind = Type_open in
  if not need_variance then None else
  let abstr = abstr || decl2.type_private = Private in
  let opn = decl2.type_kind = Type_open && decl2.type_manifest = None in
  let constrained ty = not (Btype.(is_Tvar (repr ty))) in
  if List.for_all2
      (fun ty (v1,v2) ->
        let open Variance in
        let imp a b = not a || b in
        let (co1,cn1) = get_upper v1 and (co2,cn2) = get_upper v2 in
        (if abstr then (imp co1 co2 && imp cn1 cn2)
         else if opn || constrained ty then (co1 = co2 && cn1 = cn2)
         else true) &&
        let (p1,n1,i1,j1) = get_lower v1 and (p2,n2,i2,j2) = get_lower v2 in
        imp abstr (imp p2 p1 && imp n2 n1 && imp i2 i1 && imp j2 j1))
      decl2.type_params (List.combine decl1.type_variance decl2.type_variance)
  then None else Some Variance

(* Inclusion between extension constructors *)

let extension_constructors ~loc env ~mark id ext1 ext2 =
  if mark then begin
    let usage : Env.constructor_usage =
      if ext2.ext_private = Public then Env.Exported
      else Env.Exported_private
    in
    Env.mark_extension_used usage ext1
  end;
  let ty1 =
    Btype.newgenty (Tconstr(ext1.ext_type_path, ext1.ext_type_params, ref Mnil))
  in
  let ty2 =
    Btype.newgenty (Tconstr(ext2.ext_type_path, ext2.ext_type_params, ref Mnil))
  in
  if not (Ctype.equal env true (ty1 :: ext1.ext_type_params)
                               (ty2 :: ext2.ext_type_params))
  then Some (Constructor_mismatch (id, ext1, ext2, Type))
  else
    let r =
      Variant_diffing.compare_constructors ~loc env
        ext1.ext_type_params ext2.ext_type_params
        ext1.ext_ret_type ext2.ext_ret_type
        ext1.ext_args ext2.ext_args
    in
    match r with
    | Some r -> Some (Constructor_mismatch (id, ext1, ext2, r))
    | None -> match ext1.ext_private, ext2.ext_private with
        Private, Public -> Some Constructor_privacy
      | _, _ -> None
