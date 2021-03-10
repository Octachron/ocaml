(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Florian Angeletti, projet Cambium, Inria Paris             *)
(*                                                                        *)
(*   Copyright 2021 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)


module Context = struct
  type pos =
    | Module of Ident.t
    | Modtype of Ident.t
    | Arg of Types.functor_parameter
    | Body of Types.functor_parameter

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
        Format.fprintf ppf "@[<2>module %a%a@]" Printtyp.ident id args rem
    | Modtype id :: rem ->
        Format.fprintf ppf "@[<2>module type %a =@ %a@]"
          Printtyp.ident id context_mty rem
    | Body x :: rem ->
        Format.fprintf ppf "functor (%s) ->@ %a" (argname x) context_mty rem
    | Arg x :: rem ->
        Format.fprintf ppf "functor (%s : %a) -> ..."
          (argname x) context_mty rem
    | [] ->
        Format.fprintf ppf "<here>"
  and context_mty ppf = function
      (Module _ | Modtype _) :: _ as rem ->
        Format.fprintf ppf "@[<2>sig@ %a@;<1 -2>end@]" context rem
    | cxt -> context ppf cxt
  and args ppf = function
      Body x :: rem ->
        Format.fprintf ppf "(%s)%a" (argname x) args rem
    | Arg x :: rem ->
        Format.fprintf ppf "(%s :@ %a) : ..." (argname  x) context_mty rem
    | cxt ->
        Format.fprintf ppf " :@ %a" context_mty cxt
  and argname = function
    | Types.Unit -> ""
    | Types.Named (None, _) -> "_"
    | Types.Named (Some id, _) -> Ident.name id

  let alt_pp ppf cxt =
    if cxt = [] then () else
    if List.for_all (function Module _ -> true | _ -> false) cxt then
      Format.fprintf ppf "in module %a," Printtyp.path (path_of_context cxt)
    else
      Format.fprintf ppf "@[<hv 2>at position@ %a,@]" context cxt

  let pp ppf cxt =
    if cxt = [] then () else
    if List.for_all (function Module _ -> true | _ -> false) cxt then
      Format.fprintf ppf "In module %a:@ " Printtyp.path (path_of_context cxt)
    else
      Format.fprintf ppf "@[<hv 2>At position@ %a@]@ " context cxt
end

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
  let rec transposition_under path (coerc:Typedtree.module_coercion) =
    match coerc with
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
    | (_, Typedtree.Tcoerce_none) :: q -> first_non_id path (pos + 1) q
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
        if not(Includemod.is_runtime_component item) then
          runtime_item k q
        else if k = 0 then
          item
        else
          runtime_item (k-1) q

  (* Find module type at position [path] and convert the [coerce_pos] path to
     a [pos] path *)
  let rec find env ctx path (mt:Types.module_type) = match mt, path with
    | (Mty_ident p | Mty_alias p), _ ->
        begin match (Env.find_modtype p env).mtd_type with
        | None -> raise Not_found
        | Some mt -> find env ctx path mt
        end
    | Mty_signature s , [] -> List.rev ctx, s
    | Mty_signature s, Item k :: q ->
        begin match runtime_item k s with
        | Sig_module (id, _, md,_,_) ->
            find env (Context.Module id :: ctx) q md.md_type
        | _ -> raise Not_found
        end
    | Mty_functor(Named (_,mt) as arg,_), InArg :: q ->
        find env (Context.Arg arg :: ctx) q mt
    | Mty_functor(arg, mt), InBody :: q ->
        find env (Context.Body arg :: ctx) q mt
    | _ -> raise Not_found

  let find env path mt = find env [] path mt
  let item mt k = Includemod.item_ident_name (runtime_item k mt)

  let pp_item ppf (id,_,kind) =
    Format.fprintf ppf "%s %S"
      (Includemod.kind_of_field_desc kind)
      (Ident.name id)

  let pp ctx_printer env ppf (mty,c) =
    try
      let p, k, l = transposition c in
      let ctx, mt = find env p mty in
      Format.fprintf ppf
        "@[<hv 2>Illegal permutation of runtime components in a module type.@ \
         @[For example,@ %a@]@ @[the %a@ and the %a are not in the same order@ \
         in the expected and actual module types.@]@]"
        ctx_printer ctx pp_item (item mt k) pp_item (item mt l)
    with Not_found -> (* this should not happen *)
      Format.fprintf ppf
        "Illegal permutation of runtime components in a module type."

end



module Err = Includemod.Error


module Short_name = struct

  type 'a item = {
    item: 'a;
    name : string;
    from: Ident.t option;
  }

  type 'a t =
    | Original of 'a
    | Synthetic of string * 'a

  type functor_param =
    | Unit
    | Named of (Ident.t option * Types.module_type t)

  let modtype (r : _ item) = match r.item with
    | Types.Mty_ident _
    | Types.Mty_alias _
    | Types.Mty_signature []
      -> Original r.item
    | Types.Mty_signature _ | Types.Mty_functor _
      -> Synthetic (r.name, r.item)

  let functor_param (ua : _ item) = match ua.item with
    | Types.Unit -> Unit
    | Types.Named (from, mty) ->
        Named (from, modtype { ua with item = mty ; from })

  let modexpr (r : _ item) = match r.item.Parsetree.pmod_desc with
    | Pmod_ident _
    | Pmod_structure []
      -> Original r.item
    | _
      -> Synthetic (r.name, r.item)

  let pp ppx = function
    | Original x -> ppx x
    | Synthetic (s,_) -> Format.dprintf "%s" s

  let pp_orig ppx = function
    | Original x | Synthetic (_, x) -> ppx x

end

module Pp = struct
  open Err
  let buffer = ref Bytes.empty
  let is_big obj =
    let size = !Clflags.error_size in
    size > 0 &&
    begin
      if Bytes.length !buffer < size then buffer := Bytes.create size;
      try ignore (Marshal.to_buffer !buffer 0 size obj []); false
      with _ -> true
    end

  let show_loc msg ppf loc =
    let pos = loc.Location.loc_start in
    if List.mem pos.Lexing.pos_fname [""; "_none_"; "//toplevel//"] then ()
    else Format.fprintf ppf "@\n@[<2>%a:@ %s@]" Location.print_loc loc msg

  let show_locs ppf (loc1, loc2) =
    show_loc "Expected declaration" ppf loc2;
    show_loc "Actual declaration" ppf loc1

  let core id x =
    match x with
    | Err.Value_descriptions diff ->
        let t1 = Printtyp.tree_of_value_description id diff.got in
        let t2 = Printtyp.tree_of_value_description id diff.expected in
        Format.dprintf
          "@[<hv 2>Values do not match:@ %a@;<1 -2>is not included in@ %a@]%a%t"
          !Oprint.out_sig_item t1
          !Oprint.out_sig_item t2
        show_locs (diff.got.val_loc, diff.expected.val_loc)
        Printtyp.Conflicts.print_explanations
    | Err.Type_declarations diff ->
        Format.dprintf "@[<v>@[<hv>%s:@;<1 2>%a@ %s@;<1 2>%a@]%a%a%t@]"
          "Type declarations do not match"
          !Oprint.out_sig_item
          (Printtyp.tree_of_type_declaration id diff.got Trec_first)
          "is not included in"
          !Oprint.out_sig_item
          (Printtyp.tree_of_type_declaration id diff.expected Trec_first)
          (Includecore.report_type_mismatch
             "the first" "the second" "declaration") diff.symptom
          show_locs (diff.got.type_loc, diff.expected.type_loc)
          Printtyp.Conflicts.print_explanations
    | Err.Extension_constructors diff ->
        Format.dprintf "@[<v>@[<hv>%s:@;<1 2>%a@ %s@;<1 2>%a@]@ %a%a%t@]"
          "Extension declarations do not match"
          !Oprint.out_sig_item
          (Printtyp.tree_of_extension_constructor id diff.got Text_first)
          "is not included in"
          !Oprint.out_sig_item
          (Printtyp.tree_of_extension_constructor id diff.expected Text_first)
          (Includecore.report_extension_constructor_mismatch
             "the first" "the second" "declaration") diff.symptom
          show_locs (diff.got.ext_loc, diff.expected.ext_loc)
          Printtyp.Conflicts.print_explanations
    | Err.Class_type_declarations diff ->
        Format.dprintf
          "@[<hv 2>Class type declarations do not match:@ \
           %a@;<1 -2>does not match@ %a@]@ %a%t"
          !Oprint.out_sig_item
          (Printtyp.tree_of_cltype_declaration id diff.got Trec_first)
          !Oprint.out_sig_item
          (Printtyp.tree_of_cltype_declaration id diff.expected Trec_first)
          Includeclass.report_error diff.symptom
          Printtyp.Conflicts.print_explanations
    | Err.Class_declarations {got;expected;symptom} ->
        let t1 = Printtyp.tree_of_class_declaration id got Trec_first in
        let t2 = Printtyp.tree_of_class_declaration id expected Trec_first in
        Format.dprintf
          "@[<hv 2>Class declarations do not match:@ \
           %a@;<1 -2>does not match@ %a@]@ %a%t"
          !Oprint.out_sig_item t1
          !Oprint.out_sig_item t2
          Includeclass.report_error symptom
          Printtyp.Conflicts.print_explanations

  let missing_field ppf item =
    let id, loc, kind =  Includemod.item_ident_name item in
    Format.fprintf ppf "The %s `%a' is required but not provided%a"
      (Includemod.kind_of_field_desc kind) Printtyp.ident id
    (show_loc "Expected declaration") loc

  let module_types {got=mty1; expected=mty2} =
    Format.dprintf
      "@[<hv 2>Modules do not match:@ \
       %a@;<1 -2>is not included in@ %a@]"
      !Oprint.out_module_type (Printtyp.tree_of_modtype mty1)
      !Oprint.out_module_type (Printtyp.tree_of_modtype mty2)

  let eq_module_types {got=mty1; expected=mty2} =
    Format.dprintf
      "@[<hv 2>Module types do not match:@ \
       %a@;<1 -2>is not equal to@ %a@]"
      !Oprint.out_module_type (Printtyp.tree_of_modtype mty1)
      !Oprint.out_module_type (Printtyp.tree_of_modtype mty2)


  let module_type_declarations id {got=d1 ; expected=d2} =
    Format.dprintf
      "@[<hv 2>Module type declarations do not match:@ \
       %a@;<1 -2>does not match@ %a@]"
      !Oprint.out_sig_item (Printtyp.tree_of_modtype_declaration id d1)
      !Oprint.out_sig_item (Printtyp.tree_of_modtype_declaration id d2)

  let interface_mismatch ppf diff =
    Format.fprintf ppf
      "The implementation %s@ does not match the interface %s:@ "
      diff.got diff.expected

  let core_module_type_symptom x  =
    match x with
    | Not_an_alias | Not_an_identifier | Abstract_module_type
    | Incompatible_aliases ->
        if Printtyp.Conflicts.exists () then
          Some Printtyp.Conflicts.print_explanations
        else None
    | Unbound_module_path path ->
        Some(Format.dprintf "Unbound module %a" Printtyp.path path)

  let dmodtype mty =
    let tmty = Printtyp.tree_of_modtype mty in
    Format.dprintf "%a" !Oprint.out_module_type tmty

  let definition_of_functor_param x = match Short_name.functor_param x with
    | Short_name.Unit -> Format.dprintf "()"
    | Short_name.Named(_,short_mty) ->
        match short_mty with
        | Original mty -> dmodtype mty
        | Synthetic (name, mty) ->
            Format.dprintf
              "%s@ =@ %t" name (dmodtype mty)

  let short_functor_param x = match Short_name.functor_param x with
    | Short_name.Unit -> Format.dprintf "()"
    | Short_name.Named (_, short_mty) ->
        Short_name.pp dmodtype short_mty

  let functor_param x = match Short_name.functor_param x with
    | Short_name.Unit -> Format.dprintf "()"
    | Short_name.Named (None, Original (Mty_signature []) ) ->
        Format.dprintf "(sig end)"
    | Short_name.Named (None, short_mty) ->
        Short_name.pp dmodtype short_mty
    | Short_name.Named (Some p, short_mty) ->
        Format.dprintf "(%s : %t)"
          (Ident.name p) (Short_name.pp dmodtype short_mty)

  let definition_of_argument ua =
    let arg, mty = ua.Short_name.item in
    match arg with
    | Unit_arg -> Format.dprintf "()"
    | Named_arg p ->
        let mty = Short_name.modtype { ua with item = mty } in
        Format.dprintf
          "%a@ :@ %t"
          Printtyp.path p
          (Short_name.pp_orig dmodtype mty)
    | Anonymous md ->
        let short_md = Short_name.modexpr { ua with item = md } in
        begin match short_md with
        | Original md -> fun ppf -> Pprintast.module_expr ppf md
        | Synthetic (name, md) -> fun ppf ->
            Format.fprintf ppf
              "%s@ =@ %a" name Pprintast.module_expr md
        end

  let short_argument ua =
    let arg, _mty = ua.Short_name.item in
    match arg with
    | Unit_arg -> Format.dprintf "()"
    | Named_arg p -> fun ppf -> Printtyp.path ppf p
    | Anonymous md ->
        let short_md = Short_name.modexpr { ua with item=md } in
        Short_name.pp (fun x ppf -> Pprintast.module_expr ppf x) short_md

  let style = function
    | Diffing.Keep _ -> Misc.Color.[ FG Green ]
    | Diffing.Delete _ -> Misc.Color.[ FG Red; Bold]
    | Diffing.Insert _ -> Misc.Color.[ FG Red; Bold]
    | Diffing.Change _ -> Misc.Color.[ FG Magenta; Bold]

  let prefix ppf (pos, p) =
    let sty = style p in
    Format.pp_open_stag ppf (Misc.Color.Style sty);
    Format.fprintf ppf "%i." pos;
    Format.pp_close_stag ppf ()

  let param_id x = match x.Short_name.item with
    | Types.Named (Some _ as x,_) -> x
    | Types.(Unit | Named(None,_)) -> None

  let got_arg = function
    | Diffing.Delete mty
    | Diffing.Keep (mty,_,_)
    | Diffing.Change (mty,_,_) as x ->
        Some (None,(x,mty))
    | Diffing.Insert _ -> None

 let got_app = function
    | Diffing.Delete mty
    | Diffing.Keep (mty,_,_)
    | Diffing.Change (mty,_,_) as x ->
        Some (param_id mty,(x,mty))
    | Diffing.Insert _ -> None

  let expected = function
    | Diffing.Insert mty
    | Diffing.Keep(_,mty,_)
    | Diffing.Change (_,mty,_) as x ->
        Some (param_id mty,(x, mty))
    | Diffing.Delete _ -> None

  let space ppf () = Format.fprintf ppf "@ "
  let params_diff sep proj printer patch =
    let elt (x,param) =
      let sty = style x in
      Format.dprintf "%a%t%a"
        Format.pp_open_stag (Misc.Color.Style sty)
        (printer param)
        Format.pp_close_stag ()
    in
    let params = List.filter_map proj @@ List.map snd patch in
    Printtyp.functor_parameters ~sep elt params

end


module FunctorDiff = struct
  (* Simplication for printing *)

  let shortname side pos =
    match side with
    | `Got -> Format.sprintf "$S%d" pos
    | `Expected -> Format.sprintf "$T%d" pos
    | `Unneeded -> "..."

  let to_shortnames ctx patch =
    let to_shortname side pos mty =
      {Short_name. name = (shortname side pos); item = mty; from=None }
    in
    let elide_if_app s = match ctx with
      | `App -> `Unneeded
      | `Sig -> s
    in
    let aux i d =
      let pos = i + 1 in
      let d = match d with
        | Diffing.Insert mty ->
            Diffing.Insert (to_shortname `Expected pos mty)
        | Diffing.Delete mty ->
            Diffing.Delete (to_shortname (elide_if_app `Got) pos mty)
        | Diffing.Change (g, e, p) ->
            Diffing.Change
              (to_shortname `Got pos g,
               to_shortname `Expected pos e, p)
        | Diffing.Keep (g, e, p) ->
            Diffing.Keep (to_shortname `Got pos g,
                       to_shortname (elide_if_app `Expected) pos e, p)
      in
      pos, d
    in
    List.mapi aux patch

  let drop_inserted_suffix patch =
    let rec drop = function
      | Diffing.Insert _ :: q -> drop q
      | rest -> List.rev rest in
    drop (List.rev patch)

  let prepare_patch ~drop ~ctx patch =
    let drop_suffix x = if drop then drop_inserted_suffix x else x in
    patch |> drop_suffix |> to_shortnames ctx

end



module Linearize = struct
  (** Construct a linear presentation of the error tree *)
  open Err

  let with_context ?loc ctx printer diff =
    Location.msg ?loc "%a%a" Context.pp (List.rev ctx)
      printer diff

  let dwith_context ?loc ctx printer =
    Location.msg ?loc "%a%t" Context.pp (List.rev ctx) printer

  let dwith_context_and_elision ?loc ctx printer diff =
    if Pp.is_big (diff.got,diff.expected) then
      Location.msg ?loc "..."
    else
      dwith_context ?loc ctx (printer diff)

  type ('a,'b) patch =
    ( 'a Short_name.item, 'b Short_name.item,
      Typedtree.module_coercion, arg_functor_param_symptom
    ) Diffing.change
  type ('a,'b) t = {
    msgs: Location.msg list;
    post:
      (Env.t * (int * ('a, 'b) patch) list) option
  }

  let rec module_type ~expansion_token ~eqmode ~env ~before ~ctx diff =
    match diff.symptom with
    | Invalid_module_alias _ (* the difference is non-informative here *)
    | After_alias_expansion _ (* we print only the expanded module types *) ->
        module_type_symptom ~eqmode ~expansion_token ~env ~before ~ctx
          diff.symptom
    | _ ->
        let inner = if eqmode then Pp.eq_module_types else Pp.module_types in
        let before = match diff.symptom with
          | Functor Params _ -> before
          | _ ->
              let next = dwith_context_and_elision ctx inner diff in
              next :: before in
        module_type_symptom ~eqmode ~expansion_token ~env ~before ~ctx
          diff.symptom

  and module_type_symptom ~eqmode ~expansion_token ~env ~before ~ctx = function
    | Mt_core core ->
        begin match Pp.core_module_type_symptom core with
        | None -> { msgs = before; post = None }
        | Some msg ->
            { msgs = Location.msg "%t" msg :: before; post = None }
        end
    | Signature s -> signature ~expansion_token ~env ~before ~ctx s
    | Functor f -> functor_symptom ~expansion_token ~env ~before ~ctx f
    | After_alias_expansion diff ->
        module_type ~eqmode ~expansion_token ~env ~before ~ctx diff
    | Invalid_module_alias path ->
        let printer =
          Format.dprintf "Module %a cannot be aliased" Printtyp.path path
        in
        let msgs = dwith_context ctx printer :: before in
        { msgs; post = None }


  and functor_symptom ~expansion_token ~env ~before ~ctx = function
    | Result res ->
        module_type ~expansion_token ~eqmode:false ~env ~before ~ctx res
    | Params {got; expected; symptom=()} ->
        let d =
          Includemod.Functor_inclusion_diff.diff env got expected
          |> FunctorDiff.prepare_patch ~drop:false ~ctx:`Sig
        in
        let actual = Pp.(params_diff space got_app functor_param d) in
        let expected =
          Pp.(params_diff space expected functor_param d)
        in
        let main =
          Format.dprintf
            "@[<hv 2>Modules do not match:@ \
             @[functor@ %t@ -> ...@]@;<1 -2>is not included in@ \
             @[functor@ %t@ -> ...@]@]"
            actual expected
        in
        let post = if expansion_token then Some (env,d) else None in
        { msgs = dwith_context ctx main :: before; post }

  and signature ~expansion_token ~env:_ ~before ~ctx sgs =
    Printtyp.wrap_printing_env ~error:true sgs.env (fun () ->
    match sgs.missings, sgs.incompatibles with
    | a :: l , _ ->
        let more =
          if expansion_token then
            with_context ctx Pp.missing_field a
            :: List.map (Location.msg "%a" Pp.missing_field) l
          else
            []
        in
        let msgs = more @ before in
        { msgs; post = None }
    | [], a :: _ -> sigitem ~expansion_token ~env:sgs.env ~before ~ctx a
    | [], [] -> assert false
      )
  and sigitem ~expansion_token ~env ~before ~ctx (name,s) = match s with
    | Core c ->
        { msgs = dwith_context ctx (Pp.core name c):: before; post = None }
    | Module_type diff ->
        module_type ~expansion_token ~eqmode:false ~env ~before
          ~ctx:(Context.Module name :: ctx) diff
    | Module_type_declaration diff ->
        module_type_decl ~expansion_token ~env ~before ~ctx name diff
  and module_type_decl ~expansion_token ~env ~before ~ctx id diff =
    let next =
      dwith_context_and_elision ctx (Pp.module_type_declarations id) diff in
    let before = next :: before in
    match diff.symptom with
    | Not_less_than mts ->
        let before =
          Location.msg "The first module type is not included in the second"
          :: before in
        module_type ~expansion_token ~eqmode:true ~before ~env
          ~ctx:(Context.Modtype id :: ctx) mts
    | Not_greater_than mts ->
        let before =
          Location.msg "The second module type is not included in the first"
          :: before in
        module_type ~expansion_token ~eqmode:true ~before ~env
          ~ctx:(Context.Modtype id :: ctx) mts
    | Incomparable mts ->
        module_type ~expansion_token ~eqmode:true ~env ~before
          ~ctx:(Context.Modtype id :: ctx) mts.less_than
    | Illegal_permutation c ->
        begin match diff.got.Types.mtd_type with
        | None -> assert false
        | Some mty ->
            let main =
            with_context (Modtype id::ctx)
              (Illegal_permutation.pp Context.alt_pp env) (mty,c) in
            { msgs = main :: before; post = None }
        end

  let insert_suberror mty =
    Format.dprintf
      "An argument appears to be missing with module type@;<1 2>@[%t@]"
      (Pp.definition_of_functor_param mty)

  let delete_suberror mty =
    Format.dprintf
      "An extra argument is provided of module type@;<1 2>@[%t@]"
      (Pp.definition_of_functor_param mty)

  let delete_suberror_app mty =
    Format.dprintf
      "The following extra argument is provided@;<1 2>@[%t@]"
      (Pp.definition_of_argument mty)

  let ok_suberror x y =
    Format.dprintf
      "Module types %t and %t match"
      (Pp.short_functor_param x)
      (Pp.short_functor_param y)

  let ok_suberror_app x y =
    let pp_orig_name = match Short_name.functor_param y with
      | Short_name.Named (_, Original mty) ->
          Format.dprintf " %t" (Pp.dmodtype mty)
      | _ -> ignore
    in
    Format.dprintf
      "Module %t matches the expected module type%t"
      (Pp.short_argument x)
      pp_orig_name

  let diff_arg g e more =
    let g = Pp.definition_of_functor_param g in
    let e = Pp.definition_of_functor_param e in
    Format.dprintf
      "Module types do not match:@ @[%t@]@;<1 -2>does not include@ \
       @[%t@]%t"
      g e (more ())

  let diff_app g e more =
    let g = Pp.definition_of_argument g in
    let e = Pp.definition_of_functor_param e in
    Format.dprintf
      "Modules do not match:@ @[%t@]@;<1 -2>\
       is not included in@ @[%t@]%t"
      g e (more ())

  let param_subcase sub ~expansion_token env (pos, diff) =
    Location.msg "%a%a%a %a@[<hv 2>%t@]%a"
      Format.pp_print_tab ()
      Format.pp_open_tbox ()
      Pp.prefix (pos, diff)
      Format.pp_set_tab ()
      (Printtyp.wrap_printing_env env ~error:true
         (fun () -> sub ~expansion_token env diff)
      )
     Format.pp_close_tbox ()

  let param_onlycase sub ~expansion_token env (_, diff) =
    Location.msg "%a@[<hv 2>%t@]"
      Format.pp_print_tab ()
      (Printtyp.wrap_printing_env env ~error:true
         (fun () -> sub ~expansion_token env diff)
      )

  let param_suberrors sub ~expansion_token env l =
    let rec aux = function
      | [] -> []
      | (_, Diffing.Keep _) as a :: q ->
          param_subcase sub ~expansion_token env a
          :: aux q
      | a :: q ->
          param_subcase sub ~expansion_token env a
          :: List.map (param_subcase sub ~expansion_token:false env) q
    in
    match l with
    | [a] -> [param_onlycase sub ~expansion_token env a]
    | l -> aux l

  let arg_incompatible = function
    | Types.Unit ->
        Format.dprintf
          "The functor was expected to be applicative at this position"
    | Types.Named _ ->
        Format.dprintf
          "The functor was expected to be generative at this position"

  let app_incompatible = function
    | Unit_arg ->
        Format.dprintf
          "The functor was expected to be applicative at this position"
    | Named_arg _ | Anonymous _ ->
        Format.dprintf
          "The functor was expected to be generative at this position"


  let rec diff_suberror:
    'a 'b 'c 'd. ('c -> _) -> ('a -> 'b -> _) -> expansion_token:_ -> _ ->
    'a -> 'b -> ('c,'d) Err.functor_param_symptom -> _
    = fun incompatible msg ~expansion_token env g e diff -> match diff with
    | Err.Incompatible_params (i,_) -> incompatible i
    | Err.Mismatch mty_diff ->
        let more () =
          let r =
            module_type_symptom ~eqmode:false ~expansion_token ~env ~before:[]
              ~ctx:[] mty_diff.symptom
          in
          let list l ppf = match l with
            | [] -> ()
            | _ :: _ ->
                Format.fprintf ppf "@;<1 -2>@[%a@]"
                  (Format.pp_print_list ~pp_sep:Pp.space
                     (fun ppf f -> f.Location.txt ppf)
                  )
                  l
          in
          let post = match r.post with
            | None -> []
            | Some (env, patch) ->
                param_suberrors arg ~expansion_token env patch in
          list (List.rev_append r.msgs post) in
        msg g e more

  and arg ~expansion_token env = function
    | Diffing.Insert mty -> insert_suberror mty
    | Diffing.Delete mty -> delete_suberror mty
    | Diffing.Change (g, e, d) ->
        diff_suberror arg_incompatible diff_arg ~expansion_token env g e d
    | Diffing.Keep (x, y, _) -> ok_suberror x y

  let app ~expansion_token env = function
    | Diffing.Insert mty -> insert_suberror mty
    | Diffing.Delete mty -> delete_suberror_app mty
    | Diffing.Change (g, e, d) ->
        diff_suberror app_incompatible diff_app ~expansion_token env g e d
    | Diffing.Keep (x, y, _) -> ok_suberror_app x y

  let all env = function
    | In_Compilation_unit diff ->
      let first = Location.msg "%a" Pp.interface_mismatch diff in
      signature ~expansion_token:true ~env ~before:[first] ~ctx:[] diff.symptom
    | In_Type_declaration (id,reason) ->
        let main = Location.msg "%t" (Pp.core id reason) in
        { msgs = [main]; post = None }
    | In_Module_type diff ->
        module_type ~expansion_token:true ~eqmode:false ~before:[] ~env ~ctx:[]
          diff
    | In_Signature diff ->
        signature ~expansion_token:true ~before:[] ~env ~ctx:[] diff
    | In_Expansion cmts ->
        match Pp.core_module_type_symptom cmts with
        | None -> assert false
        | Some main ->
            { msgs = [Location.msg "%t" main]; post = None }

  let coalesce { msgs; _ } =
    match List.rev msgs with
    | [] -> ignore
    | before ->
        let ctx ppf =
          Format.pp_print_list ~pp_sep:Pp.space
            (fun ppf x -> x.Location.txt ppf)
            ppf before in
        ctx
end



let err_msgs (env, err) =
  Printtyp.Conflicts.reset();
  Printtyp.wrap_printing_env ~error:true env (fun () ->
      let l = Linearize.all env err in
      let main = Linearize.coalesce l in
      let sub = match l.Linearize.post with
        | None -> []
        | Some (env,post) ->
            Linearize.(param_suberrors arg) ~expansion_token:true env post in
      sub, main
    )

let report_error err =
  let sub, main = err_msgs err in
  Location.errorf ~loc:Location.(in_file !input_name) ~sub "%t" main

let report_apply_error ~loc env (lid_app, mty_f, args) =
  let may_print_app ppf = match lid_app with
    | None -> ()
    | Some lid -> Format.fprintf ppf "%a " Printtyp.longident lid
  in
  let d =
    Includemod.Functor_app_diff.diff env ~f:mty_f ~args
    |> FunctorDiff.prepare_patch ~drop:true ~ctx:`App
  in
  match d with
  | [ _, (Diffing.Change _ as c) ] ->
      Location.errorf ~loc "%t" (Linearize.app env ~expansion_token:true c)
  | _ ->
      Location.errorf ~loc
        ~sub:(Linearize.(param_suberrors app) env ~expansion_token:true d)
        "@[<hv>The functor application %tis ill-typed.@ \
         These arguments:@;<1 2>\
         @[%t@]@ do not match these parameters:@;<1 2>@[functor@ %t@ -> ...@]@]"
        may_print_app
        Pp.(params_diff space got_arg short_argument d)
        Pp.(params_diff space expected functor_param d)


(* We could do a better job to split the individual error items
   as sub-messages of the main interface mismatch on the whole unit. *)
let register () =
  Location.register_error_of_exn
    (function
      | Includemod.Error err -> Some (report_error err)
      | Includemod.Apply_error {loc; env; lid_app; mty_f; args} ->
          Some (Printtyp.wrap_printing_env env ~error:true (fun () ->
              report_apply_error ~loc env (lid_app, mty_f, args))
            )
      | _ -> None
    )
