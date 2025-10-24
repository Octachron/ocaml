(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*  Florian Angeletti, projet Cambium, INRIA Paris                        *)
(*                                                                        *)
(*   Copyright 2024 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Trace-specific printing *)

(* A configuration type that controls which trace we print.  This could be
   exposed, but we instead expose three separate
   [{unification,equality,moregen}] functions.  This also lets us
   give the unification case an extra optional argument without adding it to the
   equality and moregen cases. *)
type 'variety trace_format =
  | Unification : Errortrace.unification trace_format
  | Equality    : Errortrace.comparison  trace_format
  | Moregen     : Errortrace.comparison  trace_format

let incompatibility_phrase (type variety) : variety trace_format -> string =
  function
  | Unification -> "is not compatible with type"
  | Equality    -> "is not equal to type"
  | Moregen     -> "is not compatible with type"

(* Print a unification error *)
open Out_type
open Format_doc
module Fmt = Format_doc
module Style = Misc.Style

type 'a diff = 'a Out_type.diff = Same of 'a | Diff of 'a * 'a

let look_ahead tr expl_tr =
  let some x =
    Errortrace.map_diff
      (fun x -> Some (Out_type.Highlighted_type x.Errortrace.ty)) x
  in
  match tr with
  | [] -> []
  | _ :: q -> List.map some q @ [expl_tr]

let syntactic_diff_highlight d =
  let syntactic_highlight l r = Oprint.syntactic_highlight l r in
  match d.Errortrace.got, d.Errortrace.expected with
  | Same l, Same r ->
      let l, r = syntactic_highlight l r in
      { Errortrace.got = Same l; expected = Same r }
  | Diff (ty,expanded), Same r ->
      let expanded, r = syntactic_highlight expanded r in
      { Errortrace.got = Diff (ty, expanded); expected = Same r }
  | Same l, Diff (ty, expanded) ->
      let l, expanded = syntactic_highlight l expanded in
      { Errortrace.got = Same l; expected = Diff (ty, expanded) }
  | Diff (ty, expanded), Diff (ty', expanded') ->
      let ty, ty' = syntactic_highlight ty ty' in
      let expanded, expanded' = syntactic_highlight expanded expanded' in
      let got = Diff (ty, expanded) in
      let expected = Diff (ty', expanded') in
      { Errortrace.got; expected }

let highlighted_trees_of_type_expansion mode next d =
  syntactic_diff_highlight @@
  Errortrace.map2_diff (trees_of_type_expansion mode) next d

let trees_of_trace mode =
  List.map2 (highlighted_trees_of_type_expansion mode)

let trace ~intro ~but ~more ~but' ppf tr =
  let rec trace txt ppf = function
    | {Errortrace.got; expected} :: rem ->
        fprintf ppf "@,@[Type@;<1 2>%a@ %s@;<1 2>%a@]%a"
          pp_type_expansion got txt pp_type_expansion expected
          (trace txt) rem
    | [] -> ()
  in
  match tr with
  | head :: rest ->
      fprintf ppf "@[%a@;<1 2>%a@ %a@;<1 2>%a@]%a%a"
        pp_doc intro
        pp_type_expansion head.Errortrace.got
        pp_doc but
        pp_type_expansion head.Errortrace.expected
        pp_doc more
        (trace but') rest
  | [] -> ()

let is_row_constr d =
  let open Errortrace in
    Btype.is_constr_row ~allow_ident:true d.got.expanded
   || Btype.is_constr_row ~allow_ident:true d.expected.expanded

let is_structural d =
  let open Errortrace in
  let structural d = same_path d.ty d.expanded in
  Errortrace.(structural d.got && structural d.expected)


(** Flatten the trace and remove elements that are always discarded
    during printing *)


let wip: type a b. (a,b) Errortrace.explanation -> bool =
  let open Errortrace in function

    | Out_of_scope_univar -> true
    | Type_constructor_arity_mismatch -> true
    | _ -> false

let transparent: type a b. (a,b) Errortrace.explanation -> bool =
  let open Errortrace in function
    | Decl _ -> true
    | Incompatible _ -> true
    | Kind_mismatch -> true
    | Type_constructor_mismatch _ -> true
    | Var_mismatch _ -> true
    | _ -> false


let clean_trace f tr empty_expl =
  let rec clean f expl = function
    | [] -> []
    | [a] ->
        let a = f a in
        if is_row_constr a || (is_structural a && not empty_expl) then []
        else [a]
    | a :: (_ :: _ as q) ->
        let a = f a in
        if is_row_constr a || is_structural a then clean f expl q
        else a :: clean f expl q
  in
  match tr with
  | [] -> []
  | a :: q ->
      let f = Errortrace.map_diff f in
      f a :: clean f empty_expl q

let rec partition_subtrace main shadow = function
  | a :: q ->
      if is_row_constr a then partition_subtrace main shadow q
      else if is_structural a then partition_subtrace main (Some a) q
      else partition_subtrace (a::main) shadow q
  | [] -> List.rev main, shadow

let highlight (type a) shadow (expl: (_,a) Errortrace.explanation list) =
  let map f d = Errortrace.map_diff (fun x -> Some (f x)) d in
  let both_side x = { Errortrace.got = Some x; expected = Some x } in
  match shadow with
  | Some x -> map (fun e -> Out_type.Highlighted_type e.Errortrace.ty) x
  | None ->
    match expl with
    | [Errortrace.Moregen_occur d | Errortrace.Univar_mismatch {diff=d}] ->
        map (fun ty -> Out_type.Highlighted_type ty) d
    | [Errortrace.Type_constructor_mismatch diff] ->
        map (function
            | Errortrace.Constructor_path_mismatch p ->
                Out_type.Highlighted_path p
            | Errortrace.Other_mismatch ty ->
                Out_type.Highlighted_type ty)
          diff
    | [Errortrace.Escape { kind = Constructor p|Module_type p; _ }] ->
       both_side (Out_type.Highlighted_path p)
    | [Errortrace.Escape { kind = Equation e; _ }] ->
       both_side (Out_type.Highlighted_type e.Errortrace.expanded)
    | [Errortrace.Rec_occur (l,_)] ->
        both_side (Out_type.Highlighted_type l)
    | [Errortrace.Incompatible d] ->
        map (fun x -> Out_type.Highlighted_type x) d
    | [Errortrace.Var_mismatch d] ->
        map (fun x -> Out_type.Highlighted_type x) d
    | _ -> { Errortrace.got = None; expected=None }

let rec split_last =
  let open Errortrace in
  function
  | [] -> [], [], None
  | [a] | [a; { explanation = Escape { kind=Constraint; _ }; _ }] ->
      let sub, last = partition_subtrace [] None a.subtrace in
      [sub], [a.explanation], last
  | [a;b] when wip b.explanation || transparent b.explanation ->
      let sub, last = partition_subtrace [] None a.subtrace in
      [sub], [a.explanation], last
  | [{explanation=Incompatible_fields _ as f; subtrace=ftr};
     {explanation= (Escape {kind=Univ _; _} | Univar_mismatch _) as e;
      subtrace = etr}
    ] ->
      let sub, last = partition_subtrace [] None etr in
      [ftr; sub], [f;e], last
  | a :: q ->
      let l, last, last_trace = split_last q in
      a.Errortrace.subtrace :: l, last, last_trace

let simplify_trace f t =
  let open Errortrace in
  let intermediary, last_explanation, last_trace = split_last t.explanations in
  let diff = List.concat (t.context :: intermediary) in
  let keep_last =
    List.for_all (fun x -> wip x || transparent x) last_explanation
  in
  clean_trace f diff keep_last, last_explanation,
  highlight last_trace last_explanation

let may_prepare_expansion compact (Errortrace.{ty; expanded} as ty_exp) =
  match Types.get_desc expanded with
    Tvariant _ | Tobject _ when compact ->
      Variable_names.reserve ty; Errortrace.{ty; expanded = ty}
  | _ -> prepare_expansion ty_exp

let print_path p =
  Fmt.dprintf "%a" !Oprint.out_ident (namespaced_tree_of_path Type p)

let print_tag ppf s = Style.inline_code ppf ("`" ^ s)

let print_tags ppf tags  =
  Fmt.(pp_print_list ~pp_sep:comma) print_tag ppf tags

let is_unit env ty =
  match Types.get_desc (Ctype.expand_head env ty) with
  | Tconstr (p, _, _) -> Path.same p Predef.path_unit
  | _ -> false

let unifiable env ty1 ty2 =
  let snap = Btype.snapshot () in
  let res =
    try Ctype.unify env ty1 ty2; true
    with Ctype.Unify _ -> false
  in
  Btype.backtrack snap;
  res

let explainf fmt = kdoc_printf (fun x -> Some x) fmt

let explainl fmt = kdoc_printf (fun x -> [x]) fmt

let explanation_diff env t3 t4 =
  match Types.get_desc t3, Types.get_desc t4 with
  | Tarrow (_, ty1, ty2, _), _
    when is_unit env ty1 && unifiable env ty2 t4 ->
      explainl
        "@,@[@{<hint>Hint@}: Did you forget to provide %a as argument?@]"
        Style.inline_code "()"
  | _, Tarrow (_, ty1, ty2, _)
    when is_unit env ty1 && unifiable env t3 ty2 ->
      explainl
        "@,@[@{<hint>Hint@}: Did you forget to wrap the expression using \
         %a?@]"
        Style.inline_code "fun () ->"
  | _ -> []

let explain_fixed_row_case = function
  | Errortrace.Cannot_be_closed -> doc_printf "it cannot be closed"
  | Errortrace.Cannot_add_tags tags ->
      doc_printf "it may not allow the tag(s) %a"
        print_tags tags

let pp_path ppf p =
  Style.as_inline_code Printtyp.Doc.path ppf p

let explain_fixed_row pos expl = match expl with
  | Types.Fixed_private ->
    doc_printf "The %a variant type is private" Errortrace.print_pos pos
  | Types.Univar x ->
    Variable_names.reserve x;
    doc_printf "The %a variant type is bound to the universal type variable %a"
      Errortrace.print_pos pos
      (Style.as_inline_code type_expr_with_reserved_names) x
  | Types.Reified p ->
    doc_printf "The %a variant type is bound to %a"
      Errortrace.print_pos pos
      (Style.as_inline_code
         (fun ppf p ->
           Internal_names.add p;
           print_path p ppf))
      p
  | Types.Rigid -> Format_doc.Doc.empty

let explain_variant (type variety) : variety Errortrace.variant -> _ = function
  (* Common *)
  | Errortrace.Incompatible_types_for s ->
      explainf "@,Types for tag %a are incompatible"
        print_tag s
  (* Unification *)
  | Errortrace.No_intersection ->
      explainf "@,These two variant types have no intersection"
  | Errortrace.No_tags(pos,fields) ->
      explainf
        "@,@[The %a variant type does not allow tag(s)@ @[<hov>%a@]@]"
        Errortrace.print_pos pos
        print_tags (List.map fst fields)
  | Errortrace.Fixed_row (pos,
                          k,
                          (Univar _ | Reified _ | Fixed_private as e)) ->
      explainf "@,@[%a,@ %a@]" pp_doc (explain_fixed_row pos e)
          pp_doc (explain_fixed_row_case k)
  | Errortrace.Fixed_row (_,_, Rigid) -> explainf "This case never happens"
  (* Equality & Moregen *)
  | Errortrace.Presence_not_guaranteed_for (pos, s) ->
      explainf
        "@,@[The tag %a is guaranteed to be present in the %a variant type,\
         @ but not in the %a@]"
        print_tag s
        Errortrace.print_pos (Errortrace.swap_position pos)
        Errortrace.print_pos pos
  | Errortrace.Openness pos ->
      explainf "@,The %a variant type is open and the %a is not"
        Errortrace.print_pos pos
        Errortrace.print_pos (Errortrace.swap_position pos)
  | Errortrace.Arity_mismatch tag ->
      explainf
        "@,The arity of the %a tag is mismatched"
        Style.inline_code tag


  | Errortrace.Invalid_conjunction ->
      explainf "@,TODO Invalid conjunction"

let explain_escape pre = function
  | Errortrace.Univ u ->
      Variable_names.reserve u;
      explainf "%a@,The universal variable %a would escape its scope"
          pp_doc pre
          (Style.as_inline_code type_expr_with_reserved_names) u
  | Errortrace.Constructor p ->
      explainf
        "%a@,@[The type constructor@;<1 2>%a@ would escape its scope@]"
        pp_doc pre pp_path p
  | Errortrace.Module_type p ->
      explainf
        "%a@,@[The module type@;<1 2>%a@ would escape its scope@]"
        pp_doc pre pp_path p
  | Errortrace.Equation Errortrace.{ty = _; expanded = t} ->
      Variable_names.reserve t;
      explainf "%a@ @[<hov>This instance of %a is ambiguous:@ %s@]"
        pp_doc pre
        (Style.as_inline_code type_expr_with_reserved_names) t
        "it would escape the scope of its equation"
  | Errortrace.Self ->
      explainf "%a@,Self type cannot escape its class" pp_doc pre
  | Errortrace.Constraint -> None

let explain_object (type variety) : variety Errortrace.obj -> _ = function
  | Errortrace.Missing_field (pos,f) ->
      explainf "@,@[The %a object type has no method %a@]"
        Errortrace.print_pos pos Style.inline_code f
  | Errortrace.Abstract_row pos ->
      explainf
        "@,@[The %a object type has an abstract row, it cannot be closed@]"
        Errortrace.print_pos pos
  | Errortrace.Self_cannot_be_closed ->
      explainf "@,Self type cannot be unified with a closed object type"

let explain_incompatible_fields name (diff: Types.type_expr Errortrace.diff) =
  Variable_names.reserve diff.got;
  Variable_names.reserve diff.expected;
  doc_printf "@,@[The method %a has type@ %a,@ \
  but the expected method type was@ %a@]"
    Style.inline_code name
    (Style.as_inline_code type_expr_with_reserved_names) diff.got
    (Style.as_inline_code type_expr_with_reserved_names) diff.expected


let explain_label_mismatch ~missing_label_msg  {Errortrace.got;expected} =
  let quoted_label ppf l = Style.inline_code ppf (Asttypes.string_of_label l) in
  match got, expected with
  | Asttypes.Nolabel, Asttypes.(Labelled _ | Optional _ )  ->
      doc_printf "@,@[A label@ %a@ was expected@]"
        quoted_label expected
  | Asttypes.(Labelled _|Optional _), Asttypes.Nolabel  ->
      doc_printf missing_label_msg
        quoted_label got
 | Asttypes.Labelled g, Asttypes.Optional e when g = e ->
      doc_printf
        "@,@[The label@ %a@ was expected to be optional@]"
        quoted_label got
  | Asttypes.Optional g, Asttypes.Labelled e when g = e ->
      doc_printf
        "@,@[The label@ %a@ was expected to not be optional@]"
        quoted_label got
  | Asttypes.(Labelled _ | Optional _), Asttypes.(Labelled _ | Optional _) ->
      doc_printf "@,@[Labels %a@ and@ %a do not match@]"
        quoted_label got
        quoted_label expected
  | Asttypes.Nolabel, Asttypes.Nolabel ->
      (* Two empty labels cannot be mismatched*)
      assert false


let explain_first_class_module = function
  | Errortrace.Package_cannot_scrape p ->
      explainf "@,@[The module alias %a could not be expanded@]"
        pp_path p
  | Errortrace.Package_inclusion pr ->
      Some(doc_printf "@,@[%a@]" Fmt.pp_doc pr)
  | Errortrace.Package_coercion pr ->
      Some(doc_printf "@,@[%a@]" Fmt.pp_doc pr)
  | Errortrace.Constraint_on_missing_type (position,name) ->
      let name = String.concat "." name in
      let expl = match position with
        | First ->
            doc_printf "@,@[There is no type %a in the first module type.@]"
              Style.inline_code name
        | Second ->
            doc_printf "@,@[There is no type %a in the second module type.@]"
              Style.inline_code name
      in
      Some expl
  | Errortrace.Constraint_with_deps (position,name) ->
      let name = String.concat "." name in
      let expl = match position with
        | First ->
            doc_printf
              "@,@[The type %a depends on internal types@ in@ the@ \
               first@ module@ type.@]"
              Style.inline_code name
        | Second ->
            doc_printf
              "@,@[The type %a depends on internal types@ in@ the@ \
               second@ module@ type.@]"
              Style.inline_code name
      in
      Some expl
  | Errortrace.Constraint_on_mismatched_type {pos; decl; lhs }  ->
      let name = String.concat "." lhs in
      let id = Ident.create_local name in
      let expl = match pos with
        | First ->
            doc_printf
              "@,@[The constraint on %a in the second module type@ \
               is not compatible@ with the declaration of@;<1 2>@[%a@]@ in \
               the first module type.@]"
              Style.inline_code name
              (Printtyp.Doc.type_declaration id) decl
        | Second ->
            doc_printf
              "@,@[The constraint on %a in the first module type@ \
               is not compatible@ with the declaration of@;<1 2>@[%a@]@ in \
               the second module type.@]"
              Style.inline_code name
              (Printtyp.Doc.type_declaration id) decl
      in
      Some expl

let rec last_opt = function
  | [] -> None
  | _ :: q -> last_opt q

let explanation (type variety) intro
    (e : (Errortrace.expanded_type,variety) Errortrace.explanation)
    =
    match e with
    | Errortrace.Escape {kind; context} ->
        let pre =
          match context with
          | Some ctx ->
              Variable_names.reserve ctx;
              doc_printf "@[%a@;<1 2>%a@]" pp_doc intro
                (Style.as_inline_code type_expr_with_reserved_names) ctx
          | _ -> Format_doc.Doc.empty
        in
        explain_escape pre kind
    | Errortrace.Incompatible_fields { name; diff} ->
        Some(explain_incompatible_fields name diff)
    | Errortrace.Function_label_mismatch diff ->
        let missing_label_msg =
          format_of_string
            "@,@[The first argument is labeled@ %a,@ \
             but an unlabeled argument was expected@]"
        in
        Some(explain_label_mismatch ~missing_label_msg diff)
    | Errortrace.Tuple_label_mismatch diff ->
        let ast_label = function
          | None -> Asttypes.Nolabel
          | Some x -> Asttypes.Labelled x
        in
        let diff = Errortrace.map_diff ast_label diff in
        let missing_label_msg =
          format_of_string
            "@,@[The first tuple element is labeled@ %a,@ \
             but an unlabeled element was expected@]"
        in
        Some(explain_label_mismatch ~missing_label_msg diff)
    | Errortrace.Variant v ->
        explain_variant v
    | Errortrace.Obj o ->
        explain_object o
    | Errortrace.First_class_module fm ->
        explain_first_class_module fm
    | Errortrace.Rec_occur(x,y) ->
        add_type_to_preparation x;
        add_type_to_preparation y;
        begin match Types.get_desc x with
        | Tvar _ | Tunivar _  ->
            Some(
              doc_printf "@,@[<hov>The type variable %a occurs inside@ %a@]"
                (Style.as_inline_code prepared_type_expr) x
                (Style.as_inline_code prepared_type_expr) y
            )
        | _ ->
            (* We had a delayed unification of the type variable with
               a non-variable after the occur check. *)
            Some Format_doc.Doc.empty
            (* There is no need to search further for an explanation, but
               we don't want to print a message of the form:
               {[ The type int occurs inside int list -> 'a |}
            *)
        end
    | Univar_mismatch { diff; order} ->
        add_type_to_preparation diff.got;
        add_type_to_preparation diff.expected;
        let more = match order with
          | Equal ->  Fmt.Doc.empty
          | Less ->
              Fmt.doc_printf
                "@ The first type variable %a was introduced in@ an@ earlier@ \
                 universal@ quantification."
                (Style.as_inline_code prepared_type_expr) diff.got
          | More ->
              Fmt.doc_printf
                "@ The second type variable %a was introduced in@ an@ earlier@ \
                 universal@ quantification."
                (Style.as_inline_code prepared_type_expr) diff.expected
        in
        Some (doc_printf
                "@,@[The universal variables@ %a and@ %a@ are distinct.%a@]"
                (Style.as_inline_code prepared_type_expr) diff.got
                (Style.as_inline_code prepared_type_expr) diff.expected
                pp_doc more
             )

    | Errortrace.Decl GADT_mismatched_return_type -> None
    | Errortrace.Parameter_mismatch (Bound_multiple_times (ty1,ty2,ty3)) ->
        add_type_to_preparation ty1;
        add_type_to_preparation ty2;
        explainf
          "@,@[The distinct type parameters@ %a@ and@ %a@ cannot@ be@ \
           both@ an@ instance@ of %a.@]"
          (Style.as_inline_code prepared_type_expr) ty1
          (Style.as_inline_code prepared_type_expr) ty2
          (Style.as_inline_code prepared_type_expr) ty3
    | Errortrace.Parameter_mismatch (Not_a_variable_param (ty1,ty2)) ->
        if not (Btype.is_Tvar ty1) then None else begin
        add_type_to_preparation ty1;
        add_type_to_preparation ty2;
        explainf
            "@,@[The parameter %a is not an instance of %a.@]"
            (Style.as_inline_code prepared_type_expr) ty1
            (Style.as_inline_code prepared_type_expr) ty2
      end
    | Errortrace.Univar_quantification_mismatch l ->
        let qp ppf x = Style.as_inline_code prepared_type_expr ppf x in
        let pp ppf (kind,ty) =
          add_type_to_preparation ty;
          match kind with
          | Errortrace.Already_bound ->
              begin match Types.get_desc ty with
              | Tunivar (Some name) ->
                  Fmt.fprintf ppf
                    "@,@[The universal variable %a would be paired@ to@ \
                     multiple@ distinct@ universal@ type@ variables.@]"
                    Style.inline_code name
              | _ -> ()
              end
          | Errortrace.Non_generic ->
              Fmt.fprintf ppf
                "@,@[The type variable %a is not generalizable@ to@ an@ \
                 universal@ type variable.@]"
                qp ty
          | Errortrace.Non_universal_row ->
              Fmt.fprintf ppf
                "@,@[The type %a is constrained and cannot be generalized@]"
                qp ty
          | Errortrace.Not_a_variable ->
              Fmt.fprintf ppf
                "@[The type %a is not a type variable@]"
                qp ty
        in

        explainf "%a"
        Fmt.(pp_print_list ~pp_sep:(fun _ppf () -> ()) pp) l
    | Errortrace.Moregen_occur diff ->
      add_type_to_preparation diff.got;
      add_type_to_preparation diff.expected;
      explainf
        "@,@[The non generic type variable@ %a@ \
           is not compatible with the universal type variable %a.@]"
        (Style.as_inline_code prepared_type_expr) diff.got
        (Style.as_inline_code prepared_type_expr) diff.expected

    (* Transparent explanation *)
    | Errortrace.Type_constructor_arity_mismatch -> None
    | Errortrace.Type_constructor_mismatch _ -> None
    | Errortrace.Incompatible _ -> None
    | Errortrace.Out_of_scope_univar ->
        assert false
    | Errortrace.Kind_mismatch -> None
    | Errortrace.Var_mismatch _ -> None

let explanations intro env last_diff l =
  match List.filter_map (explanation intro) l with
  | [] ->
    begin match last_diff with
    | None -> []
    | Some d ->
        let open Errortrace in
        explanation_diff env (d.got.expanded) (d.expected.expanded)
    end
  | l -> l

let warn_on_missing_def env ppf t =
  match Types.get_desc t with
  | Tconstr (p,_,_) ->
    begin match Env.find_type p env with
    | exception Not_found ->
        fprintf ppf
          "@,@[<hov>Type %a is abstract because@ no corresponding\
           @ cmi file@ was found@ in path.@]" pp_path p
    | { type_manifest = Some _; _ } -> ()
    | { type_manifest = None; _ } as decl ->
        match Btype.type_origin decl with
        | Rec_check_regularity ->
            fprintf ppf
              "@,@[<hov>Type %a was considered abstract@ when checking\
               @ constraints@ in this@ recursive type definition.@]"
              pp_path p
        | Definition | Existential _ -> ()
      end
  | _ -> ()

let prepare_expansion_head empty_tr d =
      Errortrace.map_diff (may_prepare_expansion empty_tr) d

let warn_on_missing_defs env ppf d =
      warn_on_missing_def env ppf Errortrace.(d.got.ty);
      warn_on_missing_def env ppf Errortrace.(d.expected.ty)

(* [subst] comes out of equality, and is [[]] otherwise *)
let error trace_format mode subst env tr txt1 ppf txt2 ty_expect_explanation =
  reset ();
  (* We want to substitute in the opposite order from [Eqtype] *)
  Variable_names.add_subst (List.map (fun (ty1,ty2) -> ty2,ty1) subst);
  let last_diff = Errortrace.narrowest_diff tr in
  let tr, root_explanation, tr_explanation =
    simplify_trace
      (fun ty_exp ->
         Errortrace.{ty_exp with expanded = hide_variant_name ty_exp.expanded})
      tr
  in
  with_labels (not !Clflags.classic) (fun () ->
      let tr =
        match tr with
        | [] -> []
        | head :: tr ->
            prepare_expansion_head (tr=[]) head
            :: List.map (Errortrace.map_diff prepare_expansion) tr
      in
      let ahead_tr = look_ahead tr tr_explanation in
      let tr = trees_of_trace mode ahead_tr tr in
      let mis = explanations txt1 env last_diff root_explanation in
      let space _ppf () = () in
       fprintf ppf
        "@[<v>%a%a@]"
        (trace
           ~intro:txt1 ~but:txt2 ~more:ty_expect_explanation
           ~but':(incompatibility_phrase trace_format)
        ) tr
        (pp_print_list pp_doc ~pp_sep:space) mis;
      if env <> Env.empty
      then Option.iter (warn_on_missing_defs env ppf) last_diff;
       Internal_names.print_explanations env ppf;
       Ident_conflicts.err_print ppf
    )

let report_error trace_format ppf mode env tr
      ?(subst = [])
      ?(type_expected_explanation = Fmt.Doc.empty)
      txt1 txt2 =
  wrap_printing_env ~error:true env (fun () ->
    error trace_format mode subst env tr txt1 ppf txt2 type_expected_explanation
  )

let unification
      ppf env ({trace} : Errortrace.unification_error) =
  report_error Unification ppf Type env
    ?subst:None trace

let equality
      ppf mode env ({subst; trace} : Errortrace.equality_error) =
  report_error Equality ppf mode env  ?type_expected_explanation:None ~subst
    trace

let moregen
      ppf mode env ({trace} : Errortrace.moregen_error) =
  report_error Moregen ppf mode env ?type_expected_explanation:None ?subst:None
    trace

let comparison ppf mode env = function
  | Errortrace.Equality_error error -> equality ppf mode env error
  | Errortrace.Moregen_error  error -> moregen  ppf mode env error

module Subtype = struct
  (* There's a frustrating amount of code duplication between this module and
     the outside code, particularly in [prepare_trace] and [filter_trace].
     Unfortunately, [Subtype] is *just* similar enough to have code duplication,
     while being *just* different enough (it's only [Diff]) for the abstraction
     to be nonobvious.  Someday, perhaps... *)


  let simplify_unification_trace = simplify_trace

  let simplify_subtype_trace f tr =
    let get_diff (Errortrace.Subtype.Diff d) = d in
    clean_trace f (List.map get_diff tr) true, [], []

  let trace fst txt tr etr ppf =
    with_labels (not !Clflags.classic) (fun () ->
      let tr =
        match fst, tr with
        | false, _ :: tr -> tr
        | _, tr -> tr
      in
      let ahead_tr = look_ahead tr etr in
      let tr =
        trees_of_trace Type ahead_tr
        @@ List.map (Errortrace.map_diff prepare_expansion) tr in
      let but = Format_doc.doc_printf "%s" txt in
      trace ~intro:(Format_doc.doc_printf "Type") ~but ~but':txt
        ~more:Format_doc.Doc.empty ppf tr
    )

  let no_explanation = {Errortrace.got = None; expected = None }

  let error
        ppf
        env
        (Errortrace.Subtype.{trace = tr_sub; unification_trace = tr_unif})
        txt1 =
    wrap_printing_env ~error:true env (fun () ->
      reset ();
      let tr_sub, _, _ = simplify_subtype_trace prepare_expansion tr_sub in
      let tr_unif, e_unif, last =
        simplify_unification_trace prepare_expansion tr_unif in
      let space _ppf () = () in
      let mis =
        explanations (doc_printf "Within this type") env
          (last_opt tr_unif) e_unif
      in
      match tr_unif with
      | [] ->
          fprintf ppf "@[<v>%t%a@]"
            (trace true txt1 tr_sub last)
            (pp_print_list pp_doc ~pp_sep:space) mis
      | _ ->
          fprintf ppf "@[<v>%t%t%a%t@]"
            (trace true txt1 tr_sub no_explanation)
            (trace  false "is not compatible with type" tr_unif last)
            (pp_print_list pp_doc ~pp_sep:space) mis
            Ident_conflicts.err_print
      )
end

let subtype = Subtype.error

let quoted_ident ppf t =
  Style.as_inline_code !Oprint.out_ident ppf t

let type_path_expansion ppf = function
  | Same p -> quoted_ident ppf p
  | Diff(p,p') ->
      fprintf ppf "@[<2>%a@ =@ %a@]"
       quoted_ident p
       quoted_ident p'

let trees_of_type_path_expansion (tp,tp') =
  let path_tree = namespaced_tree_of_path Type in
  if Path.same tp tp' then Same(path_tree tp) else
    Diff(path_tree tp, path_tree tp)

let type_path_list ppf l =
  Fmt.pp_print_list ~pp_sep:(fun ppf () -> Fmt.pp_print_break ppf 2 0)
    type_path_expansion ppf l

let ambiguous_type ppf env tp0 tpl txt1 txt2 txt3 =
  wrap_printing_env ~error:true env (fun () ->
    reset ();
    let tp0 = trees_of_type_path_expansion tp0 in
      match tpl with
      [] -> assert false
    | [tp] ->
        fprintf ppf
          "@[%a@;<1 2>%a@ \
             %a@;<1 2>%a\
           @]"
          pp_doc txt1 type_path_expansion (trees_of_type_path_expansion tp)
          pp_doc txt3 type_path_expansion tp0
    | _ ->
        fprintf ppf
          "@[%a@;<1 2>@[<hv>%a@]\
             @ %a@;<1 2>%a\
           @]"
          pp_doc txt2 type_path_list (List.map trees_of_type_path_expansion tpl)
          pp_doc txt3 type_path_expansion tp0)
