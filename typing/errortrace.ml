(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Florian Angeletti, projet Cambium, Inria Paris             *)
(*              Antal Spector-Zabusky, Jane Street, New York              *)
(*                                                                        *)
(*   Copyright 2018 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2021 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Types
open Format_doc

type position = First | Second
type order = Less | Equal | More

let swap_position = function
  | First -> Second
  | Second -> First

let swap_order = function
  | Less -> More
  | Equal -> Equal
  | More -> Less

let print_pos ppf = function
  | First -> fprintf ppf "first"
  | Second -> fprintf ppf "second"

type expanded_type = { ty: type_expr; expanded: type_expr }

let trivial_expansion ty = { ty; expanded = ty }

type 'a diff = { got: 'a; expected: 'a }

let map_diff f r =
  (* ordering is often meaningful when dealing with type_expr *)
  let got = f r.got in
  let expected = f r.expected in
  { got; expected }

let map2_diff f x y =
  (* ordering is often meaningful when dealing with type_expr *)
  let got = f x.got y.got in
  let expected = f x.expected y.expected in
  { got; expected }


let swap_diff x = { got = x.expected; expected = x.got }

type 'a escape_kind =
  | Constructor of Path.t
  | Univ of type_expr
  (* The type_expr argument of [Univ] is always a [Tunivar _],
     we keep a [type_expr] to track renaming in {!Printtyp} *)
  | Self
  | Module_type of Path.t
  | Equation of 'a
  | Constraint

type 'a escape =
  { kind : 'a escape_kind;
    context : type_expr option }

let map_escape f esc =
  {esc with kind = match esc.kind with
     | Equation eq -> Equation (f eq)
     | (Constructor _ | Univ _ | Self | Module_type _ | Constraint) as c -> c}

(* Type indices *)
type unification = private Unification
type comparison  = private Comparison

type fixed_row_case =
  | Cannot_be_closed
  | Cannot_add_tags of string list

type 'variety variant =
  (* Common *)
  | Incompatible_types_for : string -> _ variant
  | No_tags : position * (Asttypes.label * row_field) list -> _ variant
  (* Unification *)
  | No_intersection : unification variant
  | Fixed_row :
      position * fixed_row_case * fixed_explanation -> unification variant
  (* Equality & Moregen *)
  | Presence_not_guaranteed_for : position * string -> comparison variant
  | Openness : position (* Always [Second] for Moregen *) -> comparison variant

  | Arity_mismatch: string -> _ variant
  | Invalid_conjunction: _ variant

type 'variety obj =
  (* Common *)
  | Missing_field : position * string -> _ obj
  | Abstract_row : position -> _ obj
  (* Unification *)
  | Self_cannot_be_closed : unification obj

type first_class_module =
    | Package_cannot_scrape of Path.t
    | Package_inclusion of Format_doc.doc
    | Package_coercion of Format_doc.doc
    | Constraint_on_missing_type of position * string list
    | Constraint_with_deps of position * string list
    | Constraint_on_mismatched_type of {
        pos:position;
        decl:Types.type_declaration;
        lhs:string list
      }

type polyfy_error =
  | Already_bound
  | Non_generic
  | Non_universal_row
  | Not_a_variable

type ('a, 'variety) explanation =
  (* Common *)
  | Variant : 'variety variant -> ('a, 'variety) explanation
  | Obj : 'variety obj -> ('a, 'variety) explanation
  | Escape : 'a escape -> ('a, _) explanation
  | Function_label_mismatch of Asttypes.arg_label diff
  | Tuple_label_mismatch of string option diff
  | Incompatible_fields :
      { name:string; diff: type_expr diff } -> ('a, _) explanation
      (* Could move [Incompatible_fields] into [obj] *)
  | First_class_module: first_class_module -> ('a,_) explanation
  | Univar_mismatch of { order:order; diff:type_expr diff }
  (* Unification & Moregen; included in Equality for simplicity *)
  | Rec_occur : type_expr * type_expr -> ('a, _) explanation

 (* Unification *)
  | Type_constructor_mismatch: (_,_) explanation
  | Type_constructor_arity_mismatch: (_,_) explanation

(* NEW *)
  | Out_of_scope_univar: (_,_) explanation
  | Kind_mismatch: (_,_) explanation

  | Univar_quantification_mismatch:
      (polyfy_error * type_expr) list -> ('a, unification) explanation

  | GADT_mismatched_return_type : ('a, unification) explanation
  | Mismatched_type_variables: ('a, unification) explanation
  | Constructor_arity_mismatch : ('a,'v) explanation
  | Injective_arity_mismatch : ('a,'v) explanation

  | GADTness_mismatch: (_,_) explanation
  | Variant_constructor_mismatch: (_,_) explanation
  | Missing_variant_constructor: (_,_) explanation
  | Inline_record_mismatch: (_,_) explanation

  | Record_field_mismatch: (_,_) explanation

  | Moregen_occur of type_expr diff

  | Type_variable_already_bound: (_,_) explanation

  | Tuple_arity_mismatch

type ('a,'variety) explanation_segment = {
  explanation: ('a,'variety) explanation;
  subtrace: 'a diff list
}
type ('a, 'variety) t = {
  context: 'a diff list;
  explanations: ('a,'variety) explanation_segment list
}

let empty = { context = []; explanations = [] }

let diff diff x = { x with context = diff :: x.context }
let diff2 ~got ~expected x = diff {got;expected} x

let root_explanation explanation = {
  context = [];
  explanations = [ {explanation; subtrace=[]} ]
}
let late_explanation explanation { context; explanations } = {
  context = [];
  explanations = { explanation; subtrace=context } :: explanations
}
let variant v = root_explanation (Variant v)
let escape e = root_explanation (Escape (map_escape trivial_expansion e))
let incompatible_fields ~name ~got ~expected trace =
  late_explanation (Incompatible_fields { name; diff={got; expected} }) trace

let pop_explanation tr = match tr.explanations with
  | a :: q -> Some (a.explanation, { context = a.subtrace; explanations = q })
  | [] -> None

let rec last_opt = function
  | [] -> None
  | [a] -> Some a
  | _ :: (_::_ as q) -> last_opt q
let explanation x = last_opt x.explanations
let narrowest_diff e =
  let rec in_subtraces = function
    | [] -> None
    | a :: q ->
        match last_opt a.subtrace with
        | Some _ as a ->  a
        | None -> in_subtraces q
  in
  match in_subtraces e.explanations with
  | Some _ as x -> x
  | None -> last_opt e.context

type 'variety trace = (type_expr,     'variety) t
type 'variety error = (expanded_type, 'variety) t

let map_elt (type a b variety) f:
  (a, variety) explanation -> (b, variety) explanation = function
  | Escape {kind = Equation x; context} ->
      Escape { kind = Equation (f x); context }
  | Escape {kind = (Univ _ | Self | Constructor _ | Module_type _ | Constraint);
            _}
  | Variant _ | Obj _ | Function_label_mismatch _ | Tuple_label_mismatch _
  | Incompatible_fields _
  | Rec_occur (_, _) | First_class_module _ | Univar_mismatch _
  | Kind_mismatch | Out_of_scope_univar
  | Type_constructor_mismatch | Type_constructor_arity_mismatch
  | Constructor_arity_mismatch | Injective_arity_mismatch | GADTness_mismatch
  | Variant_constructor_mismatch | Missing_variant_constructor
  | Inline_record_mismatch | Record_field_mismatch
  | Type_variable_already_bound | Tuple_arity_mismatch  as x -> x
  | GADT_mismatched_return_type as x -> x
  | Mismatched_type_variables as x -> x
  | Univar_quantification_mismatch x ->
    Univar_quantification_mismatch x
  | Moregen_occur _ as x -> x

let map_segment f s = {
  explanation = map_elt f s.explanation;
  subtrace = List.map (map_diff f) s.subtrace

}

let map f t = {
  context = List.map (map_diff f) t.context;
  explanations = List.map (map_segment f) t.explanations;
}

let swap_elt (type variety):
  ('a, variety) explanation -> ('a, variety) explanation = function
  | Incompatible_fields { name; diff } ->
    Incompatible_fields { name; diff = swap_diff diff}
  | Obj (Missing_field(pos,s)) -> Obj (Missing_field(swap_position pos,s))
  | Obj (Abstract_row pos) -> Obj (Abstract_row (swap_position pos))
  | Variant (Fixed_row(pos,k,f)) ->
    Variant (Fixed_row(swap_position pos,k,f))
  | Variant (No_tags(pos,f)) ->
    Variant (No_tags(swap_position pos,f))
  | Univar_mismatch d ->
      Univar_mismatch {
        order = swap_order d.order;
        diff = swap_diff d.diff
      }
  | First_class_module (Constraint_on_missing_type (pos,lhs)) ->
    First_class_module (Constraint_on_missing_type (swap_position pos,lhs))
  | First_class_module (Constraint_with_deps (pos,lhs)) ->
    First_class_module (Constraint_with_deps (swap_position pos,lhs))
  | First_class_module (Constraint_on_mismatched_type r) ->
      let c =
        Constraint_on_mismatched_type { r with pos = swap_position r.pos}
      in
      First_class_module c
  | Moregen_occur x -> Moregen_occur (swap_diff x)
  | x -> x

let swap_explanation_segment s = {
  explanation = swap_elt s.explanation;
  subtrace = List.map swap_diff s.subtrace
}
let swap_trace e ={
  context = List.map swap_diff e.context;
  explanations = List.map swap_explanation_segment e.explanations
}

type unification_error = { trace : unification error } [@@unboxed]

type equality_error =
  { trace : comparison error;
    subst : (type_expr * type_expr) list }

type moregen_error = { trace : comparison error } [@@unboxed]

let nonempty e = e.context <> [] || e.explanations <> []

let unification_error ~trace : unification_error =
  assert (nonempty trace);
  { trace }

let equality_error ~trace ~subst : equality_error =
    assert (nonempty trace);
    { trace; subst }

let moregen_error ~trace : moregen_error =
  assert (nonempty trace);
  { trace }

type comparison_error =
  | Equality_error of equality_error
  | Moregen_error  of moregen_error

let swap_unification_error ({trace} : unification_error) =
  ({trace = swap_trace trace} : unification_error)

module Subtype = struct
  type 'a elt =
    | Diff of 'a diff

  type 'a t = 'a elt list

  type trace       = type_expr t
  type error_trace = expanded_type t

  type unification_error_trace = unification error (** To avoid shadowing *)

  type nonrec error =
    { trace             : error_trace
    ; unification_trace : unification error }

  let error ~trace ~unification_trace =
  assert (trace <> []);
  let unification_trace =
    match unification_trace.context with
    | _ :: q -> { unification_trace with context = q }
    | [] -> unification_trace
  in
  { trace; unification_trace }

  let map_elt f = function
    | Diff x -> Diff (map_diff f x)

  let map f t = List.map (map_elt f) t
end
