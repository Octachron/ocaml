(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*  Florian Angeletti, projet Cambium, Inria Paris                        *)
(*                                                                        *)
(*   Copyright 2021 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Fold on a signature by syntactic group of items *)

(** Classes and class types generate ghosts signature items, we group them
    together before printing *)
type sig_item =
  {
    src: Types.signature_item;
    post_ghosts: Types.signature_item list
    (** ghost classes types are post-declared *);
  }

type 'ident core_rec_group =
  | Not_rec of sig_item
  | Rec_group of ('ident list * sig_item list)

(** Private row types are manifested as a sequence of definitions
    preceding a recursive group, we collect them and separate them from the
    syntatic recursive group. *)
type 'ident gen_rec_group =
  { pre_ghosts: Types.signature_item list; group:'ident core_rec_group }

(** Some identifiers may require hiding when printing *)
type bound_ident = { hide:bool; ident:Ident.t }

type rec_group = bound_ident gen_rec_group


let take n seq =
  let rec aux l n seq =
    if n = 0 then List.rev l, seq else
      match seq () with
      | Seq.Nil -> assert false
      | Seq.Cons(x,next) ->
          aux (x::l) (n-1) next
  in
  aux [] n seq

let rec item_seq seq () =
  match seq () with
  | Seq.Nil -> Seq.Nil
  | Seq.Cons(x, seq) ->
    match x with
    | Types.Sig_class _ as src ->
        let ghosts, seq = take 3 seq in
        (* a class declaration for [c] is followed by the ghost
           declarations of class type [c], and types [c] and [#c] *)
        Seq.Cons({ src; post_ghosts=ghosts}, item_seq seq)
    | Types.Sig_class_type _ as src ->
        let ghosts, seq = take 2 seq in
        (* a class type declaration for [ct] is followed by the ghost
           declarations of types [ct] and [#ct] *)
        Seq.Cons({src; post_ghosts = ghosts}, item_seq seq)
    | Types.(Sig_module _ | Sig_value _ | Sig_type _ | Sig_typext _
            | Sig_modtype _ as src) ->
        Seq.Cons({src; post_ghosts=[]}, item_seq seq)


let recursive_sigitem = function
  | Types.Sig_type(ident, _, rs, _) -> Some({hide=true;ident},rs)
  | Types.Sig_class(ident,_,rs,_)
  | Types.Sig_class_type (ident,_,rs,_)
  | Types.Sig_module(ident, _, _, rs, _) -> Some ({hide=false;ident},rs)
  | Types.(Sig_value _ | Sig_modtype _ | Sig_typext _ )  -> None

let group_seq x =
  let cons_group pre ids group seq =
    let group = Rec_group(List.rev ids, List.rev group) in
    Seq.Cons({ pre_ghosts=List.rev pre; group }, seq)
  in
  let rec not_in_group pre seq () = match seq () with
    | Seq.Nil ->
        assert (pre=[]);
        Seq.Nil
    | Seq.Cons(elt, seq) ->
        match recursive_sigitem elt.src with
        | Some (id, _) when Btype.is_row_name (Ident.name id.ident) ->
            not_in_group (elt.src::pre) seq ()
        | None | Some (_, Types.Trec_not) ->
            let sgroup = { pre_ghosts=List.rev pre; group=Not_rec elt } in
            Seq.Cons(sgroup, not_in_group [] seq)
        | Some (id, Types.(Trec_first | Trec_next) )  ->
            in_group ~pre ~ids:[id] ~group:[elt] seq ()
  and in_group ~pre ~ids ~group seq () = match seq () with
    | Seq.Nil ->
        cons_group pre ids group (fun () -> Seq.Nil)
    | Seq.Cons(elt,next) ->
        match recursive_sigitem elt.src with
        | Some (id, Types.Trec_next) ->
            in_group ~pre ~ids:(id::ids) ~group:(elt::group) next ()
        | None | Some (_, Types.(Trec_not|Trec_first)) ->
            cons_group pre ids group (not_in_group [] seq)
  in
  not_in_group [] x



let group l = l |> List.to_seq |> item_seq |> group_seq
let iter f l = Seq.iter f (group l)
