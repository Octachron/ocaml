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


let item_fold f acc x  =
  let rec group ~acc  = function
    | Types.Sig_class _ as src :: rem ->
       let ctydecl, tydecl1, tydecl2, rem =
         match rem with
         | cty :: tydecl1 :: tydecl2 :: rem -> cty, tydecl1, tydecl2, rem
         | _ ->  (* a class declaration for [c] is followed by the ghost
                    declarations of class type [c], and types [c] and [#c] *)
            assert false
       in
        let s_elt =
          { src; post_ghosts= [ctydecl; tydecl1; tydecl2]}
        in
        group ~acc:(f acc s_elt) rem
    | Types.Sig_class_type _ as src :: rem ->
       let tydecl1, tydecl2, rem =
         match rem with
         | tydecl1 :: tydecl2 :: rem -> tydecl1, tydecl2, rem
         | _ ->  (* a class type declaration for [ct] is followed by the ghost
                    declarations of types [ct] and [#ct] *)
            assert false
       in
       group
         ~acc:(f acc {src; post_ghosts = [tydecl1; tydecl2]})
         rem
    | Types.(Sig_module _ | Sig_value _ | Sig_type _ | Sig_typext _
            | Sig_modtype _ as src)  :: rem ->
        group ~acc:(f acc {src; post_ghosts=[]}) rem
    | [] -> acc in
  group ~acc x


let recursive_sigitem = function
  | Types.Sig_type(ident, _, rs, _) -> Some({hide=true;ident},rs)
  | Types.Sig_class(ident,_,rs,_)
  | Types.Sig_class_type (ident,_,rs,_)
  | Types.Sig_module(ident, _, _, rs, _) -> Some ({hide=false;ident},rs)
  | Types.(Sig_value _ | Sig_modtype _ | Sig_typext _ )  -> None

type 'a state =
  | In_group of {
      pre:Types.signature_item list;
      ids:bound_ident list;
      group:sig_item list;
      acc:'a
    }
  | Not_in_group of {
      pre:Types.signature_item list;
      acc:'a
    }

let fold f acc x =
  let cons_group pre ids group acc =
    let group = Rec_group(List.rev ids, List.rev group) in
    f acc { pre_ghosts=List.rev pre; group }
  in
  let not_in_group pre acc elt = function
    | Some (id, _) when Btype.is_row_name (Ident.name id.ident) ->
        Not_in_group { pre = elt.src::pre;  acc }
    | None | Some (_, Types.Trec_not) ->
        let sgroup = { pre_ghosts=List.rev pre; group=Not_rec elt } in
        Not_in_group {pre=[]; acc = f acc sgroup }
    | Some (id, Types.(Trec_first | Trec_next) )  ->
        In_group {pre; ids = [id]; group=[elt]; acc }
  in
  let in_group pre ids group acc elt = function
    | Some (id, Types.Trec_next) ->
        In_group {pre; ids=id::ids; group= elt::group; acc}
    | None | Some (_, Types.(Trec_not|Trec_first)) as rec_status ->
        let acc = cons_group pre ids group acc in
        not_in_group [] acc elt rec_status
  in
  let add_item state elt =
      let rec_status = recursive_sigitem elt.src in
      match state with
      | Not_in_group {pre;acc} ->
          not_in_group pre acc elt rec_status
      | In_group { pre; ids; group; acc } ->
          in_group pre ids group acc elt rec_status
  in
  let init = Not_in_group { pre=[]; acc } in
  match item_fold add_item init x with
  | Not_in_group e -> e.acc
  | In_group { pre; group; ids; acc; _ } ->
      cons_group pre ids group acc


let iter f = fold (fun () x -> f x) ()
