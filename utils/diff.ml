(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Gabriel Radanne, projet Cambium, INRIA Rocquencourt        *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type ('a, 'b, 'c, 'd) config = {
  deletion : 'a -> int ;
  insertion : 'b -> int ;
  change : 'c -> int ;
  keep : 'd -> int ;
}
    

type change =
  | Insert
  | Delete
  | Change
  | Keep

type patch = change list

let min_assoc_list l =
  let rec aux m0 res0 = function
    | [] -> m0, res0
    | (m', res') :: t ->
        let m'', res'' = if m0 <= m' then m0,res0 else m',res' in
        aux m'' res'' t
  in
  match l with
  | [] -> invalid_arg "min_assoc_list: empty list"
  | (m,res)::l -> aux m res l

let compute_matrix weight cutoff test a1 a2 =
  let l1 = Array.length a1 in
  let l2 = Array.length a2 in
  let cutoff =
    (* using max_int for cutoff would cause overflows in (i + cutoff + 1);
       we bring it back to the (max la lb) worstcase *)
    min (max l1 l2) cutoff in
  if abs (l1 - l2) > cutoff then None
  else begin
    (* initialize with 'cutoff + 1' so that not-yet-written-to cases have
       the worst possible cost; this is useful when computing the cost of
       a case just at the boundary of the cutoff diagonal. *)
    let m = Array.make_matrix (l1 + 1) (l2 + 1) (cutoff + 1) in
    let results = Hashtbl.create cutoff in
    m.(0).(0) <- 0;
    for i = 1 to l1 do
      m.(i).(0) <- weight.deletion a1.(i-1) + m.(i-1).(0);
    done;
    for j = 1 to l2 do
      m.(0).(j) <- weight.insertion a2.(j-1) + m.(0).(j-1);
    done;
    for i = 1 to l1 do
      for j = max 1 (i - cutoff - 1) to min l2 (i + cutoff + 1) do
        let cost, diff = match test a1.(i-1) a2.(j-1) with
          | Ok ok -> weight.keep ok, Keep
          | Error err -> weight.change err, Change
        in
        let propositions = [
          weight.deletion a1.(i-1) + m.(i-1).(j), Delete ;
          weight.insertion a2.(j-1) + m.(i).(j-1), Insert ;
          cost + m.(i-1).(j-1), diff ;
        ]
        in
        let best, res = min_assoc_list propositions in
        Hashtbl.add results (i-1,j-1) res;
        m.(i).(j) <- best
      done;
    done;
    (* m |> Array.iter (fun a ->
     *     Array.iter (fun x -> Format.eprintf "%i" x) a;
     *     Format.eprintf "@.");
     * Format.eprintf "@." ; *)
    Some results
  end

let construct_patch a1 a2 results =
  let rec aux acc (i, j) =
    if i > 0 && j > 0 then
      let d = Hashtbl.find results (i-1, j-1) in
      let next = match d with
        | Keep | Change -> (i-1, j-1)
        | Delete -> (i-1, j)
        | Insert -> (i, j-1)
      in
      aux (d::acc) next
    else if j > 0 && i = 0 then
      aux (Insert::acc) (i, j-1)
    else if i > 0 && j = 0 then
      aux (Delete::acc) (i-1, j)
    else
      acc
  in
  aux [] (Array.length a1, Array.length a2)

let diff weight cutoff test a1 a2 =
  Option.map (construct_patch a1 a2) (compute_matrix weight cutoff test a1 a2) 
