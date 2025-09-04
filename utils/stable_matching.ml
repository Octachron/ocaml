(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                Malo Monin, projet Cambium, Inria Paris                 *)
(*                                                                        *)
(*   Copyright 2024 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)


type cost_model = {
  insertion:int;
  deletion:int;
  substitution:int;
}
type layer = { left_candidates: int list; pref:int }

module Int_map = Map.Make(Int)


module Trie = struct

 module Uchar_map = Map.Make(Uchar)
 type 'data t = {
    leaf : 'data option;
    strict_suffixes : 'data path Uchar_map.t;
  }
  and 'data path = { path:string; subtree: 'data t }


  let empty = { leaf = None; strict_suffixes = Uchar_map.empty }
  let leaf x = { leaf = Some x; strict_suffixes = Uchar_map.empty}

  type diff =
    | At_pos of {offset:int; l:Uchar.t option;  r:Uchar.t }
    | Same of int

  let rec find_difference ~llen ~lpos l ~rlen ~offset r =
    match lpos = llen, offset = rlen with
    | true, true -> Same lpos
    | true, false ->
        let r = Uchar.utf_decode_uchar (String.get_utf_8_uchar r offset) in
        At_pos { offset; l = None; r }
    | false, true -> Same lpos
    | false, false ->
        let ld = String.get_utf_8_uchar l lpos in
        let rd = String.get_utf_8_uchar r offset in
        if ld <> rd then
          let l = Some (Uchar.utf_decode_uchar ld) in
          At_pos { offset; l; r = Uchar.utf_decode_uchar rd }
        else
          let diff = Uchar.utf_decode_length ld in
          find_difference
            ~llen ~lpos:(lpos+diff) l
            ~rlen ~offset:(offset+diff) r

  let comma ppf () = Format.fprintf ppf ",@ "
  let pp_uchar ppf u =
    let u =
      let b = Buffer.create 2 in Buffer.add_utf_8_uchar b u; Buffer.contents b
    in
    Format.pp_print_string ppf u

  let[@warning "-32"] rec pp_trie pp_leaf ppf m =
    let pp_oleaf ppf = function
      | None -> Format.fprintf ppf "()"
      | Some leaf -> Format.fprintf ppf "(%a)" pp_leaf leaf
    in
    Format.fprintf ppf "@[<v 2>%a [%a]@]" pp_oleaf m.leaf
      (Format.pp_print_seq ~pp_sep:comma @@ pp_binding pp_leaf)
      (Uchar_map.to_seq m.strict_suffixes)
  and[@warning "-32"] pp_binding pp_leaf ppf (u,p) =
    Format.fprintf ppf "(%a)%s %a" pp_uchar u p.path (pp_trie pp_leaf) p.subtree


  let add (s,k) trie =
    let rec aux k len pos s trie =
      if pos = len then { trie with leaf = Some k } else
        let decode = String.get_utf_8_uchar s pos in
        let char = Uchar.utf_decode_uchar decode in
        let diff = Uchar.utf_decode_length decode in
        let new_sub = match Uchar_map.find char trie.strict_suffixes with
          | exception Not_found -> {
              path = String.sub s pos (String.length s - pos);
              subtree = leaf k
            }
          | { path; subtree } ->
              match find_difference ~llen:len ~lpos:(pos+diff) s
                      ~rlen:(String.length path) ~offset:diff path
              with
              | Same lpos -> { path; subtree = aux k len lpos s subtree }
              | At_pos {offset;l;r} ->
                  let common = String.sub path 0 offset in
                  let right =
                    let path =
                      String.sub path offset (String.length path - offset)
                    in
                    Uchar_map.singleton r { path ; subtree }
                  in
                  let subtree = match l with
                    | None -> { leaf = Some k; strict_suffixes = right }
                    | Some l ->
                        let path =
                          let pos = pos + offset in
                          String.sub s pos (String.length s - pos)
                        in
                        let strict_suffixes =
                          Uchar_map.add l {path; subtree=leaf k} right
                        in
                        { leaf = None; strict_suffixes }
                  in
                  { path = common; subtree }
        in
        let strict_suffixes = Uchar_map.add char new_sub trie.strict_suffixes in
        { trie with strict_suffixes }
    in
    let new_trie = aux k (String.length s) 0 s trie in
    Format.eprintf "@[<v 2>%a@ -(%s)>@ %a@]@."
      (pp_trie Format.pp_print_int) trie s
      (pp_trie Format.pp_print_int) new_trie;
    new_trie

  let of_seq s = Seq.fold_left (fun t x -> add x t) empty s


  type column = { char:Uchar.t; score: int Array.t }

  type frontier = { col_minus_1:column; col:column }
  let copy c = { c with score = Array.copy c.score }

  type 'a state = {
    matrix:frontier;
    best_score:int;
    trie: 'a t
  }

  module QState = Pqueue.MakeMinPoly(struct
      type 'a t = 'a state
      let compare x y = compare x.best_score y.best_score
    end)


  let score st = st.matrix.col.score.(Array.length st.matrix.col.score - 1)
  let best_future_score x = Array.fold_left min Int.max_int x.col.score


  let add_leaf result_map state =
    match state.trie.leaf with
    | Some index ->
        Format.eprintf "found leaf %d, score = %d@." index (score state);
        Int_map.add_to_list (score state) index result_map
    | None -> result_map

  let next_char pos s =
    let d = String.get_utf_8_uchar s pos in
    let len = Uchar.utf_decode_length d in
    let u = Uchar.utf_decode_uchar d in
    pos + len, u

  let rec col_edit_distance ~left ~rlen ~rpos ~right
      ~col_minus_1
      ~col
    =
    if rpos >= rlen then { col_minus_1; col } else
    let rpos, rchar = next_char rpos right in
    let l = Array.length col.score in
    for i = l - 1 downto 0 do
      let addition = col.score.(i) + 1 in
      let subst =
        if i = 0 then Int.max_int
        else if rchar = left.(i-1) then col.score.(i-1)
        else 1 + col.score.(i-1) in
      let transpose =
        if i >=2 && rchar = left.(i-2) && col.char = left.(i-1) then
          1 + col_minus_1.score.(i-2)
        else
          Int.max_int
      in
      col_minus_1.score.(i) <- min transpose (min addition subst)
    done;
    for i = 1 to l - 1 do
      col_minus_1.score.(i) <-
        min col_minus_1.score.(i) (1+col_minus_1.score.(i-1))
    done;
    let new_col = { score = col_minus_1.score; char = rchar } in
    col_edit_distance ~left ~rlen ~rpos ~right
    ~col:new_col ~col_minus_1:col


  let rec query_next score word result_map queue =
    match QState.min_elt queue with
    | None -> Format.eprintf "No elements in the queue@."; result_map
    | Some st ->
        if st.best_score > score then result_map
        else begin
          QState.remove_min queue;
          Format.eprintf "Processing children@.";
          query_children score word result_map queue st st.trie.strict_suffixes
        end
    and query_children score word result_map queue st children =
      if Uchar_map.is_empty children then
        (Format.eprintf "No children left@.";
         query_next score word result_map queue
        )
      else
        let u, first = Uchar_map.choose children in
        let rest = Uchar_map.remove u children in
        Format.eprintf "Looking at %s@." first.path;
        let trie = { st.trie with strict_suffixes = rest } in
        let st = { st with trie } in
        let () = QState.add queue st in
        query_path score word result_map queue st first

    and query_path score word result_map queue st p =
      let cols = col_edit_distance
          ~left:word ~rlen:(String.length p.path) ~right:p.path ~rpos:0
          ~col_minus_1:(copy st.matrix.col_minus_1)
          ~col:(copy st.matrix.col)
      in
      let best_score = best_future_score cols in
      let st = { matrix=cols; best_score; trie = p.subtree } in
      let result_map = add_leaf result_map st in
      if best_score <= score then
        query_children score word result_map queue st st.trie.strict_suffixes
      else
        let () = QState.add queue st in
        query_next score word result_map queue

    let uchar_array word =
      let d = Dynarray.create () in
      let pos = ref 0 in
      let len = String.length word in
      while !pos < len do
        let npos, char = next_char !pos word in
        Dynarray.add_last d char;
        pos := npos
      done;
      Dynarray.to_array d

    let rec query rmap cutoff queue name () =
      let next_score =
        match QState.min_elt queue, Int_map.choose_opt rmap with
        | None, None -> None
        | Some {best_score=s; _ }, None | None, Some (s,_) -> Some s
        | Some q, Some (m,_) -> Some (min q.best_score m)
      in
      match next_score with
      | None ->
            Format.eprintf "No children nor result left@.";
            Seq.Nil
      | Some layer ->
            Format.eprintf "Looking at layer=%d@." layer;
            if layer > cutoff then
              Seq.Nil
            else
              let rmap = query_next layer name rmap queue in
              match Int_map.find_opt layer rmap with
              | None | Some [] ->
                  Format.eprintf "Moving to next layer@.";
                  query rmap cutoff queue name ()
              | Some l ->
                  let rmap = Int_map.remove layer rmap in
                  Format.eprintf "%d candidates at distance %d@."
                    (List.length l) layer;
                  Seq.Cons( {left_candidates=l; pref=layer},
                            query rmap cutoff queue name)

    let init name trie =
      let queue = QState.create () in
      let len = Array.length name + 1 in
      let col_minus_1 =
        { char = Uchar.rep; score = Array.make len Int.max_int } in
      let col = { char = Uchar.rep; score = Array.init len Fun.id } in
      QState.add queue {
        best_score=0;
        trie;
        matrix = { col; col_minus_1 }
      };
      queue


    let compute_preference_layers ?max_elements:_ ~cutoff trie name =
      Format.eprintf "@[<v 2> looking for %s in trie:@,%a@]@."
        name
        (pp_trie Format.pp_print_int) trie;
      let uchar_name = uchar_array name in
      query Int_map.empty cutoff (init uchar_name trie) uchar_name

end



type ('a,'v) matches = {
  left : 'a list;
  pairs : ('v * 'v) list;
  right : 'a list;
}


let reverse_matches d =
  {
    right = d.left;
    left = d.right;
    pairs = List.map (fun (right, left) -> (left, right)) d.pairs;
  }

module Item = struct
  type ('v, 'k) t = {
    name: string;
    item: 'v;
    kind: 'k;
  }

  let name f = f.name
  let item f = f.item
  let kind i = i.kind
end
type nonrec ('v,'k) item_matches =  (('v,'k) Item.t, 'v) matches


(** An implementation (in [diff]) of Zoltan Kiraly's "New Algorithm," presented
    in "Linear Time Local Approximation Algorithm for Maximum Stable Marriage":
    https://www.mdpi.com/1999-4893/6/3/471. It computes a 3/2-approximation of
    a maximum stable marriage in linear time (linear in the sum of the lengths
    of the preference lists). *)
module Stable_marriage_diff = struct

  (* This implementation does not use the same semantics as the original paper.
     Below is a conversion from the paper's terms to the implementation's terms:
     - woman: left
     - man: right
     - engaged (woman / man): paired
     - maiden (woman): unpaired
     - active (man): active
     - lad: first phase
     - bachelor: second phase
     - old bachelor: closed
     - uncertain (man): has other choices
     - flighty (woman): has a weak pair *)

  type distance = int

  type dequeue = { front: int list; pre:bool; back:int list }
  let next dq = match dq.front with
    | a :: front -> Some (a, { dq with front })
    | [] -> match List.rev dq.back with
      | [] -> None
      | a :: front -> Some (a, { front; pre=false; back = [] })
  let push_back dq x = { dq with back = x :: dq.back }

  let replace_front x dq = { dq with front = x :: dq.front }



  type left_state =
    | Left_unpaired
    | Left_paired of int * distance

  type right_phase =
    | First
    | Second

  type active_right_state = {
    mutable previous_layers : layer list;
        (** Invariant: this list is not empty in the first phase . *)
    mutable current_layer : dequeue; (** Invariant: this list is not empty. *)
    mutable current_distance : distance;
    mutable paired: bool;
    mutable phase: right_phase;
    mutable next_layers : layer Seq.t;
  }

  type ('a,'b) state =
    { left: 'a array; right: 'b array; mutable reactivated:int list }

  let is_never_paired state j = match state.left.(j) with
    | Left_unpaired -> true
    | _ -> false

  let rec has_alternative_choices state r =
    let cl = r.current_layer in
    if not cl.pre then false else
    match cl.front with
    | a :: b :: q ->
       is_never_paired state b ||
          let current_layer =
            { front = a :: q; pre=true; back = b :: cl.back }
          in
          r.current_layer <- current_layer;
          has_alternative_choices state r
    | [_] | [] -> false

  let rec skip_paired state dq =
    assert dq.pre;
    match next dq with
    | None -> assert false
    | Some (first,others) ->
        if is_never_paired state first then
          first, others
        else skip_paired state (push_back others first)

  let has_weak_pair state j =
    match state.left.(j) with
    | Left_unpaired -> false
    | Left_paired (i, _) ->
        match state.right.(i) with
        | None -> assert false
        | Some r -> has_alternative_choices state r

  let phase state i =
    Option.map (fun x -> x.phase) state.right.(i)

  let prepare_dequeue state { left_candidates=i; pref=d} r =
    let pre, later = List.partition (is_never_paired state) i in
    let dequeue = { front = pre; pre=true; back = later } in
    match state.right.(r) with
    | None -> ()
    | Some r ->
        r.current_distance <- d;
        r.current_layer <- dequeue

  let second_phase state ir r =
    let layers = List.rev r.previous_layers in
    r.previous_layers <- [];
    r.phase <- Second;
    match layers with
    | [] -> assert false
    | layer :: q ->
        prepare_dequeue state layer ir;
        r.next_layers <- List.to_seq q


  let next_layer state ir r = match r.next_layers () with
    | Seq.Nil ->
        begin match r.phase with
        | First -> second_phase state ir r; true
        | Second -> false
        end
    | Seq.Cons(layer, next_layers) ->
        r.previous_layers <- layer :: r.previous_layers;
        r.next_layers <- next_layers;
        prepare_dequeue state layer ir;
        true

  let rec get_left_candidate state ir r =
    assert (r.paired = false);
    if has_alternative_choices state r then
      let f, others = skip_paired state r.current_layer in
      r.current_layer <- others;
      Some f
    else match next r.current_layer with
      | Some (f,others) ->
          r.current_layer <- others;
          Some f
      | None ->
          if next_layer state ir r then get_left_candidate state ir r
          else None

  let rec get_compatible_left_candidate compatibility state ir r =
    match get_left_candidate state ir r with
    | None -> None
    | Some l as c ->
        if compatibility l ir then c
        else
          get_compatible_left_candidate compatibility state ir r

  let reject state i =
    match state.right.(i) with
    | None -> ()
    | Some right ->
    right.paired <- false;
    match next right.current_layer with
    | None -> state.right.(i) <- None
    | Some (f,others) ->
        let dequeue = if others.pre then
            push_back others f
          else others
        in
        right.current_layer <- dequeue;
        state.reactivated <- i :: state.reactivated

  let accepted_proposal state i j d =
    has_weak_pair state j ||
    match state.left.(j) with
    | Left_unpaired -> true
    | Left_paired (i', d') ->
        d < d' ||
        d = d' &&
        match phase state i, phase state i' with
        | Some Second, Some First -> true
        | _ -> false

  let pair state i j d =
    begin match state.right.(i) with
    | None -> ()
    | Some r ->
      r.paired <- true;
      r.current_layer <- replace_front j r.current_layer
    end;
    match state.left.(j) with
    | Left_unpaired -> state.left.(j) <- Left_paired (i, d)
    | Left_paired (i', _) ->
        reject state i';
        state.left.(j) <- Left_paired (i, d)

  let trie_preferences ?max_elements ~cutoff x =
    let name i field = Item.name field, i in
    let left_trie = x |> Array.to_seq |> Seq.mapi name |> Trie.of_seq in
    fun name ->
      Trie.compute_preference_layers
        ~cutoff:(cutoff name)
        ?max_elements
        left_trie
        name

  let d_of_list front = { front; pre=true; back = [] }

  let init_right_state ~preferences right =
    Array.map
      (fun right_field ->
         let name = Item.name right_field in
         let sequence = preferences name in
         match sequence () with
         | Seq.Nil -> None
         | Seq.Cons (layer, tail) ->
             Some {
               paired = false;
               phase = First;
               current_distance = layer.pref;
               current_layer = d_of_list layer.left_candidates;
               previous_layers = [layer];
               next_layers = tail;
             }
      )
      right


  let rec proposals compatibility state i right =
    match get_compatible_left_candidate compatibility state i right with
    | None -> ()
    | Some j ->
        if accepted_proposal state i j right.current_distance then
          pair state i j right.current_distance
        else
          proposals compatibility state i right


  let diff ~preferences ~compatibility left right =
    let n = Array.length left in
    let m = Array.length right in
    let left_state = Array.make n Left_unpaired in
    let right_state = init_right_state ~preferences right in
    let state = { left=left_state; reactivated = []; right=right_state } in
    let rec loop = function
      | [] ->
          begin  match state.reactivated with
          | [] -> ()
          | l -> state.reactivated <- []; loop l
          end
      | i :: l ->
        match state.right.(i) with
          | None -> loop l
          | Some right ->
              proposals compatibility state i right;
              loop l
    in
    loop (List.init m Fun.id);
    let left_final = Seq.zip (Array.to_seq left) (Array.to_seq state.left) in
    let left, pairs = Seq.partition_map (fun (field, status) ->
        match status with
        | Left_unpaired -> Either.Left field
        | Left_paired (i,_) ->
            Either.Right (field, right.(i))
      ) left_final
    in
    {
      left = List.of_seq left;
      right =
        Array.to_seq right
        |> Seq.filteri (fun i _ ->
          match state.right.(i) with
          | Some r -> not r.paired
          | None -> true)
        |> List.of_seq
      ;
      pairs = List.of_seq pairs;
    }

  let diff ~compatibility ~preferences left right =
    if Array.length right >  Array.length left then
      diff
        ~preferences
        ~compatibility:(fun a b -> compatibility b a)
        right left
      |> reverse_matches
    else diff ~preferences ~compatibility left right

end

let (%<%) (x:int option) (y:int option) = match x, y with
  | None , _ -> false
  | _, None -> true
  | Some x, Some y -> x < y

let greedy_matching ~compatibility ~cutoff missings additions =
  let rec list_extract predicate l =
    match l with
    | [] -> None
    | hd :: tl when predicate hd -> Some (hd, tl)
    | hd :: tl -> (
        match list_extract predicate tl with
        | None -> None
        | Some (x, tl') -> Some (x, hd :: tl'))
  in

  let compute_distance expected_field added_field =
    if compatibility (Item.kind added_field) (Item.kind expected_field) then
      let distance =
        let expected_name = Item.name expected_field in
        Misc.edit_distance
          expected_name
          (Item.name added_field)
          (cutoff expected_name)
      in
      distance
    else
      None
  in

  let remaining_added_fields = ref additions in
  let name_changes = ref [] in
  let actually_missing =
    missings
    |> List.filter_map
      (fun missing_field ->
        let missing_id = missing_field in
        let missing_name = Item.name missing_id in
        match
          list_extract
            (fun added_field ->
              (compute_distance missing_field added_field)
                %<% Some (cutoff missing_name))
            !remaining_added_fields
        with
        | None -> Some missing_field
        | Some (added_field, additions) ->
            let name_change = Item.item added_field, Item.item missing_field in
            name_changes := name_change :: !name_changes;
            remaining_added_fields := additions;
            None)
  in
  {
    left = !remaining_added_fields;
    pairs= !name_changes;
    right = actually_missing
  }

let simple_preferences ~cutoff left name =
  let cutoff = 1 + cutoff name in
  let a =
    Array.of_seq
    @@ Seq.filter (fun (_,d) -> d < cutoff)
    @@ Seq.mapi (fun i r ->
        i, String.edit_distance ~limit:cutoff name @@ Item.name r)
    @@ Array.to_seq left in
  let () = Array.sort (fun (_,n) (_,n') -> Int.compare n n') a in
  let rec group_by current acc pos () =
    if pos >= Array.length a then
      match acc with
      | [] -> Seq.Nil
      | _ -> Seq.Cons ({ left_candidates=acc; pref=current }, Seq.empty)
    else
      let x, dist = a.(pos) in
      if dist = current then
        group_by current (x::acc) (pos+1) ()
      else if acc = [] then
        group_by dist [x] (pos+1) ()
      else
        Seq.Cons (
          {left_candidates=acc; pref=current}, group_by dist [x] (pos+1)
        )
  in
  group_by 0 [] 0

module BK_tree_hm = struct
  type 'a t = { root: 'a; name:string; children: (int,'a t) Hashtbl.t  }

  let rec query results stack ~cutoff ~max_dist name t =
    let dist = String.edit_distance name t.name in
    if dist <= cutoff then Hashtbl.add results dist t.root;
    query_children results stack ~cutoff ~max_dist name dist t.children
  and query_children result stack ~cutoff ~max_dist name dist children =
    let children =
      if max_dist = 0 then Option.to_list (Hashtbl.find_opt children dist)
      else
        let left =Hashtbl.find_opt children (dist-max_dist) in
        let right = Hashtbl.find_opt children (dist+max_dist) in
        Option.to_list left @ Option.to_list right
    in
    match children, stack with
    | [], [] -> result
    | [], a :: q -> query result q ~cutoff ~max_dist name a
    | a :: q, _  ->
        let stack = q @ stack in
        query result stack ~cutoff ~max_dist name a

  let rec layer_seq result ~cutoff ~max_dist dist children name () =
    if max_dist > cutoff then Seq.Nil
    else
      let r = query_children result []  ~cutoff ~max_dist name dist children in
      let at_dist = Hashtbl.find_all r max_dist in
      match at_dist with
      | [] -> layer_seq r ~cutoff ~max_dist:(max_dist+1) dist children name ()
      | _ ->
          let next =
            layer_seq r ~cutoff ~max_dist:(max_dist+1) dist children name in
          Seq.Cons( {left_candidates=at_dist; pref=max_dist}, next)

  let layers cutoff (t: int t) name =
    let cutoff = cutoff name in
    let dist = String.edit_distance t.name name in
    let results = Hashtbl.create 17 in
    if dist <= cutoff then Hashtbl.add results dist t.root;
    layer_seq results ~cutoff ~max_dist:0 dist t.children name

  let rec make = function
    | [] -> invalid_arg "Empty lexicon"
    | (root_name,root) :: q ->
        let tbl = Hashtbl.create 4 in
        List.iter (fun (name,_ as x) ->
            let dist = String.edit_distance root_name name in
            let v= Option.value ~default:[] @@ Hashtbl.find_opt tbl dist in
            Hashtbl.replace tbl dist (x::v)
          ) q;
        let children = Hashtbl.of_seq
          @@ Seq.map (fun (k,l) -> k, make l)
          @@ Hashtbl.to_seq tbl
        in
        { root; name=root_name; children }

  let preferences ?max_elements:_ ~cutoff d =
    let d = List.mapi (fun i item -> Item.name item, i) d in
    match d with
    | [] -> Fun.const Seq.empty
    | _ ->
        layers cutoff (make d)
end


module BK_tree_da = struct
  type 'a t = { root: 'a; name:string; children: 'a t option Dynarray.t  }

  let (.?()) da n = if n >= Dynarray.length da || n <= 0 then None else
      Dynarray.get da n


  let (.$()<-) da n x =
    if n < Dynarray.length da then Dynarray.set da n (x::Dynarray.get da n)
    else
      let diff = 1 + n - Dynarray.length da in
      Dynarray.append_seq da Seq.(take diff @@ repeat []);
      Dynarray.set da n [x]

  let rec query results stack ~cutoff ~max_dist name t =
    let dist = String.edit_distance name t.name in
    if dist <= cutoff then Hashtbl.add results dist t.root;
    query_children results stack ~cutoff ~max_dist name dist t.children
  and query_children result stack ~cutoff ~max_dist name dist children =
    let children =
      if max_dist = 0 then Option.to_list children.?(dist)
      else
        let left = children.?(dist-max_dist) in
        let right = children.?(dist+max_dist) in
        Option.to_list left @ Option.to_list right
    in
    match children, stack with
    | [], [] -> result
    | [], a :: q -> query result q ~cutoff ~max_dist name a
    | a :: q, _  ->
        let stack = q @ stack in
        query result stack ~cutoff ~max_dist name a

  let rec layer_seq result ~cutoff ~max_dist dist children name () =
    if max_dist > cutoff then Seq.Nil
    else
      let r = query_children result [] ~cutoff ~max_dist name dist children in
      let at_dist = Hashtbl.find_all r max_dist in
      match at_dist with
      | [] -> layer_seq r ~cutoff ~max_dist:(max_dist+1) dist children name ()
      | _ ->
          let next =
            layer_seq r ~cutoff ~max_dist:(max_dist+1) dist children name in
          Seq.Cons( {left_candidates=at_dist; pref=max_dist}, next)

  let layers cutoff (t: int t) name =
    let cutoff = cutoff name in
    let dist = String.edit_distance t.name name in
    let results = Hashtbl.create 17 in
    if dist <= cutoff then Hashtbl.add results dist t.root;
    layer_seq results ~cutoff ~max_dist:0 dist t.children name

  let rec make = function
    | [] -> invalid_arg "Empty lexicon"
    | (root_name,root) :: q ->
        let da = Dynarray.create () in
        List.iter (fun (name,_ as x) ->
            let dist = String.edit_distance root_name name in
            da.$(dist) <- x
          ) q;
        let children =
          Dynarray.map (function [] -> None | l -> Some(make l)) da in
        { root; name=root_name; children }

  let preferences ?max_elements:_ ~cutoff d =
    let d = List.mapi (fun i item -> Item.name item, i) d in
    match d with
    | [] -> Fun.const Seq.empty
    | _ ->
        layers cutoff (make d)
end



module BK_tree = struct

  type 'a t = { root: 'a; name:string; children: 'a t Int_map.t }

  let rec query results stack ~cutoff ~max_dist name t =
    let dist = String.edit_distance name t.name in
    let results =
      if dist <= cutoff then Int_map.add_to_list dist t.root results
      else results
    in
    query_children results stack ~cutoff ~max_dist name dist t.children
  and query_children result stack ~cutoff ~max_dist name dist children =
    let children =
      if max_dist = 0 then Option.to_list (Int_map.find_opt dist children)
      else
        let left = if dist - max_dist < 0
          then None
          else Int_map.find_opt (dist-max_dist) children
        in
        let right = Int_map.find_opt (dist+max_dist) children in
        Option.to_list left @ Option.to_list right
    in
    match children, stack with
    | [], [] -> result
    | [], a :: q -> query result q ~cutoff ~max_dist name a
    | a :: q, _  ->
        let stack = q @ stack in
        query result stack ~cutoff ~max_dist name a

  let rec layer_seq result ~cutoff ~max_dist dist children name () =
    if max_dist > cutoff then Seq.Nil
    else
      let r = query_children result []  ~cutoff ~max_dist name dist children in
      let at_dist =
        Option.value ~default:[] (Int_map.find_opt max_dist r)
      in
      match at_dist with
      | [] -> layer_seq r ~cutoff ~max_dist:(max_dist+1) dist children name ()
      | _ ->
          let next =
            layer_seq r ~cutoff ~max_dist:(max_dist+1) dist children name in
          Seq.Cons( {left_candidates=at_dist; pref=max_dist}, next)

  let layers cutoff (t: int t) name =
    let cutoff = cutoff name in
    let dist = String.edit_distance t.name name in
    let results =
      if dist <= cutoff then Int_map.singleton dist [t.root]
      else Int_map.empty
    in
    layer_seq results ~cutoff ~max_dist:0 dist t.children name

  let rec make = function
    | [] -> invalid_arg "Empty lexicon"
    | (root_name,root) :: q ->
        let dist_map = List.fold_left (fun map (name,_ as x) ->
            let dist = String.edit_distance root_name name in
            Int_map.add_to_list dist x map
          ) Int_map.empty q
        in
        let children = Int_map.map make dist_map in
        { root; name=root_name; children }

  let preferences ?max_elements:_ ~cutoff d =
    let d = List.mapi (fun i item -> Item.name item, i) d in
    match d with
    | [] -> Fun.const Seq.empty
    | _ ->
        layers cutoff (make d)
end

let fuzzy_match_names ~compatibility left0 right =
  (* The edit distance between an existing name and a suggested rename must be
     at most half the length of the name. *)
  let cutoff name =
    let len = String.length name in
    len/2
  in
  if (*  *List.length left < 60 && List.length right < 60 *) true then
    (* Stable marriages. *)
    let left = Array.of_list left0 in
    let right = Array.of_list right in
    let compatibility i j =
      compatibility (Item.kind left.(i)) (Item.kind right.(j))
    in
    let matches =
      let preferences = match Sys.getenv_opt "OPREF" with
        | Some "T" -> Stable_marriage_diff.trie_preferences ~cutoff left
        | Some "B" -> BK_tree.preferences ~cutoff left0
        | Some "H" -> BK_tree_hm.preferences ~cutoff left0
        | Some "D" -> BK_tree_da.preferences ~cutoff left0
        | Some "S" -> simple_preferences ~cutoff left
        |  _ -> BK_tree_da.preferences ~cutoff left0
      in
      Stable_marriage_diff.diff
        ~preferences ~compatibility
        left
        right
    in
    let pairs = List.map (fun (x,y) -> Item.(item x, item y)) matches.pairs in
    { matches with pairs }
  else
    (* Greedy. *)
    greedy_matching ~compatibility ~cutoff left0 right
