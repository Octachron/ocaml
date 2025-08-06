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

module Trie = struct
  let new_uid =
    let counter = ref 0 in
    fun () ->
      incr counter;
      !counter

  type 'a t = {
    uid : int;
    mutable leaf_data : 'a option;
    strict_suffixes : (char, 'a t) Hashtbl.t;
    mutable subtrie_count : int;
        (** The total number of subtries this trie contains (including
            itself). *)
    mutable shortest_suffix : (int * 'a) option;
        (** The length and associated data of a shortest suffix, if any. *)
    mutable longest_suffix : (int * 'a) option;
        (** The length and associated data of a longest suffix, if any. *)
  }

  let create () =
    {
      uid = new_uid ();
      leaf_data = None;
      strict_suffixes = Hashtbl.create 1;
      subtrie_count = 1;
      shortest_suffix = None;
      longest_suffix = None;
    }

  let add trie string data =
    let rec aux s length trie =
      (trie.shortest_suffix <-
        match trie.shortest_suffix with
        | Some (l, d) when l <= length -> Some (l, d)
        | _ -> Some (length, data));
      (trie.longest_suffix <-
        match trie.longest_suffix with
        | Some (l, d) when l >= length -> Some (l, d)
        | _ -> Some (length, data));
      match s () with
      | Seq.Nil ->
          trie.leaf_data <- Some data
      | Seq.Cons (c, next) ->
          match Hashtbl.find_opt trie.strict_suffixes c with
          | None ->
              let new_child = create () in
              aux next (length - 1) new_child;
              Hashtbl.add trie.strict_suffixes c new_child;
              trie.subtrie_count <- trie.subtrie_count + new_child.subtrie_count
          | Some child ->
              let subtries_without_child =
                trie.subtrie_count - child.subtrie_count
              in
              aux next (length - 1) child;
              trie.subtrie_count <- subtries_without_child + child.subtrie_count
    in
    aux (String.to_seq string) (String.length string) trie

  let of_seq entries =
    let trie = create () in
    Seq.iter (fun (string, data) -> add trie string data) entries;
    trie

  module Levenshtein_state = struct
    (** A state of a Levenshtein automaton. *)

    type nonrec 'a t = {
      trie : 'a t;  (** The remaining suffixes we can match against. *)
      remaining_length : int;
          (** The remaining length of the string we are trying to match. *)
      distance : int;
          (** The current distance to the string we are trying to match. *)
      remaining_distance_estimation : int;
          (** An estimation of the remaining distance. *)
    }

    let priority state = state.distance + state.remaining_distance_estimation
    let compare s s' = compare (priority s) (priority s')

    (** An admissible heuristic for A* (as in, it always under-estimates the
        true remainign distance). *)
    let estimate_remaining_distance cost remaining_length trie =
      match (trie.shortest_suffix, trie.longest_suffix) with
      | Some (shortest_length, _), _
      when remaining_length <= shortest_length ->
          Some ((shortest_length - remaining_length) * cost.insertion)
      | _, Some (longest_length, _) when remaining_length >= longest_length ->
          Some ((remaining_length - longest_length) * cost.deletion)
      | None, None -> None
      | _, _ -> Some 0

    let make cost trie remaining_length distance =
      match estimate_remaining_distance cost remaining_length trie with
      | Some remaining_distance_estimation ->
          [{
            trie;
            remaining_length;
            distance;
            remaining_distance_estimation
          }]
      | None -> []

    (** Computes a list of all possible states after performing a single
        operation. *)
    let transitions cost text state =
      let n = String.length text in
      let deletions =
        if state.remaining_length > 0 then
          make cost state.trie
            (state.remaining_length - 1)
            (state.distance + cost.deletion)
        else []
      in
      Hashtbl.fold
        (fun c suffix_trie transitions ->
           let insertion = make cost suffix_trie
               state.remaining_length
               (state.distance + cost.insertion)
           in
           let subst =
             if state.remaining_length = 0 then [] else
             let substitution_cost_here =
               if c = text.[n - state.remaining_length] then
                 0
               else
                 cost.substitution
             in
             make cost suffix_trie
               (state.remaining_length - 1)
               (state.distance + substitution_cost_here)
           in
            subst @ insertion @ transitions
        ) state.trie.strict_suffixes deletions
  end


  let (%>%) (x:int) (y:int option) =
    match y with
    | None -> false
    | Some y -> x > y

  module State = Levenshtein_state

  let default_cost = { deletion = 1; insertion=1; substitution=1 }

  let compute_preferences (type a) cost ?(cutoff : int option) (trie : a t)
      (string : string) : (a * int) Seq.t =
    let module PriorityQueue =
      Pqueue.MakeMin (struct type t = a State.t let compare = State.compare end)
    in
    let rec compute queue seen_states = fun () ->
      match PriorityQueue.pop_min queue with
      | None -> Seq.Nil
      | Some state ->
          if State.priority state %>% cutoff then
            Seq.Nil
          else
            let state_id = state.trie.uid, state.State.remaining_length in
            if Hashtbl.mem seen_states state_id then
              compute queue seen_states ()
            else (
              Hashtbl.add seen_states state_id ();
              List.iter
                  (PriorityQueue.add queue)
                  (State.transitions cost string state);
              match state with
              | {
                State.trie = { leaf_data = Some data; _ };
                remaining_length = 0;
                distance;
                _;
              } ->
                  Seq.Cons ((data, distance), compute queue seen_states)
              | _ -> compute queue seen_states ()
            )
    in

    let n = String.length string in
    let queue = PriorityQueue.create () in
    List.iter (PriorityQueue.add queue) (State.make cost trie n 0);
    let seen_states = Hashtbl.create trie.subtrie_count in
    compute queue seen_states

  let rec group_ties seq current_distance acc () =
    match seq () with
    | Seq.Nil -> Seq.Cons ((acc, current_distance), Seq.empty)
    | Seq.Cons ((data, distance), next) ->
        if distance = current_distance then
          group_ties next current_distance (data :: acc) ()
        else
          let next_layer = group_ties next distance [ data ] in
          Seq.Cons ((acc, current_distance), next_layer)

  let compute_preference_layers ?(cost = default_cost) ?cutoff ?max_elements
      trie query =
    let preferences = compute_preferences cost ?cutoff trie query in
    let seq =
      match max_elements with
      | Some n -> Seq.take n preferences
      | None -> preferences
    in
    match seq () with
    | Seq.Nil -> Seq.empty
    | Seq.Cons((data,distance), seq) -> group_ties seq distance [data]
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
  let rec list_remove x list =
    match list with
    | [] -> []
    | hd :: tl when hd = x -> tl
    | hd :: tl -> hd :: list_remove x tl

  (* This implementation does not use the same semantics as the original paper.
     Below is a conversion from the paper's terms to the implementation's terms:
     - woman: left
     - man: right
     - engaged (woman / man): paired
     - maiden (woman) / active (man): available
     - lad: first phase
     - bachelor: second phase
     - old bachelor: closed
     - uncertain (man): has a better choice
     - flighty (woman): has a weak pair *)

  type distance = int

  type 'a preferences = {
    mutable previous_layers : 'a;
        (** Invariant: if this is a list, it is not empty. *)
    mutable current_layer : int list; (** Invariant: this list is not empty. *)
    mutable current_layer_distance : distance;
    mutable next_layers : (int list * distance) Seq.t;
  }

  type left_state =
    | Left_available
    | Left_paired of int * distance

  type right_phase =
    | First_phase of (int list * distance) list preferences
    | Second_phase of unit preferences

  type right_state =
    | Right_available of right_phase
    | Right_paired of right_phase
    | Right_closed

  type ('a,'b) state = { left: 'a array; right: 'b array}

  let has_better_choice state i =
    match state.right.(i) with
    | Right_paired
        (First_phase {current_layer; _}
        | Second_phase {current_layer; _}) ->
        List.exists (fun j -> state.left.(j) = Left_available) current_layer
    | _ -> false

  let has_weak_pair state j =
    match state.left.(j) with
    | Left_paired (i, _) -> has_better_choice state i
    | _ -> false

  let phase state i =
    match state.right.(i) with
    | Right_available phase | Right_paired phase -> Some phase
    | Right_closed -> None

  let get_preferred_candidate state phase =
    match phase with
    | First_phase {current_layer; current_layer_distance; _}
    | Second_phase {current_layer; current_layer_distance; _} ->
        current_layer
        |> List.find_opt (fun j -> state.left.(j) = Left_available)
        |> Option.value ~default:(List.hd current_layer),
        current_layer_distance

  let propose state i j d =
    has_weak_pair state j ||
    match state.left.(j) with
    | Left_available -> true
    | Left_paired (i', d') ->
        d < d' ||
        d = d' &&
        match phase state i, phase state i' with
        | Some Second_phase _, Some First_phase _ -> true
        | _ -> false

  let create_second_phase layers =
    match layers with
    | [] -> assert false
    | (layer, distance) :: tail ->
        Second_phase {
          previous_layers = ();
          current_layer = layer;
          current_layer_distance = distance;
          next_layers = List.to_seq tail;
        }

  let remove_left_from_preferences preferences j nil_callback cons_callback =
    match list_remove j preferences.current_layer with
    | [] -> (
        match preferences.next_layers () with
        | Seq.Nil ->
            nil_callback ()
        | Seq.Cons ((layer, distance), next) ->
            preferences.current_layer <- layer;
            preferences.current_layer_distance <- distance;
            preferences.next_layers <- next;
            cons_callback layer distance)
    | remaining_elements ->
        preferences.current_layer <- remaining_elements

  let remove_left state i j =
    match phase state i with
    | Some First_phase preferences ->
        remove_left_from_preferences
          preferences
          j
          (fun () ->
             let phase = create_second_phase preferences.previous_layers in
             state.right.(i) <- Right_available phase)
          (fun layer distance ->
             preferences.previous_layers <-
               (layer, distance) :: preferences.previous_layers)
    | Some Second_phase preferences ->
        remove_left_from_preferences
          preferences
          j
          (fun () -> state.right.(i) <- Right_closed)
          (fun _ _ -> ())
    | None ->
        assert false

  let init_trie x =
    let name i field = Item.name field, i in
    x |> Array.to_seq |> Seq.mapi name |> Trie.of_seq

  let init_right_state ~cutoff ?max_elements left right =
    let left_trie = init_trie left in
    Array.map
      (fun right_field ->
         let name = Item.name right_field in
         let sequence =
           Trie.compute_preference_layers
             ~cutoff:(cutoff name)
             ?max_elements
             left_trie
             name
         in
         match sequence () with
         | Seq.Nil -> Right_closed
         | Seq.Cons ((layer, distance), tail) ->
             Right_available (First_phase {
                 previous_layers = [ (layer, distance) ];
                 current_layer = layer;
                 current_layer_distance = distance;
                 next_layers = tail;
               }))
      right

  let diff ~cutoff ?max_elements ~compatibility left right =
    let n = Array.length left in
    let m = Array.length right in
    let left_state = Array.make n Left_available in
    let right_state = init_right_state ~cutoff ?max_elements left right in
    let state = { left=left_state; right=right_state } in
    let ok = ref false in
    while not !ok do
      ok := true;
      for i = 0 to m - 1 do
        match state.right.(i) with
        | Right_available right_phase ->
            ok := false;
            let (j, d) = get_preferred_candidate state right_phase in
            if compatibility (Item.kind left.(j)) (Item.kind right.(i))
            && propose state i j d then (
              (* Unpair [j]. *)
              (match state.left.(j) with
              | Left_paired (i', _) ->
                  (* [i'] is paired, so it has a phase. *)
                  state.right.(i') <-
                    Right_available (Option.get (phase state i'));
                  if not (has_better_choice state i') then
                    remove_left state  i' j
              | _ -> ());
              (* Pair [i] and [j]. *)
              state.right.(i) <- Right_paired right_phase;
              state.left.(j) <- Left_paired (i, d))
            else
              remove_left state i j
        | _ -> ()
      done
    done;
    let left_final = Seq.zip (Array.to_seq left) (Array.to_seq state.left) in
    let left, pairs = Seq.partition_map (fun (field, status) ->
        match status with
        | Left_available -> Either.Left field
        | Left_paired (i,_) -> Either.Right (field, right.(i))
      ) left_final
    in
    {
      left = List.of_seq left;
      right =
        Array.to_seq right
        |> Seq.filteri (fun i _ ->
          match state.right.(i) with
          | Right_paired _ -> false
          | _ -> true)
        |> List.of_seq
      ;
      pairs = List.of_seq pairs;
    }

  let diff ~cutoff ?max_elements ~compatibility left right =
    if Array.length right >  Array.length left then
      diff
        ~cutoff ?max_elements
        ~compatibility:(fun a b -> compatibility b a)
        right left
      |> reverse_matches
    else diff ~cutoff ?max_elements ~compatibility left right

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

let fuzzy_match_names ~compatibility left right =
  (* The edit distance between an existing name and a suggested rename must be
     at most half the length of the name. *)
  let cutoff name = String.length name / 2 in
  let m = List.length left in
  let n = List.length right in

  if m < 60 && n < 60 then
    (* Stable marriages. *)
    let matches =
      Stable_marriage_diff.diff
        ~cutoff ~max_elements:10 ~compatibility
        (Array.of_list left)
        (Array.of_list right)
    in
    let pairs = List.map (fun (x,y) -> Item.(item x, item y)) matches.pairs in
    { matches with pairs }
  else
    (* Greedy. *)
    greedy_matching ~compatibility ~cutoff left right
