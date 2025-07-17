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

  module Levenshtein_state(T : sig type a end) = struct
    (** A state of a Levenshtein automaton. *)

    type nonrec t = {
      trie : T.a t;  (** The remaining suffixes we can match against. *)
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
    let estimate_remaining_distance
      ~insertion_cost
      ~deletion_cost
      remaining_length
      trie
    =
      match (trie.shortest_suffix, trie.longest_suffix) with
      | Some (shortest_length, _), _
      when remaining_length <= shortest_length ->
          Some ((shortest_length - remaining_length) * insertion_cost)
      | _, Some (longest_length, _) when remaining_length >= longest_length ->
          Some ((remaining_length - longest_length) * deletion_cost)
      | None, None -> None
      | _, _ -> Some 0

    let make ~insertion_cost ~deletion_cost trie remaining_length distance =
      match
        estimate_remaining_distance
          ~insertion_cost
          ~deletion_cost
          remaining_length
          trie
      with
      | Some remaining_distance_estimation ->
          Some
            {
              trie;
              remaining_length;
              distance;
              remaining_distance_estimation
            }
      | None -> None

    (** Computes a list of all possible states after performing a single
        operation. *)
    let transitions
      ~insertion_cost
      ~deletion_cost
      ~substitution_cost
      text
      state
    =
      let n = String.length text in
      []
      (* Deletion. *)
      |> (fun transitions ->
          if state.remaining_length > 0 then
            match
              make
                ~insertion_cost
                ~deletion_cost
                state.trie
                (state.remaining_length - 1)
                (state.distance + deletion_cost)
            with
            | None -> transitions
            | Some transition -> transition :: transitions
          else
            transitions)
      (* Insertions. *)
      |> Hashtbl.fold
          (fun _ suffix_trie transitions ->
            match
              make
                ~insertion_cost
                ~deletion_cost
                suffix_trie
                state.remaining_length
                (state.distance + insertion_cost)
            with
            | None -> transitions
            | Some transition -> transition :: transitions)
          state.trie.strict_suffixes
      (* Substitutions. *)
      |> Hashtbl.fold
          (fun c suffix_trie transitions ->
            if state.remaining_length > 0 then
              let substitution_cost_here =
                if c = text.[n - state.remaining_length] then
                  0
                else
                  substitution_cost
              in
              match
                make
                  ~insertion_cost
                  ~deletion_cost
                  suffix_trie
                  (state.remaining_length - 1)
                  (state.distance + substitution_cost_here)
              with
              | None -> transitions
              | Some transition -> transition :: transitions
            else
              transitions)
          state.trie.strict_suffixes
  end


  let (%>%) (x:int) (y:int option) =
    match y with
    | None -> false
    | Some y -> x > y

  let compute_preferences (type a) ?(deletion_cost = 1) ?(insertion_cost = 1)
      ?(substitution_cost = 1) ?(cutoff : int option) (trie : a t)
      (string : string) : (a * int) Seq.t =

      let module State = Levenshtein_state(struct type nonrec a = a end) in
      let module PriorityQueue = Pqueue.MakeMin (State) in


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
                  (fun transition -> PriorityQueue.add queue transition)
                  (State.transitions
                    ~insertion_cost ~deletion_cost ~substitution_cost
                    string
                    state);
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
    Option.iter
      (fun state -> PriorityQueue.add queue state)
      (State.make ~insertion_cost ~deletion_cost trie n 0);
    let seen_states = Hashtbl.create trie.subtrie_count in
    compute queue seen_states

  let compute_preference_layers (type a) ?(deletion_cost = 1)
      ?(insertion_cost = 1) ?(substitution_cost = 1) ?(cutoff : int option)
      ?(max_elements : int option) (trie : a t) (string : string)
      : (a list * int) Seq.t =

    (* [current_distance = None] iff [acc = []]. *)
    let rec compute seq current_distance acc = fun () ->
      match current_distance, seq () with
      | None, Seq.Nil ->
          Seq.Nil
      | Some current_distance, Seq.Nil ->
          Seq.Cons ((acc, current_distance), Seq.empty)
      | None, Seq.Cons ((data, distance), next) ->
          compute next (Some distance) [ data ] ()
      | Some current_distance, Seq.Cons ((data, distance), next) ->
          if distance = current_distance then
            compute next (Some current_distance) (data :: acc) ()
          else
            let kont = compute next (Some distance) [ data ] in
            Seq.Cons ((acc, current_distance), kont)
    in

    let preferences =
      compute_preferences
        ~deletion_cost
        ~insertion_cost
        ~substitution_cost
        ?cutoff
        trie
        string
    in
    let seq =
      match max_elements with
      | Some n -> Seq.take n preferences
      | None -> preferences
    in
    compute seq None []
end



type 'a diff = {
  delete : 'a list;
  add : 'a list;
  substitute : ('a * 'a) list;
}

let reverse_diff d =
  {
    delete = d.add;
    add = d.delete;
    substitute = List.map (fun (right, left) -> (left, right)) d.substitute;
  }

module Field = struct
  type ('v, 't) t = {
    item : Types.signature_item;
    value : 'v;
    type_ : 't;
  }

  let first_order item value type_ = {
    item;
    value;
    type_;
  }

  let second_order item value = {
    item;
    value;
    type_ = ();
  }

  let ident field = Types.signature_item_id field.item
  let name field = Ident.name (ident field)
end

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

  let rec diff
    ~cutoff ?max_elements ~compatibility_test
    left right
  =
    let n = Array.length left in
    let m = Array.length right in

    if m > n then
      diff
        ~cutoff ?max_elements
        ~compatibility_test:(fun a b -> compatibility_test b a)
        right left
      |> reverse_diff
    else

    let left_states = Array.make n Left_available in
    let right_states =
      let left_trie =
        left
        |> Array.to_seq
        |> Seq.mapi (fun j field -> (Field.name field, j))
        |> Trie.of_seq
      in
      Array.map
        (fun right_field ->
          let name = Field.name right_field in
          let sequence =
            Trie.compute_preference_layers
              ~cutoff:(cutoff name)
              ?max_elements
              left_trie
              name
          in
          match sequence () with
          | Seq.Nil ->
              Right_closed
          | Seq.Cons ((layer, distance), tail) ->
              Right_available (First_phase {
                previous_layers = [ (layer, distance) ];
                current_layer = layer;
                current_layer_distance = distance;
                next_layers = tail;
              }))
        right
    in

    let has_better_choice i =
      match right_states.(i) with
      | Right_paired
        (First_phase {current_layer; _}
        | Second_phase {current_layer; _}) ->
          List.exists (fun j -> left_states.(j) = Left_available) current_layer
      | _ -> false
    in

    let has_weak_pair j =
      match left_states.(j) with
      | Left_paired (i, _) -> has_better_choice i
      | _ -> false
    in

    let phase i =
      match right_states.(i) with
      | Right_available phase | Right_paired phase -> Some phase
      | Right_closed -> None
    in

    let get_preferred_candidate phase =
      match phase with
      | First_phase {current_layer; current_layer_distance; _}
      | Second_phase {current_layer; current_layer_distance; _} ->
          current_layer
          |> List.find_opt (fun j -> left_states.(j) = Left_available)
          |> Option.value ~default:(List.hd current_layer),
          current_layer_distance
    in

    let propose i j d =
      has_weak_pair j ||
      match left_states.(j) with
      | Left_available -> true
      | Left_paired (i', d') ->
          d < d' ||
          d = d' &&
            match phase i, phase i' with
            | Some Second_phase _, Some First_phase _ -> true
            | _ -> false
    in

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
    in

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
    in

    let remove_left i j =
      match phase i with
      | Some First_phase preferences ->
          remove_left_from_preferences
            preferences
            j
            (fun () ->
              let phase = create_second_phase preferences.previous_layers in
              right_states.(i) <- Right_available phase)
            (fun layer distance ->
              preferences.previous_layers <-
                (layer, distance) :: preferences.previous_layers)
      | Some Second_phase preferences ->
          remove_left_from_preferences
            preferences
            j
            (fun () -> right_states.(i) <- Right_closed)
            (fun _ _ -> ())
      | None ->
          assert false
    in

    let ok = ref false in
    while not !ok do
      ok := true;
      for i = 0 to m - 1 do
        match right_states.(i) with
        | Right_available right_phase ->
            ok := false;
            let (j, d) = get_preferred_candidate right_phase in
            if compatibility_test right.(i) left.(j) && propose i j d then (
              (* Unpair [j]. *)
              (match left_states.(j) with
              | Left_paired (i', _) ->
                  (* [i'] is paired, so it has a phase. *)
                  right_states.(i') <- Right_available (Option.get (phase i'));
                  if not (has_better_choice i') then
                    remove_left i' j
              | _ -> ());
              (* Pair [i] and [j]. *)
              right_states.(i) <- Right_paired right_phase;
              left_states.(j) <- Left_paired (i, d))
            else
              remove_left i j
        | _ -> ()
      done
    done;

    {
      delete =
        left_states
        |> Array.to_seq
        |> Seq.zip (Array.to_seq left)
        |> Seq.filter_map (function
          | left_field, Left_available -> Some left_field
          | _, Left_paired _ -> None)
        |> List.of_seq;

      add =
        right
        |> Array.to_list
        |> List.filteri (fun i _ ->
          match right_states.(i) with
          | Right_paired _ -> false
          | _ -> true);

      substitute =
        left_states
        |> Array.to_seq
        |> Seq.zip (Array.to_seq left)
        |> Seq.filter_map (function
          | left_field, Left_paired (i, _) -> Some (left_field, right.(i))
          | _, Left_available -> None)
        |> List.of_seq;
    }
end

let (%<%) (x:int option) (y:int option) = match x, y with
  | None , _ -> false
  | _, None -> true
  | Some x, Some y -> x < y

let greedy_matching ~compatibility_test ~cutoff missings additions =
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
    if compatibility_test expected_field added_field then
      let distance =
        let expected_name = Field.name expected_field in
        Misc.edit_distance
          expected_name
          (Field.name added_field)
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
    |> List.filter
      (fun missing_field ->
        let missing_id = Field.ident missing_field in
        let missing_name = Ident.name missing_id in
        match
          list_extract
            (fun added_field ->
              (compute_distance missing_field added_field)
                %<% Some (cutoff missing_name))
            !remaining_added_fields
        with
        | None -> true
        | Some (added_field, additions) ->
            let name_change = added_field, missing_field in
            name_changes := name_change :: !name_changes;
            remaining_added_fields := additions;
            false)
  in

  actually_missing, !name_changes

let fuzzy_match_names compatibility_test missings additions =
  (* The edit distance between an existing name and a suggested rename must be
     at most half the length of the name. *)
  let cutoff name = String.length name / 2 in
  let m = List.length missings in
  let n = List.length additions in

  if m < 60 && n < 60 then
    (* Stable marriages. *)
    let diff =
      Stable_marriage_diff.diff
        ~cutoff ~max_elements:10 ~compatibility_test
        (Array.of_list additions)
        (Array.of_list missings)
    in
    diff.add, diff.substitute

  else
    (* Greedy. *)
    greedy_matching ~compatibility_test ~cutoff missings additions
