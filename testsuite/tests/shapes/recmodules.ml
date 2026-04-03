(* TEST
 flags = "-dshape";
 expect;
*)

(**********)
(* Simple *)
(**********)

module rec A : sig
   type t = Leaf of B.t
 end = struct
   type t = Leaf of B.t
 end
 and B
   : sig type t = int end
   = struct type t = int end
[%%expect{|
{
 "A"[module] -> {
                 "t"[type] -> {<.8>
                               "Leaf"[constructor] -> {<.9>};
                               };
                 };
 "B"[module] -> {
                 "t"[type] -> <.10>;
                 };
 }
module rec A : sig type t = Leaf of B.t end
and B : sig type t = int end
|}, Principal{|
{
 "A"[module] -> {
                 "t"[type] -> {<.19>
                               "Leaf"[constructor] -> {<.20>};
                               };
                 };
 "B"[module] -> {
                 "t"[type] -> <.21>;
                 };
 }
module rec A : sig type t = Leaf of B.t end
and B : sig type t = int end
|}, Rectypes{|
{
 "A"[module] -> {
                 "t"[type] -> {<.30>
                               "Leaf"[constructor] -> {<.31>};
                               };
                 };
 "B"[module] -> {
                 "t"[type] -> <.32>;
                 };
 }
module rec A : sig type t = Leaf of B.t end
and B : sig type t = int end
|}]

(*****************)
(* Intf only ... *)
(*****************)

(* reduce is going to die on this. *)

module rec A : sig
   type t = Leaf of B.t
 end = A

and B : sig
  type t = int
end = B
[%%expect{|
{
 "A"[module] -> A<.33>;
 "B"[module] -> B<.34>;
 }
module rec A : sig type t = Leaf of B.t end
and B : sig type t = int end
|}, Principal{|
{
 "A"[module] -> A<.41>;
 "B"[module] -> B<.42>;
 }
module rec A : sig type t = Leaf of B.t end
and B : sig type t = int end
|}, Rectypes{|
{
 "A"[module] -> A<.49>;
 "B"[module] -> B<.50>;
 }
module rec A : sig type t = Leaf of B.t end
and B : sig type t = int end
|}]

(***************************)
(* Example from the manual *)
(***************************)

 module rec A : sig
   type t = Leaf of string | Node of ASet.t
   val compare: t -> t -> int
 end = struct
   type t = Leaf of string | Node of ASet.t
   let compare t1 t2 =
     match (t1, t2) with
     | (Leaf s1, Leaf s2) -> Stdlib.compare s1 s2
     | (Leaf _, Node _) -> 1
     | (Node _, Leaf _) -> -1
     | (Node n1, Node n2) -> ASet.compare n1 n2
 end

(* we restrict the sig to limit the bloat in the expected output. *)
and ASet : sig
  type t
  type elt = A.t
  val compare : t -> t -> int
end = Set.Make(A)
[%%expect{|
{
 "A"[module] ->
   {
    "compare"[value] -> <.76>;
    "t"[type] ->
      {<.73>
       "Leaf"[constructor] -> {<.74>};
       "Node"[constructor] -> {<.75>};
       };
    };
 "ASet"[module] ->
   {
    "compare"[value] ->
      CU Stdlib . "Set"[module] . "Make"[module](A<.57>) . "compare"[value];
    "elt"[type] ->
      CU Stdlib . "Set"[module] . "Make"[module](A<.57>) . "elt"[type];
    "t"[type] ->
      CU Stdlib . "Set"[module] . "Make"[module](A<.57>) . "t"[type];
    };
 }
module rec A :
  sig
    type t = Leaf of string | Node of ASet.t
    val compare : t -> t -> int
  end
and ASet : sig type t type elt = A.t val compare : t -> t -> int end
|}, Principal{|
{
 "A"[module] ->
   {
    "compare"[value] -> <.102>;
    "t"[type] ->
      {<.99>
       "Leaf"[constructor] -> {<.100>};
       "Node"[constructor] -> {<.101>};
       };
    };
 "ASet"[module] ->
   {
    "compare"[value] ->
      CU Stdlib . "Set"[module] . "Make"[module](A<.83>) . "compare"[value];
    "elt"[type] ->
      CU Stdlib . "Set"[module] . "Make"[module](A<.83>) . "elt"[type];
    "t"[type] ->
      CU Stdlib . "Set"[module] . "Make"[module](A<.83>) . "t"[type];
    };
 }
module rec A :
  sig
    type t = Leaf of string | Node of ASet.t
    val compare : t -> t -> int
  end
and ASet : sig type t type elt = A.t val compare : t -> t -> int end
|}, Rectypes{|
{
 "A"[module] ->
   {
    "compare"[value] -> <.128>;
    "t"[type] ->
      {<.125>
       "Leaf"[constructor] -> {<.126>};
       "Node"[constructor] -> {<.127>};
       };
    };
 "ASet"[module] ->
   {
    "compare"[value] ->
      CU Stdlib . "Set"[module] . "Make"[module](A<.109>) . "compare"[value];
    "elt"[type] ->
      CU Stdlib . "Set"[module] . "Make"[module](A<.109>) . "elt"[type];
    "t"[type] ->
      CU Stdlib . "Set"[module] . "Make"[module](A<.109>) . "t"[type];
    };
 }
module rec A :
  sig
    type t = Leaf of string | Node of ASet.t
    val compare : t -> t -> int
  end
and ASet : sig type t type elt = A.t val compare : t -> t -> int end
|}]
