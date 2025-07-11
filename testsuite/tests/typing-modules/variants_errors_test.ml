(* TEST
 expect;
*)

module M1 : sig
  type t =
    | Foo of int * int
end = struct
  type t =
    | Foo of float * int
end;;
[%%expect{|
Lines 4-7, characters 6-3:
4 | ......struct
5 |   type t =
6 |     | Foo of float * int
7 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = Foo of float * int end
       is not included in
         sig type t = Foo of int * int end
       Try changing type "t" to
       type t = Foo of int * int
|}];;

module M2 : sig
  type t =
    | Foo of int * int
end = struct
  type t =
    | Foo of float
end;;
[%%expect{|
Lines 4-7, characters 6-3:
4 | ......struct
5 |   type t =
6 |     | Foo of float
7 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = Foo of float end
       is not included in
         sig type t = Foo of int * int end
       Try changing type "t" to
       type t = Foo of int * int
|}];;

module M3 : sig
  type t =
    | Foo of {x : int; y : int}
end = struct
  type t =
    | Foo of {x : float; y : int}
end;;
[%%expect{|
Lines 4-7, characters 6-3:
4 | ......struct
5 |   type t =
6 |     | Foo of {x : float; y : int}
7 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = Foo of { x : float; y : int; } end
       is not included in
         sig type t = Foo of { x : int; y : int; } end
       Try changing type "t" to
       type t = Foo of { x : int; y : int; }
|}];;

module M4 : sig
  type t =
    | Foo of {x : int; y : int}
end = struct
  type t =
    | Foo of float
end;;
[%%expect{|
Lines 4-7, characters 6-3:
4 | ......struct
5 |   type t =
6 |     | Foo of float
7 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = Foo of float end
       is not included in
         sig type t = Foo of { x : int; y : int; } end
       Try changing type "t" to
       type t = Foo of { x : int; y : int; }
|}];;

module M5 : sig
  type 'a t =
    | Foo : int -> int t
end = struct
  type 'a t =
    | Foo of 'a
end;;
[%%expect{|
Lines 4-7, characters 6-3:
4 | ......struct
5 |   type 'a t =
6 |     | Foo of 'a
7 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = Foo of 'a end
       is not included in
         sig type 'a t = Foo : int -> int t end
       Try changing type "t" to
       type 'a t = Foo : int -> int t
|}];;

module M : sig
  type ('a, 'b) t = A of 'a
end = struct
  type ('a, 'b) t = A of 'b
end;;
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type ('a, 'b) t = A of 'b
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type ('a, 'b) t = A of 'b end
       is not included in
         sig type ('a, 'b) t = A of 'a end
       Try changing type "t" to
       type ('a, 'b) t = A of 'a
|}];;

module M : sig
  type ('a, 'b) t = A of 'a
end = struct
  type ('b, 'a) t = A of 'a
end;;
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type ('b, 'a) t = A of 'a
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type ('b, 'a) t = A of 'a end
       is not included in
         sig type ('a, 'b) t = A of 'a end
       Try changing type "t" to
       type ('a, 'b) t = A of 'a
|}];;



(** Random additions and deletions of constructors *)

module Addition : sig
  type t =
    | A
    | B
    | C
    | D
end = struct
  type t =
    | A
    | B
    | Beta
    | C
    | D
end
[%%expect {|
Lines 9-16, characters 6-3:
 9 | ......struct
10 |   type t =
11 |     | A
12 |     | B
13 |     | Beta
14 |     | C
15 |     | D
16 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = A | B | Beta | C | D end
       is not included in
         sig type t = A | B | C | D end
       Try changing type "t" to
       type t = A | B | C | D
|}]


module Addition : sig
  type t =
    | A
    | B
    | C
    | D
end = struct
  type t =
    | A
    | B
    | D
end
[%%expect {|
Lines 7-12, characters 6-3:
 7 | ......struct
 8 |   type t =
 9 |     | A
10 |     | B
11 |     | D
12 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = A | B | D end
       is not included in
         sig type t = A | B | C | D end
       Try changing type "t" to
       type t = A | B | C | D
|}]


module Multi: sig
  type t =
    | A
    | B
    | C
    | D
    | E
    | F
    | G
end = struct
  type t =
    | A
    | B
    | Beta
    | C
    | D
    | F
    | G
    | Phi
end

[%%expect {|
Lines 10-20, characters 6-3:
10 | ......struct
11 |   type t =
12 |     | A
13 |     | B
14 |     | Beta
...
17 |     | F
18 |     | G
19 |     | Phi
20 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = A | B | Beta | C | D | F | G | Phi end
       is not included in
         sig type t = A | B | C | D | E | F | G end
       Try changing type "t" to
       type t = A | B | C | D | E | F | G
|}]


(** Swaps and moves *)

module Swap : sig
  type t =
    | A
    | E
    | C
    | D
    | B
end = struct
  type t =
    | Alpha
    | B
    | C
    | D
    | E
end
[%%expect {|
Lines 10-17, characters 6-3:
10 | ......struct
11 |   type t =
12 |     | Alpha
13 |     | B
14 |     | C
15 |     | D
16 |     | E
17 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = Alpha | B | C | D | E end
       is not included in
         sig type t = A | E | C | D | B end
       Try changing type "t" to
       type t = A | E | C | D | B
|}]


module Move: sig
  type t =
    | A of int
    | B
    | C
    | D
    | E
    | F
end = struct
  type t =
    | A of float
    | B
    | D
    | E
    | F
    | C
end
[%%expect {|
Lines 9-17, characters 6-3:
 9 | ......struct
10 |   type t =
11 |     | A of float
12 |     | B
13 |     | D
14 |     | E
15 |     | F
16 |     | C
17 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = A of float | B | D | E | F | C end
       is not included in
         sig type t = A of int | B | C | D | E | F end
       Try changing type "t" to
       type t = A of int | B | C | D | E | F
|}]


module Imperfect_match: sig
  type t = A | B of int
end = struct
 type t = A | R | C of int | S | B of float
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |  type t = A | R | C of int | S | B of float
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = A | R | C of int | S | B of float end
       is not included in
         sig type t = A | B of int end
       Try changing type "t" to
       type t = A | B of int
|}]

module Very_imperfect_match: sig
  type t = A | B of int
end = struct
 type t = A | R | C of float | S | D of int
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |  type t = A | R | C of float | S | D of int
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = A | R | C of float | S | D of int end
       is not included in
         sig type t = A | B of int end
       Try changing type "t" to
       type t = A | B of int
|}]
