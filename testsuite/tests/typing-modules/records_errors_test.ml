(* TEST
 expect;
*)

module M1 : sig
  type t = {f0 : unit * unit * unit * int * unit * unit * unit;
            f1 : unit * unit * unit * int * unit * unit * unit}
end = struct
  type t = {f0 : unit * unit * unit * float* unit * unit * unit;
            f1 : unit * unit * unit * string * unit * unit * unit}
end;;
[%%expect{|
Lines 4-7, characters 6-3:
4 | ......struct
5 |   type t = {f0 : unit * unit * unit * float* unit * unit * unit;
6 |             f1 : unit * unit * unit * string * unit * unit * unit}
7 | end..
Error: Signature mismatch:
       Modules do not match:
         sig
           type t = {
             f0 : unit * unit * unit * float * unit * unit * unit;
             f1 : unit * unit * unit * string * unit * unit * unit;
           }
         end
       is not included in
         sig
           type t = {
             f0 : unit * unit * unit * int * unit * unit * unit;
             f1 : unit * unit * unit * int * unit * unit * unit;
           }
         end
       Try changing type "t" to
       type t = {
         f0 : unit * unit * unit * int * unit * unit * unit;
         f1 : unit * unit * unit * int * unit * unit * unit;
       }
|}];;


module M2 : sig
  type t = {mutable f0 : unit * unit * unit * int * unit * unit * unit;
            f1 : unit * unit * unit * int * unit * unit * unit}
end = struct
  type t = {f0 : unit * unit * unit * float* unit * unit * unit;
            f1 : unit * unit * unit * string * unit * unit * unit}
end;;
[%%expect{|
Lines 4-7, characters 6-3:
4 | ......struct
5 |   type t = {f0 : unit * unit * unit * float* unit * unit * unit;
6 |             f1 : unit * unit * unit * string * unit * unit * unit}
7 | end..
Error: Signature mismatch:
       Modules do not match:
         sig
           type t = {
             f0 : unit * unit * unit * float * unit * unit * unit;
             f1 : unit * unit * unit * string * unit * unit * unit;
           }
         end
       is not included in
         sig
           type t = {
             mutable f0 : unit * unit * unit * int * unit * unit * unit;
             f1 : unit * unit * unit * int * unit * unit * unit;
           }
         end
       Try changing type "t" to
       type t = {
         mutable f0 : unit * unit * unit * int * unit * unit * unit;
         f1 : unit * unit * unit * int * unit * unit * unit;
       }
|}];;

module M3 : sig
  type t = {f0 : unit}
end = struct
  type t = {f1 : unit}
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = {f1 : unit}
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = { f1 : unit; } end
       is not included in
         sig type t = { f0 : unit; } end
       Try changing type "t" to
       type t = { f0 : unit; }
|}];;

module M4 : sig
  type t = {f0 : unit; f1 : unit}
end = struct
  type t = {f0 : unit}
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = {f0 : unit}
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = { f0 : unit; } end
       is not included in
         sig type t = { f0 : unit; f1 : unit; } end
       Try changing type "t" to
       type t = { f0 : unit; f1 : unit; }
|}];;


(** Random additions and deletions of fields *)

module Addition : sig
  type t = {a : unit; b : unit; c : unit; d : unit}
end = struct
  type t = {a : unit; b : unit; beta : unit; c : unit; d: unit}
end
[%%expect {|
Lines 5-7, characters 6-3:
5 | ......struct
6 |   type t = {a : unit; b : unit; beta : unit; c : unit; d: unit}
7 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           type t = { a : unit; b : unit; beta : unit; c : unit; d : unit; }
         end
       is not included in
         sig type t = { a : unit; b : unit; c : unit; d : unit; } end
       Try changing type "t" to
       type t = { a : unit; b : unit; c : unit; d : unit; }
|}]


module Deletion : sig
  type t = {a : unit; b : unit; c : unit; d : unit}
end = struct
  type t = {a : unit; c : unit; d : unit}
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = {a : unit; c : unit; d : unit}
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = { a : unit; c : unit; d : unit; } end
       is not included in
         sig type t = { a : unit; b : unit; c : unit; d : unit; } end
       Try changing type "t" to
       type t = { a : unit; b : unit; c : unit; d : unit; }
|}]


module Multi: sig
  type t = {
    a : unit;
    b : unit;
    c : unit;
    d : unit;
    e : unit;
    f : unit;
    g : unit
  }
end = struct
  type t = {
    a : unit;
    b : unit;
    beta: int;
    c : unit;
    d : unit;
    f : unit;
    g : unit;
    phi : unit;
  }
end

[%%expect {|
Lines 11-22, characters 6-3:
11 | ......struct
12 |   type t = {
13 |     a : unit;
14 |     b : unit;
15 |     beta: int;
...
19 |     g : unit;
20 |     phi : unit;
21 |   }
22 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           type t = {
             a : unit;
             b : unit;
             beta : int;
             c : unit;
             d : unit;
             f : unit;
             g : unit;
             phi : unit;
           }
         end
       is not included in
         sig
           type t = {
             a : unit;
             b : unit;
             c : unit;
             d : unit;
             e : unit;
             f : unit;
             g : unit;
           }
         end
       Try changing type "t" to
       type t = {
         a : unit;
         b : unit;
         c : unit;
         d : unit;
         e : unit;
         f : unit;
         g : unit;
       }
|}]


(** Multiple errors *)

module M : sig
  type t = { a:int; e:int; c:int; d:int; b:int }
end = struct
  type t = { alpha:int; b:int; c:int; d:int; e:int }
end
[%%expect {|
Lines 5-7, characters 6-3:
5 | ......struct
6 |   type t = { alpha:int; b:int; c:int; d:int; e:int }
7 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           type t = { alpha : int; b : int; c : int; d : int; e : int; }
         end
       is not included in
         sig type t = { a : int; e : int; c : int; d : int; b : int; } end
       Try changing type "t" to
       type t = { a : int; e : int; c : int; d : int; b : int; }
|}]


module M: sig
  type t = { a:int; b:int; c:int; d:int; e:int; f:float }
end =
struct
  type t = { b:int; c:int; d:int; e:int; a:int; f:int }
end
[%%expect {|
Lines 4-6, characters 0-3:
4 | struct
5 |   type t = { b:int; c:int; d:int; e:int; a:int; f:int }
6 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           type t = { b : int; c : int; d : int; e : int; a : int; f : int; }
         end
       is not included in
         sig
           type t = {
             a : int;
             b : int;
             c : int;
             d : int;
             e : int;
             f : float;
           }
         end
       Try changing type "t" to
       type t = { a : int; b : int; c : int; d : int; e : int; f : float; }
|}]

(** Existential types introduce equations that must be taken in account
    when diffing
*)


module Eq : sig
  type t = A: { a:'a; b:'b; x:'a } -> t
end = struct
  type t = A: { a:'a; b:'b; x:'x } -> t
end
[%%expect {|
Lines 8-10, characters 6-3:
 8 | ......struct
 9 |   type t = A: { a:'a; b:'b; x:'x } -> t
10 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = A : { a : 'a; b : 'b; x : 'x; } -> t end
       is not included in
         sig type t = A : { a : 'a; b : 'b; x : 'a; } -> t end
       Try changing type "t" to
       type t = A : { a : 'a; b : 'b; x : 'a; } -> t
|}]


module Not_a_swap: sig
  type t = A: { x:'a; a:'a; b:'b; y:'b} -> t
end = struct
  type t = A: { y:'a; a:'a; b:'b; x:'b} -> t
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = A: { y:'a; a:'a; b:'b; x:'b} -> t
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = A : { y : 'a; a : 'a; b : 'b; x : 'b; } -> t end
       is not included in
         sig type t = A : { x : 'a; a : 'a; b : 'b; y : 'b; } -> t end
       Try changing type "t" to
       type t = A : { x : 'a; a : 'a; b : 'b; y : 'b; } -> t
|}]

module Swap: sig
  type t = A: { x:'a; a:'a; b:'b; y:'b} -> t
end = struct
  type t = A: { y:'b; a:'a; b:'b; x:'a} -> t
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = A: { y:'b; a:'a; b:'b; x:'a} -> t
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = A : { y : 'b; a : 'a; b : 'b; x : 'a; } -> t end
       is not included in
         sig type t = A : { x : 'a; a : 'a; b : 'b; y : 'b; } -> t end
       Try changing type "t" to
       type t = A : { x : 'a; a : 'a; b : 'b; y : 'b; } -> t
|}]


module Not_a_move: sig
  type t = A: { a:'a; b:'b; x:'b} -> t
end = struct
  type t = A: { x:'a; a:'a; b:'b} -> t
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = A: { x:'a; a:'a; b:'b} -> t
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = A : { x : 'a; a : 'a; b : 'b; } -> t end
       is not included in
         sig type t = A : { a : 'a; b : 'b; x : 'b; } -> t end
       Try changing type "t" to
       type t = A : { a : 'a; b : 'b; x : 'b; } -> t
|}]


module Move: sig
  type t = A: { a:'a; b:'b; x:'b} -> t
end = struct
  type t = A: { x:'b; a:'a; b:'b} -> t
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = A: { x:'b; a:'a; b:'b} -> t
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = A : { x : 'b; a : 'a; b : 'b; } -> t end
       is not included in
         sig type t = A : { a : 'a; b : 'b; x : 'b; } -> t end
       Try changing type "t" to
       type t = A : { a : 'a; b : 'b; x : 'b; } -> t
|}]


module Imperfect_match: sig
  type t = { a:unit; b:int }
end = struct
 type t = { a:unit; r:unit; c:int; s:unit; b:float }
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |  type t = { a:unit; r:unit; c:int; s:unit; b:float }
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           type t = { a : unit; r : unit; c : int; s : unit; b : float; }
         end
       is not included in
         sig type t = { a : unit; b : int; } end
       Try changing type "t" to
       type t = { a : unit; b : int; }
|}]


module Very_imperfect_match: sig
  type t = { a:unit; b:int }
end = struct
 type t = { a:unit; r:unit; c:float; s:unit; d:int }
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |  type t = { a:unit; r:unit; c:float; s:unit; d:int }
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           type t = { a : unit; r : unit; c : float; s : unit; d : int; }
         end
       is not included in
         sig type t = { a : unit; b : int; } end
       Try changing type "t" to
       type t = { a : unit; b : int; }
|}]
