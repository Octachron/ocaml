(* TEST
 expect;
*)

(********************************** Equality **********************************)

module M : sig
  type ('a, 'b) t = 'a * 'b
end = struct
  type ('a, 'b) t = 'a * 'a
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type ('a, 'b) t = 'a * 'a
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type ('a, 'b) t = 'a * 'a end
       is not included in
         sig type ('a, 'b) t = 'a * 'b end
       Try changing type "t" to
       type ('a, 'b) t = 'a * 'b
|}];;

module M : sig
  type ('a, 'b) t = 'a * 'a
end = struct
  type ('a, 'b) t = 'a * 'b
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type ('a, 'b) t = 'a * 'b
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type ('a, 'b) t = 'a * 'b end
       is not included in
         sig type ('a, 'b) t = 'a * 'a end
       Try changing type "t" to
       type ('a, 'b) t = 'a * 'a
|}];;

type 'a x
module M: sig
  type ('a,'b,'c) t = ('a * 'b * 'c * 'b * 'a) x
end = struct
  type ('b,'c,'a) t = ('b * 'c * 'a * 'c * 'a) x
end
[%%expect{|
type 'a x
Lines 4-6, characters 6-3:
4 | ......struct
5 |   type ('b,'c,'a) t = ('b * 'c * 'a * 'c * 'a) x
6 | end
Error: Signature mismatch:
       Modules do not match:
         sig type ('b, 'c, 'a) t = ('b * 'c * 'a * 'c * 'a) x end
       is not included in
         sig type ('a, 'b, 'c) t = ('a * 'b * 'c * 'b * 'a) x end
       Try changing type "t" to
       type ('a, 'b, 'c) t = ('a * 'b * 'c * 'b * 'a) x
|}]

module M : sig
  type t = <m : 'b. 'b * ('b * <m:'c. 'c * 'bar> as 'bar)>
end = struct
  type t = <m : 'a. 'a * ('a * 'foo)> as 'foo
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = <m : 'a. 'a * ('a * 'foo)> as 'foo
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = < m : 'a. 'a * ('a * 'b) > as 'b end
       is not included in
         sig type t = < m : 'b. 'b * ('b * < m : 'c. 'c * 'a > as 'a) > end
       Try changing type "t" to
       type t = < m : 'b. 'b * ('b * < m : 'c. 'c * 'a > as 'a) >
|}];;

type s = private < m : int; .. >;;
[%%expect{|
type s = private < m : int; .. >
|}];;

module M : sig
  type t = s
end = struct
  type t = <m : int>
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = <m : int>
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = < m : int > end
       is not included in
         sig type t = s end
       Try changing type "t" to
       type t = s
|}];;

module M : sig
  type t = <m : int>
end = struct
  type t = s
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = s
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = s end
       is not included in
         sig type t = < m : int > end
       Try changing type "t" to
       type t = < m : int >
|}];;

module M : sig
  type t =
    | Foo of (int)*float
end = struct
  type t =
    | Foo of (int*int)*float
end;;
[%%expect{|
Lines 4-7, characters 6-3:
4 | ......struct
5 |   type t =
6 |     | Foo of (int*int)*float
7 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = Foo of (int * int) * float end
       is not included in
         sig type t = Foo of int * float end
       Try changing type "t" to
       type t = Foo of int * float
|}];;

module M : sig
  type t = (int * float)
end = struct
  type t = (int * float * int)
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = (int * float * int)
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = int * float * int end
       is not included in
         sig type t = int * float end
       Try changing type "t" to
       type t = int * float
|}];;

module M : sig
  type t = <n : int; m : float>
end = struct
  type t = <n : int; f : float>
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = <n : int; f : float>
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = < f : float; n : int > end
       is not included in
         sig type t = < m : float; n : int > end
       Try changing type "t" to
       type t = < m : float; n : int >
|}];;

module M : sig
  type t = <n : int; m : float>
end = struct
  type t = <n : int>
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = <n : int>
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = < n : int > end
       is not included in
         sig type t = < m : float; n : int > end
       Try changing type "t" to
       type t = < m : float; n : int >
|}];;

module M4 : sig
  type t = <n : int; m : float * int>
end = struct
  type t = <n : int; m : int>
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = <n : int; m : int>
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = < m : int; n : int > end
       is not included in
         sig type t = < m : float * int; n : int > end
       Try changing type "t" to
       type t = < m : float * int; n : int >
|}];;

module M4 : sig
  type t =
    | Foo of [`Foo of string | `Bar of string]
end = struct
  type t =
    | Foo of [`Bar of string]
end;;
[%%expect{|
Lines 4-7, characters 6-3:
4 | ......struct
5 |   type t =
6 |     | Foo of [`Bar of string]
7 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = Foo of [ `Bar of string ] end
       is not included in
         sig type t = Foo of [ `Bar of string | `Foo of string ] end
       Try changing type "t" to
       type t = Foo of [ `Bar of string | `Foo of string ]
|}];;

module M : sig
  type t = private [`C of int]
end = struct
  type t = private [`C]
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = private [`C]
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private [ `C ] end
       is not included in
         sig type t = private [ `C of int ] end
       Try changing type "t" to
       type t = private [ `C of int ]
|}];;

module M : sig
  type t = private [`C]
end = struct
  type t = private [`C of int]
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = private [`C of int]
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private [ `C of int ] end
       is not included in
         sig type t = private [ `C ] end
       Try changing type "t" to
       type t = private [ `C ]
|}];;

module M : sig
  type t = [`C of [< `A] | `C of [`A]]
end = struct
  type t = [`C of [< `A | `B] | `C of [`A]]
end;;
[%%expect{|
module M : sig type t = [ `C of [ `A ] ] end
|}];;

module M : sig
  type t = private [> `A of int]
end = struct
  type t = private [`A of int]
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = private [`A of int]
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private [ `A of int ] end
       is not included in
         sig type t = private [> `A of int ] end
       Try changing type "t" to
       type t = private [> `A of int ]
|}];;

module M : sig
  type t = private [`A of int]
end = struct
  type t = private [> `A of int]
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = private [> `A of int]
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private [> `A of int ] end
       is not included in
         sig type t = private [ `A of int ] end
       Try changing type "t" to
       type t = private [ `A of int ]
|}];;

module M : sig
  type 'a t =  [> `A of int | `B of int] as 'a
end = struct
  type 'a t =  [> `A of int] as 'a
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type 'a t =  [> `A of int] as 'a
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = 'a constraint 'a = [> `A of int ] end
       is not included in
         sig type 'a t = 'a constraint 'a = [> `A of int | `B of int ] end
       Try changing type "t" to
       type 'a t = 'a constraint 'a = [> `A of int | `B of int ]
|}];;

module M : sig
  type 'a t =  [> `A of int] as 'a
end = struct
  type 'a t =  [> `A of int | `C of float] as 'a
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type 'a t =  [> `A of int | `C of float] as 'a
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = 'a constraint 'a = [> `A of int | `C of float ] end
       is not included in
         sig type 'a t = 'a constraint 'a = [> `A of int ] end
       Try changing type "t" to
       type 'a t = 'a constraint 'a = [> `A of int ]
|}];;

module M : sig
  type t = [`C of [< `A | `B] | `C of [`A]]
end = struct
  type t = [`C of [< `A] | `C of [`A]]
end;;
[%%expect{|
module M : sig type t = [ `C of [ `A ] ] end
|}];;

module M : sig
  type t = private [< `C]
end = struct
  type t = private [< `C of int&float]
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = private [< `C of int&float]
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private [< `C of int & float ] end
       is not included in
         sig type t = private [< `C ] end
       Try changing type "t" to
       type t = private [< `C ]
|}];;

(********************************** Moregen ***********************************)

module type T = sig
  type t
end
module Int = struct
  type t = int
end
module type S = sig
  module Choice : T
  val r : Choice.t list ref ref
end
module Force (X : functor () -> S) = struct end
module Choose () = struct
  module Choice =
    (val (module Int : T))
  let r = ref (ref [])
end
module Ignore = Force(Choose)
[%%expect{|
module type T = sig type t end
module Int : sig type t = int end
module type S = sig module Choice : T val r : Choice.t list ref ref end
module Force : (X : () -> S) -> sig end
module Choose : () -> sig module Choice : T val r : '_weak1 list ref ref end
Line 17, characters 16-29:
17 | module Ignore = Force(Choose)
                     ^^^^^^^^^^^^^
Error: Modules do not match:
       () -> sig module Choice : T val r : '_weak1 list ref ref end
     is not included in () -> S
     Modules do not match:
       sig module Choice : T val r : '_weak1 list ref ref end
     is not included in
       S
     Try changing value "r" to be a "Choice.t list ref ref"
|}];;

module O = struct
  module type s
  module M: sig
    val f: (module s) -> unit
  end = struct
    module type s
    let f (module X:s) = ()
  end
end;;
[%%expect{|
Lines 5-8, characters 8-5:
5 | ........struct
6 |     module type s
7 |     let f (module X:s) = ()
8 |   end
Error: Signature mismatch:
       Modules do not match:
         sig module type s val f : (module s) -> unit end
       is not included in
         sig val f : (module s) -> unit end
       Try changing value "f" to be a "(module s/2) -> unit"
Line 6, characters 4-17:
  Definition of module type "s"
Line 2, characters 2-15:
  Definition of module type "s/2"
|}];;

module M : sig
  val f : (<m : 'b. ('b * <m: 'c. 'c * 'bar> as 'bar)>) -> unit
end = struct
  let f (x : <m : 'a. ('a * 'foo)> as 'foo) = ()
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f (x : <m : 'a. ('a * 'foo)> as 'foo) = ()
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val f : (< m : 'a. 'a * 'b > as 'b) -> unit end
       is not included in
         sig val f : < m : 'b. 'b * < m : 'c. 'c * 'a > as 'a > -> unit end
       Try changing value "f" to be a "< m : 'b. 'b * < m : 'c. 'c * 'a > as 'a > ->
                                    unit"
|}];;

type s = private < m : int; .. >;;

module M : sig
  val f : s -> s
end = struct
  let f (x : <m : int>) = x
end;;
[%%expect{|
type s = private < m : int; .. >
Lines 5-7, characters 6-3:
5 | ......struct
6 |   let f (x : <m : int>) = x
7 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val f : < m : int > -> < m : int > end
       is not included in
         sig val f : s -> s end
       Try changing value "f" to be a "s -> s"
|}];;

module M : sig
  val f : 'a -> float
end = struct
  let f : 'b -> int = fun _ -> 0
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f : 'b -> int = fun _ -> 0
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val f : 'b -> int end
       is not included in
         sig val f : 'a -> float end
       Try changing value "f" to be a "'a -> float"
|}]

module M : sig
  val x : 'a list ref
end = struct
  let x = ref []
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let x = ref []
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val x : '_weak2 list ref end
       is not included in
         sig val x : 'a list ref end
       Try changing value "x" to be a "'a list ref"
|}];;

module M = struct let r = ref [] end;;
type t;;
module N : sig val r : t list ref end = M;;
[%%expect{|
module M : sig val r : '_weak3 list ref end
type t
Line 3, characters 40-41:
3 | module N : sig val r : t list ref end = M;;
                                            ^
Error: Signature mismatch:
       Modules do not match:
         sig val r : '_weak3 list ref end
       is not included in
         sig val r : t list ref end
       Try changing value "r" to be a "t list ref"
|}];;

type (_, _) eq = Refl : ('a, 'a) eq;;

module T : sig
  type t
  type s
  val eq : (t, s) eq
end = struct
  type t = int
  type s = int
  let eq = Refl
end;;

module M = struct let r = ref [] end;;

let foo p (e : (T.t, T.s) eq) (x : T.t) (y : T.s) =
  match e with
  | Refl ->
    let z = if p then x else y in
    let module N = struct
      module type S = module type of struct let r = ref [z] end
    end in
    let module O : N.S = M in
    ();;
[%%expect{|
type (_, _) eq = Refl : ('a, 'a) eq
module T : sig type t type s val eq : (t, s) eq end
module M : sig val r : '_weak4 list ref end
Line 22, characters 25-26:
22 |     let module O : N.S = M in
                              ^
Error: Signature mismatch:
       Modules do not match:
         sig val r : '_weak4 list ref end
       is not included in
         N.S
       Try changing value "r" to be a "T.t list ref"
|}];;

module M: sig
  val f : int -> float
end = struct
  let f (x : 'a) = x
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f (x : 'a) = x
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val f : 'a -> 'a end
       is not included in
         sig val f : int -> float end
       Try changing value "f" to be a "int -> float"
|}];;

module M: sig
  val f : (int * float * int) -> (int -> int)
end = struct
  let f (x : (int * int)) = x
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f (x : (int * int)) = x
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val f : int * int -> int * int end
       is not included in
         sig val f : int * float * int -> int -> int end
       Try changing value "f" to be a "int * float * int -> int -> int"
|}];;

module M: sig
  val f : <m : int; n : float> -> <m : int; n : float>
end = struct
  let f (x : <m : int; f : float>) = x
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f (x : <m : int; f : float>) = x
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val f : < f : float; m : int > -> < f : float; m : int > end
       is not included in
         sig val f : < m : int; n : float > -> < m : int; n : float > end
       Try changing value "f" to be a "< m : int; n : float > ->
                                    < m : int; n : float >"
|}];;

module M : sig
  val f : [`Foo] -> unit
end = struct
  let f (x : [ `Foo | `Bar]) = ()
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f (x : [ `Foo | `Bar]) = ()
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val f : [ `Bar | `Foo ] -> unit end
       is not included in
         sig val f : [ `Foo ] -> unit end
       Try changing value "f" to be a "[ `Foo ] -> unit"
|}];;

module M : sig
  val f : [>`Foo] -> unit
end = struct
  let f (x : [< `Foo]) = ()
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f (x : [< `Foo]) = ()
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val f : [< `Foo ] -> unit end
       is not included in
         sig val f : [> `Foo ] -> unit end
       Try changing value "f" to be a "[> `Foo ] -> unit"
|}];;

module M : sig
  val f : [< `Foo | `Bar] -> unit
end = struct
  let f (x : [< `Foo]) = ()
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f (x : [< `Foo]) = ()
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val f : [< `Foo ] -> unit end
       is not included in
         sig val f : [< `Bar | `Foo ] -> unit end
       Try changing value "f" to be a "[< `Bar | `Foo ] -> unit"
|}];;

module M : sig
  val f : < m : [< `Foo]> -> unit
end = struct
  let f (x : < m : 'a. [< `Foo] as 'a >) = ()
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f (x : < m : 'a. [< `Foo] as 'a >) = ()
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val f : < m : 'a. [< `Foo ] as 'a > -> unit end
       is not included in
         sig val f : < m : [< `Foo ] > -> unit end
       Try changing value "f" to be a "< m : [< `Foo ] > -> unit"
|}];;

module M : sig
  val f : < m : 'a. [< `Foo] as 'a > -> unit
end = struct
  let f (x : < m : [`Foo]>) = ()
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f (x : < m : [`Foo]>) = ()
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val f : < m : [ `Foo ] > -> unit end
       is not included in
         sig val f : < m : 'a. [< `Foo ] as 'a > -> unit end
       Try changing value "f" to be a "< m : 'a. [< `Foo ] as 'a > -> unit"
|}];;

module M : sig
  val f : [< `C] -> unit
end = struct
  let f (x : [< `C of int&float]) = ()
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f (x : [< `C of int&float]) = ()
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val f : [< `C of int & float ] -> unit end
       is not included in
         sig val f : [< `C ] -> unit end
       Try changing value "f" to be a "[< `C ] -> unit"
|}];;

module M : sig
  val f : [`Foo] -> unit
end = struct
  let f (x : [`Foo of int]) = ()
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f (x : [`Foo of int]) = ()
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val f : [ `Foo of int ] -> unit end
       is not included in
         sig val f : [ `Foo ] -> unit end
       Try changing value "f" to be a "[ `Foo ] -> unit"
|}];;

module M : sig
  val f : [`Foo of int] -> unit
end = struct
  let f (x : [`Foo]) = ()
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f (x : [`Foo]) = ()
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val f : [ `Foo ] -> unit end
       is not included in
         sig val f : [ `Foo of int ] -> unit end
       Try changing value "f" to be a "[ `Foo of int ] -> unit"
|}];;

module M : sig
  val f : [< `Foo | `Bar | `Baz] -> unit
end = struct
  let f (x : [< `Foo | `Bar | `Baz]) = ()
end;;
[%%expect{|
module M : sig val f : [< `Bar | `Baz | `Foo ] -> unit end
|}];;

module M : sig
  val f : [< `Foo | `Bar | `Baz] -> unit
end = struct
  let f (x : [> `Foo | `Bar]) = ()
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f (x : [> `Foo | `Bar]) = ()
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val f : [> `Bar | `Foo ] -> unit end
       is not included in
         sig val f : [< `Bar | `Baz | `Foo ] -> unit end
       Try changing value "f" to be a "[< `Bar | `Baz | `Foo ] -> unit"
|}];;

(******************************* Type manifests *******************************)

module M : sig
  type t = private [< `A | `B]
end = struct
  type t = [`C]
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = [`C]
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = [ `C ] end
       is not included in
         sig type t = private [< `A | `B ] end
       Try changing type "t" to
       type t = private [< `A | `B ]
|}];;

module M : sig
  type t = private [< `A | `B]
end = struct
  type t = private [> `A]
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = private [> `A]
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private [> `A ] end
       is not included in
         sig type t = private [< `A | `B ] end
       Try changing type "t" to
       type t = private [< `A | `B ]
|}];;

module M : sig
  type t = private [< `A | `B > `A]
end = struct
  type t = [`B]
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = [`B]
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = [ `B ] end
       is not included in
         sig type t = private [< `A | `B > `A ] end
       Try changing type "t" to
       type t = private [< `A | `B > `A ]
|}];;

module M : sig
  type t = private [> `A of int]
end = struct
  type t = [`A]
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = [`A]
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = [ `A ] end
       is not included in
         sig type t = private [> `A of int ] end
       Try changing type "t" to
       type t = private [> `A of int ]
|}];;

module M : sig
   type t = private [< `A of int]
end = struct
   type t = private [< `A of & int]
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |    type t = private [< `A of & int]
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private [< `A of & int ] end
       is not included in
         sig type t = private [< `A of int ] end
       Try changing type "t" to
       type t = private [< `A of int ]
|}];;


module M : sig
  type t = private [< `A of int]
end = struct
  type t = private [< `A]
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = private [< `A]
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private [< `A ] end
       is not included in
         sig type t = private [< `A of int ] end
       Try changing type "t" to
       type t = private [< `A of int ]
|}];;


module M : sig
  type t = private [< `A of int & float]
end = struct
  type t = private [< `A]
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = private [< `A]
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private [< `A ] end
       is not included in
         sig type t = private [< `A of int & float ] end
       Try changing type "t" to
       type t = private [< `A of int & float ]
|}];;

module M : sig
  type t = private [> `A of int]
end = struct
  type t = [`A of float]
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = [`A of float]
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = [ `A of float ] end
       is not included in
         sig type t = private [> `A of int ] end
       Try changing type "t" to
       type t = private [> `A of int ]
|}];;

module M : sig
  type t = private [< `A | `B]
end = struct
  type t = private [`A | `B]
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = private [`A | `B]
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private [ `A | `B ] end
       is not included in
         sig type t = private [< `A | `B ] end
       Try changing type "t" to
       type t = private [< `A | `B ]
|}];;

module M : sig
  type t = [`A | `B]
end = struct
  type t = private [`A | `B]
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = private [`A | `B]
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private [ `A | `B ] end
       is not included in
         sig type t = [ `A | `B ] end
       Try changing type "t" to
       type t = [ `A | `B ]
|}];;

module M : sig
  type t = private [< `A | `B > `B]
end = struct
  type t = private [< `A | `B]
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = private [< `A | `B]
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private [< `A | `B ] end
       is not included in
         sig type t = private [< `A | `B > `B ] end
       Try changing type "t" to
       type t = private [< `A | `B > `B ]
|}];;

module M : sig
  type t = private <a : int; ..>
end = struct
  type t = <b : int>
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = <b : int>
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = < b : int > end
       is not included in
         sig type t = private < a : int; .. > end
       Try changing type "t" to
       type t = private < a : int; .. >
|}];;

module M : sig
  type t = private <a : float; ..>
end = struct
  type t = <a : int>
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = <a : int>
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = < a : int > end
       is not included in
         sig type t = private < a : float; .. > end
       Try changing type "t" to
       type t = private < a : float; .. >
|}];;

type w = private float
type q = private (int * w)
type u = private (int * q)
module M : sig (* Confusing error message :( *)
  type t = private (int * (int * int))
end = struct
  type t = private u
end;;
[%%expect{|
type w = private float
type q = private int * w
type u = private int * q
Lines 6-8, characters 6-3:
6 | ......struct
7 |   type t = private u
8 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private u end
       is not included in
         sig type t = private int * (int * int) end
       Try changing type "t" to
       type t = private int * (int * int)
|}];;

type w = float
type q = (int * w)
type u = private (int * q)
module M : sig
  type t = private (int * (int * int))
end = struct
  type t = private u
end;;
[%%expect{|
type w = float
type q = int * w
type u = private int * q
Lines 6-8, characters 6-3:
6 | ......struct
7 |   type t = private u
8 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private u end
       is not included in
         sig type t = private int * (int * int) end
       Try changing type "t" to
       type t = private int * (int * int)
|}];;

type s = private int

module M : sig
  type t = private float
end = struct
  type t = private s
end;;
[%%expect{|
type s = private int
Lines 5-7, characters 6-3:
5 | ......struct
6 |   type t = private s
7 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private s end
       is not included in
         sig type t = private float end
       Try changing type "t" to
       type t = private float
|}];;

module M : sig
  type t = A
end = struct
  type t = private A
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = private A
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private A end
       is not included in
         sig type t = A end
       Try changing type "t" to
       type t = A
|}];;

module M : sig
  type t = A | B
end = struct
  type t = private A | B
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = private A | B
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private A | B end
       is not included in
         sig type t = A | B end
       Try changing type "t" to
       type t = A | B
|}];;

module M : sig
  type t = A of { x : int; y : bool }
end = struct
  type t = private A of { x : int; y : bool }
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = private A of { x : int; y : bool }
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private A of { x : int; y : bool; } end
       is not included in
         sig type t = A of { x : int; y : bool; } end
       Try changing type "t" to
       type t = A of { x : int; y : bool; }
|}];;

module M : sig
  type t = { x : int; y : bool }
end = struct
  type t = private { x : int; y : bool }
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = private { x : int; y : bool }
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private { x : int; y : bool; } end
       is not included in
         sig type t = { x : int; y : bool; } end
       Try changing type "t" to
       type t = { x : int; y : bool; }
|}];;

module M : sig
  type t = A
end = struct
  type t = private A | B
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = private A | B
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private A | B end
       is not included in
         sig type t = A end
       Try changing type "t" to
       type t = A
|}];;

module M : sig
  type t = A | B
end = struct
  type t = private A
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = private A
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private A end
       is not included in
         sig type t = A | B end
       Try changing type "t" to
       type t = A | B
|}];;

module M : sig
  type t = { x : int }
end = struct
  type t = private { x : int; y : bool }
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = private { x : int; y : bool }
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private { x : int; y : bool; } end
       is not included in
         sig type t = { x : int; } end
       Try changing type "t" to
       type t = { x : int; }
|}];;

module M : sig
  type t = { x : int; y : bool }
end = struct
  type t = private { x : int }
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = private { x : int }
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private { x : int; } end
       is not included in
         sig type t = { x : int; y : bool; } end
       Try changing type "t" to
       type t = { x : int; y : bool; }
|}];;

module M : sig
  type t = A | B
end = struct
  type t = private { x : int; y : bool }
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = private { x : int; y : bool }
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private { x : int; y : bool; } end
       is not included in
         sig type t = A | B end
       Try changing type "t" to
       type t = A | B
|}];;

module M : sig
  type t = { x : int; y : bool }
end = struct
  type t = private A | B
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = private A | B
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private A | B end
       is not included in
         sig type t = { x : int; y : bool; } end
       Try changing type "t" to
       type t = { x : int; y : bool; }
|}];;

module M : sig
  type t = [`A]
end = struct
  type t = private [> `A | `B]
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = private [> `A | `B]
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private [> `A | `B ] end
       is not included in
         sig type t = [ `A ] end
       Try changing type "t" to
       type t = [ `A ]
|}];;

module M : sig
  type t = [`A]
end = struct
  type t = private [< `A | `B]
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = private [< `A | `B]
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private [< `A | `B ] end
       is not included in
         sig type t = [ `A ] end
       Try changing type "t" to
       type t = [ `A ]
|}];;

module M : sig
  type t = [`A]
end = struct
  type t = private [< `A | `B > `A]
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = private [< `A | `B > `A]
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private [< `A | `B > `A ] end
       is not included in
         sig type t = [ `A ] end
       Try changing type "t" to
       type t = [ `A ]
|}];;

module M : sig
  type t = < m : int >
end = struct
  type t = private < m : int; .. >
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = private < m : int; .. >
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private < m : int; .. > end
       is not included in
         sig type t = < m : int > end
       Try changing type "t" to
       type t = < m : int >
|}];;


(** Unexpected recursive types *)
module M: sig
  type _ t = A : (<x:'a> as 'a) -> (<y:'b> as 'b) t
end = struct
  type _ t = A : (<x:'a * 'a> as 'a) -> (<y:'b> as 'b) t
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type _ t = A : (<x:'a * 'a> as 'a) -> (<y:'b> as 'b) t
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           type _ t = A : (< x : 'b * 'b > as 'b) -> (< y : 'a > as 'a) t
         end
       is not included in
         sig type _ t = A : (< x : 'b > as 'b) -> (< y : 'a > as 'a) t end
       Try changing type "t" to
       type _ t = A : (< x : 'b > as 'b) -> (< y : 'a > as 'a) t
|}]
module R: sig
  type t = { a: (<x:'a> as 'a) }
end = struct
  type t = { a: (<x:'a * 'a> as 'a) }
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = { a: (<x:'a * 'a> as 'a) }
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = { a : < x : 'a * 'a > as 'a; } end
       is not included in
         sig type t = { a : < x : 'a > as 'a; } end
       Try changing type "t" to
       type t = { a : < x : 'a > as 'a; }
|}]
type _ ext = ..
module Ext: sig
  type _ ext += A : (<x:'a> as 'a) -> (<y:'b> as 'b) ext
end = struct
  type _ ext  += A : (<x:'a * 'a> as 'a) -> (<y:'b> as 'b) ext
end
[%%expect {|
type _ ext = ..
Lines 4-6, characters 6-3:
4 | ......struct
5 |   type _ ext  += A : (<x:'a * 'a> as 'a) -> (<y:'b> as 'b) ext
6 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           type _ ext +=
               A : (< x : 'b * 'b > as 'b) -> (< y : 'a > as 'a) ext
         end
       is not included in
         sig
           type _ ext += A : (< x : 'b > as 'b) -> (< y : 'a > as 'a) ext
         end
|}]

(********************************** Nested modules ****************************)

(* This test ensures that the error messages for missing bindings
   are printed in a logical order, with "In module N:" mentioned
   in sensible places.
 *)

module M : sig
  module N : sig
    type t
    val x : t
    val y : t
  end
end = struct
  module N = struct
  end
end

[%%expect {|
Lines 7-10, characters 6-3:
 7 | ......struct
 8 |   module N = struct
 9 |   end
10 | end
Error: Signature mismatch:
       Modules do not match:
         sig module N : sig end end
       is not included in
         sig module N : sig type t val x : t val y : t end end
       Try changing module "N" to be a
       sig type t val x : t val y : t end
|}];;


module Eq_label: sig
  type t = int -> int
end = struct
  type t = x:int -> int
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = x:int -> int
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = x:int -> int end
       is not included in
         sig type t = int -> int end
       Try changing type "t" to
       type t = int -> int
|}]

module Eq_label2: sig
  type t = y:int -> int
end = struct
  type t = x:int -> int
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = x:int -> int
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = x:int -> int end
       is not included in
         sig type t = y:int -> int end
       Try changing type "t" to
       type t = y:int -> int
|}]

module Label1 : sig
  val f: int -> unit
end = struct
  let f ~x = ()
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f ~x = ()
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : x:'a -> unit end
       is not included in
         sig val f : int -> unit end
       Try changing value "f" to be a "int -> unit"
|}]

module Label2 : sig
  val f: int -> unit
end = struct
  let f ?x = ()
end
[%%expect {|
Line 4, characters 9-10:
4 |   let f ?x = ()
             ^
Warning 16 [unerasable-optional-argument]: this optional argument cannot be erased.

Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f ?x = ()
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : ?x:'a -> unit end
       is not included in
         sig val f : int -> unit end
       Try changing value "f" to be a "int -> unit"
|}]


module Label3 : sig
  val f: x:int -> unit
end = struct
  let f ?x = ()
end
[%%expect {|
Line 4, characters 9-10:
4 |   let f ?x = ()
             ^
Warning 16 [unerasable-optional-argument]: this optional argument cannot be erased.

Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f ?x = ()
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : ?x:'a -> unit end
       is not included in
         sig val f : x:int -> unit end
       Try changing value "f" to be a "x:int -> unit"
|}]


module Label4 : sig
  val f: ?x:int -> unit
end = struct
  let f ?y = ()
end
[%%expect {|
Line 4, characters 9-10:
4 |   let f ?y = ()
             ^
Warning 16 [unerasable-optional-argument]: this optional argument cannot be erased.

Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f ?y = ()
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : ?y:'a -> unit end
       is not included in
         sig val f : ?x:int -> unit end
       Try changing value "f" to be a "?x:int -> unit"
|}]


module Label5 : sig
  val f: ?x:int -> unit
end = struct
  let f x = ()
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f x = ()
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : 'a -> unit end
       is not included in
         sig val f : ?x:int -> unit end
       Try changing value "f" to be a "?x:int -> unit"
|}]
