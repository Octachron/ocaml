(* TEST
 flags = "-dlambda -dcanonical-ids";
 expect;
*)

(* We explicitly enable the warning (see the discussion in the
   "Warning reference" section of the reference manual), which makes
   it clear which examples have been intentionally pessimized by the
   compiler. *)
#warnings "+degraded-to-partial-match";;
[%%expect {|
|}];;

(* The original example of unsoundness in #7421. *)
type t = {a: bool; mutable b: int option}

let f x =
  match x with
  | {a = false; b = _} -> 0
  | {a = _;     b = None} -> 1
  | {a = _;     b = _} when (x.b <- None; false) -> 2
  | {a = true;  b = Some y} -> y
;;
(* Correctness condition: there should either be a single
   (field_mut 1) access, or the second access should include
   a Match_failure case.

   PASS: the second access includes a Match_failure case. *)
[%%expect {|
0
type t = { a : bool; mutable b : int option; }
Lines 4-8, characters 2-32:
4 | ..match x with
5 |   | {a = false; b = _} -> 0
6 |   | {a = _;     b = None} -> 1
7 |   | {a = _;     b = _} when (x.b <- None; false) -> 2
8 |   | {a = true;  b = Some y} -> y
Warning 74 [degraded-to-partial-match]: This pattern-matching is compiled as
  partial, even if it appears to be total. It may generate a "Match_failure"
  exception. This typically occurs due to complex matches on mutable fields.
  (see manual section 13.5.5)
(let
  (f/0 =
     (function x/0 : int
       (if (field_int 0 x/0)
         (let (*match*/0 =o (field_mut 1 x/0))
           (if *match*/0
             (if (seq (setfield_ptr 1 x/0 0) 0) 2
               (let (*match*/1 =o (field_mut 1 x/0))
                 (if *match*/1 (field_imm 0 *match*/1)
                   (raise
                     (makeblock 0 (global Match_failure/0!) [0: "" 4 2])))))
             1))
         0)))
  (apply (field_mut 1 (global Toploop!)) "f" f/0))

val f : t -> int = <fun>
|}, Principal{|
0
type t = { a : bool; mutable b : int option; }
Lines 4-8, characters 2-32:
4 | ..match x with
5 |   | {a = false; b = _} -> 0
6 |   | {a = _;     b = None} -> 1
7 |   | {a = _;     b = _} when (x.b <- None; false) -> 2
8 |   | {a = true;  b = Some y} -> y
Warning 74 [degraded-to-partial-match]: This pattern-matching is compiled as
  partial, even if it appears to be total. It may generate a "Match_failure"
  exception. This typically occurs due to complex matches on mutable fields.
  (see manual section 13.5.5)
(let
  (f/1 =
     (function x/1 : int
       (if (field_int 0 x/1)
         (let (*match*/2 =o (field_mut 1 x/1))
           (if *match*/2
             (if (seq (setfield_ptr 1 x/1 0) 0) 2
               (let (*match*/3 =o (field_mut 1 x/1))
                 (if *match*/3 (field_imm 0 *match*/3)
                   (raise
                     (makeblock 0 (global Match_failure/0!) [0: "" 4 2])))))
             1))
         0)))
  (apply (field_mut 1 (global Toploop!)) "f" f/1))

val f : t -> int = <fun>
|}, Rectypes{|
0
type t = { a : bool; mutable b : int option; }
Lines 4-8, characters 2-32:
4 | ..match x with
5 |   | {a = false; b = _} -> 0
6 |   | {a = _;     b = None} -> 1
7 |   | {a = _;     b = _} when (x.b <- None; false) -> 2
8 |   | {a = true;  b = Some y} -> y
Warning 74 [degraded-to-partial-match]: This pattern-matching is compiled as
  partial, even if it appears to be total. It may generate a "Match_failure"
  exception. This typically occurs due to complex matches on mutable fields.
  (see manual section 13.5.5)
(let
  (f/2 =
     (function x/2 : int
       (if (field_int 0 x/2)
         (let (*match*/4 =o (field_mut 1 x/2))
           (if *match*/4
             (if (seq (setfield_ptr 1 x/2 0) 0) 2
               (let (*match*/5 =o (field_mut 1 x/2))
                 (if *match*/5 (field_imm 0 *match*/5)
                   (raise
                     (makeblock 0 (global Match_failure/0!) [0: "" 4 2])))))
             1))
         0)))
  (apply (field_mut 1 (global Toploop!)) "f" f/2))

val f : t -> int = <fun>
|}]



(* A simple example of a complete switch
   inside a mutable position. *)
type t = {a: bool; mutable b: int option}

let simple x =
  match x with
  | {b = None} -> 1
  | {b = Some y} -> y
;;
(* Performance expectation: there should not be a Match_failure case. *)
[%%expect {|
0
type t = { a : bool; mutable b : int option; }
(let
  (simple/0 =
     (function x/3 : int
       (let (*match*/6 =o (field_mut 1 x/3))
         (if *match*/6 (field_imm 0 *match*/6) 1))))
  (apply (field_mut 1 (global Toploop!)) "simple" simple/0))
val simple : t -> int = <fun>
|}, Principal{|
0
type t = { a : bool; mutable b : int option; }
(let
  (simple/1 =
     (function x/4 : int
       (let (*match*/7 =o (field_mut 1 x/4))
         (if *match*/7 (field_imm 0 *match*/7) 1))))
  (apply (field_mut 1 (global Toploop!)) "simple" simple/1))
val simple : t -> int = <fun>
|}, Rectypes{|
0
type t = { a : bool; mutable b : int option; }
(let
  (simple/2 =
     (function x/5 : int
       (let (*match*/8 =o (field_mut 1 x/5))
         (if *match*/8 (field_imm 0 *match*/8) 1))))
  (apply (field_mut 1 (global Toploop!)) "simple" simple/2))
val simple : t -> int = <fun>
|}]

(* This more complex case has the switch on [b] split across two cases
   on [a], so it may need a [Match_failure] for soundness -- it does
   if the two accesses to [b] are done on different reads of the same
   mutable field.

   PASS: two reads of [field_mut 1 x], and a Match_failure case. *)
let f x =
  match x with
  | {a = false; b = _} -> 0
  | {a = _;     b = None} -> 1
  | {a = true;  b = Some y} -> y
;;
[%%expect {|
Lines 2-5, characters 2-32:
2 | ..match x with
3 |   | {a = false; b = _} -> 0
4 |   | {a = _;     b = None} -> 1
5 |   | {a = true;  b = Some y} -> y
Warning 74 [degraded-to-partial-match]: This pattern-matching is compiled as
  partial, even if it appears to be total. It may generate a "Match_failure"
  exception. This typically occurs due to complex matches on mutable fields.
  (see manual section 13.5.5)
(let
  (f/3 =
     (function x/6 : int
       (if (field_int 0 x/6)
         (let (*match*/9 =o (field_mut 1 x/6))
           (if *match*/9 (field_imm 0 *match*/9)
             (let (*match*/10 =o (field_mut 1 x/6))
               (if *match*/10
                 (raise (makeblock 0 (global Match_failure/0!) [0: "" 2 2]))
                 1))))
         0)))
  (apply (field_mut 1 (global Toploop!)) "f" f/3))

val f : t -> int = <fun>
|}, Principal{|
Lines 2-5, characters 2-32:
2 | ..match x with
3 |   | {a = false; b = _} -> 0
4 |   | {a = _;     b = None} -> 1
5 |   | {a = true;  b = Some y} -> y
Warning 74 [degraded-to-partial-match]: This pattern-matching is compiled as
  partial, even if it appears to be total. It may generate a "Match_failure"
  exception. This typically occurs due to complex matches on mutable fields.
  (see manual section 13.5.5)
(let
  (f/4 =
     (function x/7 : int
       (if (field_int 0 x/7)
         (let (*match*/11 =o (field_mut 1 x/7))
           (if *match*/11 (field_imm 0 *match*/11)
             (let (*match*/12 =o (field_mut 1 x/7))
               (if *match*/12
                 (raise (makeblock 0 (global Match_failure/0!) [0: "" 2 2]))
                 1))))
         0)))
  (apply (field_mut 1 (global Toploop!)) "f" f/4))

val f : t -> int = <fun>
|}, Rectypes{|
Lines 2-5, characters 2-32:
2 | ..match x with
3 |   | {a = false; b = _} -> 0
4 |   | {a = _;     b = None} -> 1
5 |   | {a = true;  b = Some y} -> y
Warning 74 [degraded-to-partial-match]: This pattern-matching is compiled as
  partial, even if it appears to be total. It may generate a "Match_failure"
  exception. This typically occurs due to complex matches on mutable fields.
  (see manual section 13.5.5)
(let
  (f/5 =
     (function x/8 : int
       (if (field_int 0 x/8)
         (let (*match*/13 =o (field_mut 1 x/8))
           (if *match*/13 (field_imm 0 *match*/13)
             (let (*match*/14 =o (field_mut 1 x/8))
               (if *match*/14
                 (raise (makeblock 0 (global Match_failure/0!) [0: "" 2 2]))
                 1))))
         0)))
  (apply (field_mut 1 (global Toploop!)) "f" f/5))

val f : t -> int = <fun>
|}]



(* A variant of the #7421 example. *)
let f r =
  match Some r with
  | Some { contents = None } -> 0
  | _ when (r := None; false) -> 1
  | Some { contents = Some n } -> n
  | None -> 3
;;
(* Correctness condition: there should either be a single
   (field_mut 0) access, or the second access should include
   a Match_failure case.

   PASS: two different reads (field_mut 0), and a Match_failure case. *)
[%%expect {|
Lines 2-6, characters 2-13:
2 | ..match Some r with
3 |   | Some { contents = None } -> 0
4 |   | _ when (r := None; false) -> 1
5 |   | Some { contents = Some n } -> n
6 |   | None -> 3
Warning 74 [degraded-to-partial-match]: This pattern-matching is compiled as
  partial, even if it appears to be total. It may generate a "Match_failure"
  exception. This typically occurs due to complex matches on mutable fields.
  (see manual section 13.5.5)
(let
  (f/6 =
     (function r/0 : int
       (let (*match*/15 = (makeblock 0 r/0))
         (catch
           (if *match*/15
             (let (*match*/16 =o (field_mut 0 (field_imm 0 *match*/15)))
               (if *match*/16 (exit 33) 0))
             (exit 33))
          with (33)
           (if (seq (setfield_ptr 0 r/0 0) 0) 1
             (if *match*/15
               (let (*match*/17 =o (field_mut 0 (field_imm 0 *match*/15)))
                 (if *match*/17 (field_imm 0 *match*/17)
                   (raise
                     (makeblock 0 (global Match_failure/0!) [0: "" 2 2]))))
               3))))))
  (apply (field_mut 1 (global Toploop!)) "f" f/6))

val f : int option ref -> int = <fun>
|}, Principal{|
Lines 2-6, characters 2-13:
2 | ..match Some r with
3 |   | Some { contents = None } -> 0
4 |   | _ when (r := None; false) -> 1
5 |   | Some { contents = Some n } -> n
6 |   | None -> 3
Warning 74 [degraded-to-partial-match]: This pattern-matching is compiled as
  partial, even if it appears to be total. It may generate a "Match_failure"
  exception. This typically occurs due to complex matches on mutable fields.
  (see manual section 13.5.5)
(let
  (f/7 =
     (function r/1 : int
       (let (*match*/18 = (makeblock 0 r/1))
         (catch
           (if *match*/18
             (let (*match*/19 =o (field_mut 0 (field_imm 0 *match*/18)))
               (if *match*/19 (exit 37) 0))
             (exit 37))
          with (37)
           (if (seq (setfield_ptr 0 r/1 0) 0) 1
             (if *match*/18
               (let (*match*/20 =o (field_mut 0 (field_imm 0 *match*/18)))
                 (if *match*/20 (field_imm 0 *match*/20)
                   (raise
                     (makeblock 0 (global Match_failure/0!) [0: "" 2 2]))))
               3))))))
  (apply (field_mut 1 (global Toploop!)) "f" f/7))

val f : int option ref -> int = <fun>
|}, Rectypes{|
Lines 2-6, characters 2-13:
2 | ..match Some r with
3 |   | Some { contents = None } -> 0
4 |   | _ when (r := None; false) -> 1
5 |   | Some { contents = Some n } -> n
6 |   | None -> 3
Warning 74 [degraded-to-partial-match]: This pattern-matching is compiled as
  partial, even if it appears to be total. It may generate a "Match_failure"
  exception. This typically occurs due to complex matches on mutable fields.
  (see manual section 13.5.5)
(let
  (f/8 =
     (function r/2 : int
       (let (*match*/21 = (makeblock 0 r/2))
         (catch
           (if *match*/21
             (let (*match*/22 =o (field_mut 0 (field_imm 0 *match*/21)))
               (if *match*/22 (exit 41) 0))
             (exit 41))
          with (41)
           (if (seq (setfield_ptr 0 r/2 0) 0) 1
             (if *match*/21
               (let (*match*/23 =o (field_mut 0 (field_imm 0 *match*/21)))
                 (if *match*/23 (field_imm 0 *match*/23)
                   (raise
                     (makeblock 0 (global Match_failure/0!) [0: "" 2 2]))))
               3))))))
  (apply (field_mut 1 (global Toploop!)) "f" f/8))

val f : int option ref -> int = <fun>
|}]



(* This example has an ill-typed counter-example: the type-checker
   finds it Total, but the pattern-matching compiler cannot see that
   (Some (Some (Bool b))) cannot occur. *)
type _ t = Int : int -> int t | Bool : bool -> bool t

let test = function
  | None -> 0
  | Some (Int n) -> n
;;
(* Performance expectation: there should not be a Match_failure case. *)
[%%expect {|
0
type _ t = Int : int -> int t | Bool : bool -> bool t
(let
  (test/0 =
     (function param/0 : int
       (if param/0 (field_imm 0 (field_imm 0 param/0)) 0)))
  (apply (field_mut 1 (global Toploop!)) "test" test/0))
val test : int t option -> int = <fun>
|}, Principal{|
0
type _ t = Int : int -> int t | Bool : bool -> bool t
(let
  (test/1 =
     (function param/1 : int
       (if param/1 (field_imm 0 (field_imm 0 param/1)) 0)))
  (apply (field_mut 1 (global Toploop!)) "test" test/1))
val test : int t option -> int = <fun>
|}, Rectypes{|
0
type _ t = Int : int -> int t | Bool : bool -> bool t
(let
  (test/2 =
     (function param/2 : int
       (if param/2 (field_imm 0 (field_imm 0 param/2)) 0)))
  (apply (field_mut 1 (global Toploop!)) "test" test/2))
val test : int t option -> int = <fun>
|}]


(* This example has an ill-typed counter-example, inside
   a mutable position.  *)
type _ t = Int : int -> int t | Bool : bool -> bool t

let test = function
  | { contents = None } -> 0
  | { contents = Some (Int n) } -> n
;;
(* Performance expectation: there should not be a Match_failure case. *)
[%%expect {|
0
type _ t = Int : int -> int t | Bool : bool -> bool t
(let
  (test/3 =
     (function param/3 : int
       (let (*match*/24 =o (field_mut 0 param/3))
         (if *match*/24 (field_imm 0 (field_imm 0 *match*/24)) 0))))
  (apply (field_mut 1 (global Toploop!)) "test" test/3))
val test : int t option ref -> int = <fun>
|}, Principal{|
0
type _ t = Int : int -> int t | Bool : bool -> bool t
(let
  (test/4 =
     (function param/4 : int
       (let (*match*/25 =o (field_mut 0 param/4))
         (if *match*/25 (field_imm 0 (field_imm 0 *match*/25)) 0))))
  (apply (field_mut 1 (global Toploop!)) "test" test/4))
val test : int t option ref -> int = <fun>
|}, Rectypes{|
0
type _ t = Int : int -> int t | Bool : bool -> bool t
(let
  (test/5 =
     (function param/5 : int
       (let (*match*/26 =o (field_mut 0 param/5))
         (if *match*/26 (field_imm 0 (field_imm 0 *match*/26)) 0))))
  (apply (field_mut 1 (global Toploop!)) "test" test/5))
val test : int t option ref -> int = <fun>
|}]



(* This example has a ill-typed counter-example,
   and also mutable sub-patterns, but in different places. *)
type _ t = Int : int -> int t | Bool : bool -> bool t

let test n =
  match Some (ref true, Int 42) with
  | Some ({ contents = true }, Int n) -> n
  | Some ({ contents = false }, Int n) -> -n
  | None -> 3
;;
(* Performance expectation: there should not be a Match_failure case. *)
[%%expect {|
0
type _ t = Int : int -> int t | Bool : bool -> bool t
(let
  (test/6 =
     (function n/0 : int
       (let
         (*match*/27 =
            (makeblock 0 (makeblock 0 (makemutable 0 (int) 1) [0: 42])))
         (if *match*/27
           (let
             (*match*/28 =a (field_imm 0 *match*/27)
              *match*/29 =o (field_mut 0 (field_imm 0 *match*/28)))
             (if *match*/29 (field_imm 0 (field_imm 1 *match*/28))
               (~ (field_imm 0 (field_imm 1 *match*/28)))))
           3))))
  (apply (field_mut 1 (global Toploop!)) "test" test/6))
val test : 'a -> int = <fun>
|}, Principal{|
0
type _ t = Int : int -> int t | Bool : bool -> bool t
(let
  (test/7 =
     (function n/1 : int
       (let
         (*match*/30 =
            (makeblock 0 (makeblock 0 (makemutable 0 (int) 1) [0: 42])))
         (if *match*/30
           (let
             (*match*/31 =a (field_imm 0 *match*/30)
              *match*/32 =o (field_mut 0 (field_imm 0 *match*/31)))
             (if *match*/32 (field_imm 0 (field_imm 1 *match*/31))
               (~ (field_imm 0 (field_imm 1 *match*/31)))))
           3))))
  (apply (field_mut 1 (global Toploop!)) "test" test/7))
val test : 'a -> int = <fun>
|}, Rectypes{|
0
type _ t = Int : int -> int t | Bool : bool -> bool t
(let
  (test/8 =
     (function n/2 : int
       (let
         (*match*/33 =
            (makeblock 0 (makeblock 0 (makemutable 0 (int) 1) [0: 42])))
         (if *match*/33
           (let
             (*match*/34 =a (field_imm 0 *match*/33)
              *match*/35 =o (field_mut 0 (field_imm 0 *match*/34)))
             (if *match*/35 (field_imm 0 (field_imm 1 *match*/34))
               (~ (field_imm 0 (field_imm 1 *match*/34)))))
           3))))
  (apply (field_mut 1 (global Toploop!)) "test" test/8))
val test : 'a -> int = <fun>
|}]



(* In this example, the constructor on which unsound assumptions could
   be made is not located directly below a mutable constructor, but
   one level deeper inside an immutable pair constructor (below the
   mutable constructor). This checks that there is a form of
   "transitive" propagation of mutability.

   Correctness condition: either there is a single mutable field read,
   or the accesses below the second mutable read have a Match_failure
   case.
*)
let deep r =
  match Some r with
  | Some { contents = ((), None) } -> 0
  | _ when (r := ((), None); false) -> 1
  | Some { contents = ((), Some n) } -> n
  | None -> 3
;;
(* PASS: two different reads (field_mut 0), and a Match_failure case. *)
[%%expect {|
Lines 2-6, characters 2-13:
2 | ..match Some r with
3 |   | Some { contents = ((), None) } -> 0
4 |   | _ when (r := ((), None); false) -> 1
5 |   | Some { contents = ((), Some n) } -> n
6 |   | None -> 3
Warning 74 [degraded-to-partial-match]: This pattern-matching is compiled as
  partial, even if it appears to be total. It may generate a "Match_failure"
  exception. This typically occurs due to complex matches on mutable fields.
  (see manual section 13.5.5)
(let
  (deep/0 =
     (function r/3 : int
       (let (*match*/36 = (makeblock 0 r/3))
         (catch
           (if *match*/36
             (let (*match*/37 =o (field_mut 0 (field_imm 0 *match*/36)))
               (if (field_imm 1 *match*/37) (exit 57) 0))
             (exit 57))
          with (57)
           (if (seq (setfield_ptr 0 r/3 [0: 0 0]) 0) 1
             (if *match*/36
               (let
                 (*match*/38 =o (field_mut 0 (field_imm 0 *match*/36))
                  *match*/39 =a (field_imm 1 *match*/38))
                 (if *match*/39 (field_imm 0 *match*/39)
                   (raise
                     (makeblock 0 (global Match_failure/0!) [0: "" 2 2]))))
               3))))))
  (apply (field_mut 1 (global Toploop!)) "deep" deep/0))

val deep : (unit * int option) ref -> int = <fun>
|}, Principal{|
Lines 2-6, characters 2-13:
2 | ..match Some r with
3 |   | Some { contents = ((), None) } -> 0
4 |   | _ when (r := ((), None); false) -> 1
5 |   | Some { contents = ((), Some n) } -> n
6 |   | None -> 3
Warning 74 [degraded-to-partial-match]: This pattern-matching is compiled as
  partial, even if it appears to be total. It may generate a "Match_failure"
  exception. This typically occurs due to complex matches on mutable fields.
  (see manual section 13.5.5)
(let
  (deep/1 =
     (function r/4 : int
       (let (*match*/40 = (makeblock 0 r/4))
         (catch
           (if *match*/40
             (let (*match*/41 =o (field_mut 0 (field_imm 0 *match*/40)))
               (if (field_imm 1 *match*/41) (exit 61) 0))
             (exit 61))
          with (61)
           (if (seq (setfield_ptr 0 r/4 [0: 0 0]) 0) 1
             (if *match*/40
               (let
                 (*match*/42 =o (field_mut 0 (field_imm 0 *match*/40))
                  *match*/43 =a (field_imm 1 *match*/42))
                 (if *match*/43 (field_imm 0 *match*/43)
                   (raise
                     (makeblock 0 (global Match_failure/0!) [0: "" 2 2]))))
               3))))))
  (apply (field_mut 1 (global Toploop!)) "deep" deep/1))

val deep : (unit * int option) ref -> int = <fun>
|}, Rectypes{|
Lines 2-6, characters 2-13:
2 | ..match Some r with
3 |   | Some { contents = ((), None) } -> 0
4 |   | _ when (r := ((), None); false) -> 1
5 |   | Some { contents = ((), Some n) } -> n
6 |   | None -> 3
Warning 74 [degraded-to-partial-match]: This pattern-matching is compiled as
  partial, even if it appears to be total. It may generate a "Match_failure"
  exception. This typically occurs due to complex matches on mutable fields.
  (see manual section 13.5.5)
(let
  (deep/2 =
     (function r/5 : int
       (let (*match*/44 = (makeblock 0 r/5))
         (catch
           (if *match*/44
             (let (*match*/45 =o (field_mut 0 (field_imm 0 *match*/44)))
               (if (field_imm 1 *match*/45) (exit 65) 0))
             (exit 65))
          with (65)
           (if (seq (setfield_ptr 0 r/5 [0: 0 0]) 0) 1
             (if *match*/44
               (let
                 (*match*/46 =o (field_mut 0 (field_imm 0 *match*/44))
                  *match*/47 =a (field_imm 1 *match*/46))
                 (if *match*/47 (field_imm 0 *match*/47)
                   (raise
                     (makeblock 0 (global Match_failure/0!) [0: "" 2 2]))))
               3))))))
  (apply (field_mut 1 (global Toploop!)) "deep" deep/2))

val deep : (unit * int option) ref -> int = <fun>
|}]


(* In this example:
   - the pattern-matching is total, with subtle GADT usage
     (only the type-checker can tell that it is Total)
   - there are no mutable fields

   Performance expectation: there should not be a Match_failure clause.

   This example is a reduction of a regression caused by #13076 on the
   'CamlinternalFormat.trans' function in the standard library.
*)
type _ t = Bool : bool t | Int : int t | Char : char t;;
let test : type a . a t * a t -> unit = function
  | Int, Int -> ()
  | Bool, Bool -> ()
  | _, Char -> ()
;;
(* PASS: no Match_failure clause generated. *)
[%%expect {|
0
type _ t = Bool : bool t | Int : int t | Char : char t
(let
  (test/9 =
     (function param/6 : int
       (catch
         (if (>= (field_imm 0 param/6) 2) (exit 68)
           (if (>= (field_imm 1 param/6) 2) (exit 68) 0))
        with (68) 0)))
  (apply (field_mut 1 (global Toploop!)) "test" test/9))
val test : 'a t * 'a t -> unit = <fun>
|}, Principal{|
0
type _ t = Bool : bool t | Int : int t | Char : char t
(let
  (test/10 =
     (function param/7 : int
       (catch
         (if (>= (field_imm 0 param/7) 2) (exit 71)
           (if (>= (field_imm 1 param/7) 2) (exit 71) 0))
        with (71) 0)))
  (apply (field_mut 1 (global Toploop!)) "test" test/10))
val test : 'a t * 'a t -> unit = <fun>
|}, Rectypes{|
0
type _ t = Bool : bool t | Int : int t | Char : char t
(let
  (test/11 =
     (function param/8 : int
       (catch
         (if (>= (field_imm 0 param/8) 2) (exit 74)
           (if (>= (field_imm 1 param/8) 2) (exit 74) 0))
        with (74) 0)))
  (apply (field_mut 1 (global Toploop!)) "test" test/11))
val test : 'a t * 'a t -> unit = <fun>
|}];;

(* Another regression testcase from #13076, proposed by Nick Roberts.

   Performance expectation: no Match_failure clause.
*)
type nothing = |
type t = A | B | C of nothing
let f : bool * t -> int = function
  | true, A -> 3
  | false, A -> 4
  | _, B -> 5
  | _, C _ -> .
(* PASS: no Match_failure clause generated. *)
[%%expect {|
0
type nothing = |
0
type t = A | B | C of nothing
(let
  (f/9 =
     (function param/9 : int
       (catch
         (if (field_imm 0 param/9)
           (switch* (field_imm 1 param/9)
            case int 0: 3
            case int 1: (exit 77))
           (switch* (field_imm 1 param/9)
            case int 0: 4
            case int 1: (exit 77)))
        with (77) 5)))
  (apply (field_mut 1 (global Toploop!)) "f" f/9))
val f : bool * t -> int = <fun>
|}, Principal{|
0
type nothing = |
0
type t = A | B | C of nothing
(let
  (f/10 =
     (function param/10 : int
       (catch
         (if (field_imm 0 param/10)
           (switch* (field_imm 1 param/10)
            case int 0: 3
            case int 1: (exit 79))
           (switch* (field_imm 1 param/10)
            case int 0: 4
            case int 1: (exit 79)))
        with (79) 5)))
  (apply (field_mut 1 (global Toploop!)) "f" f/10))
val f : bool * t -> int = <fun>
|}, Rectypes{|
0
type nothing = |
0
type t = A | B | C of nothing
(let
  (f/11 =
     (function param/11 : int
       (catch
         (if (field_imm 0 param/11)
           (switch* (field_imm 1 param/11)
            case int 0: 3
            case int 1: (exit 81))
           (switch* (field_imm 1 param/11)
            case int 0: 4
            case int 1: (exit 81)))
        with (81) 5)))
  (apply (field_mut 1 (global Toploop!)) "f" f/11))
val f : bool * t -> int = <fun>
|}];;


(* Another regression testcase from #13076, proposed by Nick Roberts.

   Performance expectation: no Match_failure clause.
*)
type t =
  | A of int
  | B of string
  | C of string
  | D of string

(* use primitives directly rather than Int.compare, String.compare to
   avoid offset number churns in the -dlambda test output when new
   functions get added to the stdlib. *)
external compare_int : int -> int -> int = "%compare"
external compare_string : string -> string -> int = "%compare"

let compare t1 t2 =
  match t1, t2 with
  | A i, A j -> compare_int i j
  | B l1, B l2 -> compare_string l1 l2
  | C l1, C l2 -> compare_string l1 l2
  | D l1, D l2 -> compare_string l1 l2
  | A _, (B _ | C _ | D _ ) -> -1
  | (B _ | C _ | D _ ), A _ -> 1
  | B _, (C _ | D _) -> -1
  | (C _ | D _), B _ -> 1
  | C _, D _ -> -1
  | D _, C _ -> 1
(* PASS: no Match_failure clause generated. *)
[%%expect {|
0
type t = A of int | B of string | C of string | D of string
0
external compare_int : int -> int -> int = "%compare"
0
external compare_string : string -> string -> int = "%compare"
(let
  (compare/0 =
     (function t1/0 t2/0 : int
       (catch
         (switch* t1/0
          case tag 0:
           (switch t2/0
            case tag 0: (compare_ints (field_imm 0 t1/0) (field_imm 0 t2/0))
            default: -1)
          case tag 1:
           (catch
             (switch* t2/0
              case tag 0: (exit 85)
              case tag 1:
               (caml_string_compare (field_imm 0 t1/0) (field_imm 0 t2/0))
              case tag 2: (exit 90)
              case tag 3: (exit 90))
            with (90) -1)
          case tag 2:
           (switch* t2/0
            case tag 0: (exit 85)
            case tag 1: (exit 85)
            case tag 2:
             (caml_string_compare (field_imm 0 t1/0) (field_imm 0 t2/0))
            case tag 3: -1)
          case tag 3:
           (switch* t2/0
            case tag 0: (exit 85)
            case tag 1: (exit 85)
            case tag 2: 1
            case tag 3:
             (caml_string_compare (field_imm 0 t1/0) (field_imm 0 t2/0))))
        with (85) (switch* t2/0 case tag 0: 1
                                case tag 1: 1))))
  (apply (field_mut 1 (global Toploop!)) "compare" compare/0))
val compare : t -> t -> int = <fun>
|}, Principal{|
0
type t = A of int | B of string | C of string | D of string
0
external compare_int : int -> int -> int = "%compare"
0
external compare_string : string -> string -> int = "%compare"
(let
  (compare/1 =
     (function t1/1 t2/1 : int
       (catch
         (switch* t1/1
          case tag 0:
           (switch t2/1
            case tag 0: (compare_ints (field_imm 0 t1/1) (field_imm 0 t2/1))
            default: -1)
          case tag 1:
           (catch
             (switch* t2/1
              case tag 0: (exit 98)
              case tag 1:
               (caml_string_compare (field_imm 0 t1/1) (field_imm 0 t2/1))
              case tag 2: (exit 103)
              case tag 3: (exit 103))
            with (103) -1)
          case tag 2:
           (switch* t2/1
            case tag 0: (exit 98)
            case tag 1: (exit 98)
            case tag 2:
             (caml_string_compare (field_imm 0 t1/1) (field_imm 0 t2/1))
            case tag 3: -1)
          case tag 3:
           (switch* t2/1
            case tag 0: (exit 98)
            case tag 1: (exit 98)
            case tag 2: 1
            case tag 3:
             (caml_string_compare (field_imm 0 t1/1) (field_imm 0 t2/1))))
        with (98) (switch* t2/1 case tag 0: 1
                                case tag 1: 1))))
  (apply (field_mut 1 (global Toploop!)) "compare" compare/1))
val compare : t -> t -> int = <fun>
|}, Rectypes{|
0
type t = A of int | B of string | C of string | D of string
0
external compare_int : int -> int -> int = "%compare"
0
external compare_string : string -> string -> int = "%compare"
(let
  (compare/2 =
     (function t1/2 t2/2 : int
       (catch
         (switch* t1/2
          case tag 0:
           (switch t2/2
            case tag 0: (compare_ints (field_imm 0 t1/2) (field_imm 0 t2/2))
            default: -1)
          case tag 1:
           (catch
             (switch* t2/2
              case tag 0: (exit 111)
              case tag 1:
               (caml_string_compare (field_imm 0 t1/2) (field_imm 0 t2/2))
              case tag 2: (exit 116)
              case tag 3: (exit 116))
            with (116) -1)
          case tag 2:
           (switch* t2/2
            case tag 0: (exit 111)
            case tag 1: (exit 111)
            case tag 2:
             (caml_string_compare (field_imm 0 t1/2) (field_imm 0 t2/2))
            case tag 3: -1)
          case tag 3:
           (switch* t2/2
            case tag 0: (exit 111)
            case tag 1: (exit 111)
            case tag 2: 1
            case tag 3:
             (caml_string_compare (field_imm 0 t1/2) (field_imm 0 t2/2))))
        with (111) (switch* t2/2 case tag 0: 1
                                 case tag 1: 1))))
  (apply (field_mut 1 (global Toploop!)) "compare" compare/2))
val compare : t -> t -> int = <fun>
|}];;


(* Different testcases involving or-patterns and polymorphic variants,
   proposed by Nick Roberts. In both cases, we do *not* expect a Match_failure case. *)

let f x y =
 match x, y with
 | _, `Y1 -> 0
 | `X1, `Y2 -> 1
 | (`X2 | `X3), `Y3 -> 2
 | `X1, `Y3
 | `X2, `Y2
 | `X3, _  -> 3
(* PASS: no Match_failure generated *)
[%%expect {|
(let
  (f/12 =
     (function x/9[int] y/0[int] : int
       (catch
         (catch
           (catch
             (if (isint y/0) (if (!= y/0 19896) (exit 125) 0) (exit 125))
            with (125)
             (if (!= x/9 19674)
               (if (>= x/9 19675) (exit 124)
                 (if (>= y/0 19898) (exit 122) 1))
               (if (isint y/0) (if (!= y/0 19897) (exit 124) (exit 122))
                 (exit 124))))
          with (124)
           (if (isint y/0) (if (!= y/0 19898) (exit 122) 2) (exit 122)))
        with (122) 3)))
  (apply (field_mut 1 (global Toploop!)) "f" f/12))
val f : [< `X1 | `X2 | `X3 ] -> [< `Y1 | `Y2 | `Y3 ] -> int = <fun>
|}, Principal{|
(let
  (f/13 =
     (function x/10[int] y/1[int] : int
       (catch
         (catch
           (catch
             (if (isint y/1) (if (!= y/1 19896) (exit 133) 0) (exit 133))
            with (133)
             (if (!= x/10 19674)
               (if (>= x/10 19675) (exit 132)
                 (if (>= y/1 19898) (exit 130) 1))
               (if (isint y/1) (if (!= y/1 19897) (exit 132) (exit 130))
                 (exit 132))))
          with (132)
           (if (isint y/1) (if (!= y/1 19898) (exit 130) 2) (exit 130)))
        with (130) 3)))
  (apply (field_mut 1 (global Toploop!)) "f" f/13))
val f : [< `X1 | `X2 | `X3 ] -> [< `Y1 | `Y2 | `Y3 ] -> int = <fun>
|}, Rectypes{|
(let
  (f/14 =
     (function x/11[int] y/2[int] : int
       (catch
         (catch
           (catch
             (if (isint y/2) (if (!= y/2 19896) (exit 141) 0) (exit 141))
            with (141)
             (if (!= x/11 19674)
               (if (>= x/11 19675) (exit 140)
                 (if (>= y/2 19898) (exit 138) 1))
               (if (isint y/2) (if (!= y/2 19897) (exit 140) (exit 138))
                 (exit 140))))
          with (140)
           (if (isint y/2) (if (!= y/2 19898) (exit 138) 2) (exit 138)))
        with (138) 3)))
  (apply (field_mut 1 (global Toploop!)) "f" f/14))
val f : [< `X1 | `X2 | `X3 ] -> [< `Y1 | `Y2 | `Y3 ] -> int = <fun>
|}];;


let check_results r1 r2 =
  match r1 r2 with
  | (Ok _ as r), _ | _, (Ok _ as r) -> r
  | (Error `A as r), Error _
  | Error _, (Error `A as r) -> r
  | (Error `B as r), Error `B -> r
(* PASS: no Match_failure case generated *)
[%%expect {|
(let
  (check_results/0 =
     (function r1/0 r2/0
       (let (*match*/48 = (apply r1/0 r2/0))
         (catch
           (catch
             (let (r/6 =a (field_imm 0 *match*/48))
               (catch
                 (switch* r/6
                  case tag 0: (exit 146 r/6)
                  case tag 1:
                   (catch
                     (if (>= (field_imm 0 r/6) 66)
                       (let (*match*/49 =a (field_imm 1 *match*/48))
                         (switch* *match*/49
                          case tag 0: (exit 148)
                          case tag 1:
                           (let (*match*/50 =a (field_imm 0 *match*/49))
                             (if (isint *match*/50)
                               (if (!= *match*/50 66) (exit 149) r/6)
                               (exit 149)))))
                       (switch* (field_imm 1 *match*/48)
                        case tag 0: (exit 148)
                        case tag 1: (exit 147 r/6)))
                    with (149) (exit 147 (field_imm 1 *match*/48))))
                with (148) (exit 146 (field_imm 1 *match*/48))))
            with (146 r/7) r/7)
          with (147 r/8) r/8))))
  (apply (field_mut 1 (global Toploop!)) "check_results" check_results/0))
val check_results :
  ('a -> ('b, [< `A | `B ]) result * ('b, [< `A | `B ]) result) ->
  'a -> ('b, [> `A | `B ]) result = <fun>
|}, Principal{|
(let
  (check_results/1 =
     (function r1/1 r2/1
       (let (*match*/51 = (apply r1/1 r2/1))
         (catch
           (catch
             (let (r/9 =a (field_imm 0 *match*/51))
               (catch
                 (switch* r/9
                  case tag 0: (exit 153 r/9)
                  case tag 1:
                   (catch
                     (if (>= (field_imm 0 r/9) 66)
                       (let (*match*/52 =a (field_imm 1 *match*/51))
                         (switch* *match*/52
                          case tag 0: (exit 155)
                          case tag 1:
                           (let (*match*/53 =a (field_imm 0 *match*/52))
                             (if (isint *match*/53)
                               (if (!= *match*/53 66) (exit 156) r/9)
                               (exit 156)))))
                       (switch* (field_imm 1 *match*/51)
                        case tag 0: (exit 155)
                        case tag 1: (exit 154 r/9)))
                    with (156) (exit 154 (field_imm 1 *match*/51))))
                with (155) (exit 153 (field_imm 1 *match*/51))))
            with (153 r/10) r/10)
          with (154 r/11) r/11))))
  (apply (field_mut 1 (global Toploop!)) "check_results" check_results/1))
val check_results :
  ('a -> ('b, [< `A | `B ]) result * ('b, [< `A | `B ]) result) ->
  'a -> ('b, [> `A | `B ]) result = <fun>
|}, Rectypes{|
(let
  (check_results/2 =
     (function r1/2 r2/2
       (let (*match*/54 = (apply r1/2 r2/2))
         (catch
           (catch
             (let (r/12 =a (field_imm 0 *match*/54))
               (catch
                 (switch* r/12
                  case tag 0: (exit 160 r/12)
                  case tag 1:
                   (catch
                     (if (>= (field_imm 0 r/12) 66)
                       (let (*match*/55 =a (field_imm 1 *match*/54))
                         (switch* *match*/55
                          case tag 0: (exit 162)
                          case tag 1:
                           (let (*match*/56 =a (field_imm 0 *match*/55))
                             (if (isint *match*/56)
                               (if (!= *match*/56 66) (exit 163) r/12)
                               (exit 163)))))
                       (switch* (field_imm 1 *match*/54)
                        case tag 0: (exit 162)
                        case tag 1: (exit 161 r/12)))
                    with (163) (exit 161 (field_imm 1 *match*/54))))
                with (162) (exit 160 (field_imm 1 *match*/54))))
            with (160 r/13) r/13)
          with (161 r/14) r/14))))
  (apply (field_mut 1 (global Toploop!)) "check_results" check_results/2))
val check_results :
  ('a -> ('b, [< `A | `B ]) result * ('b, [< `A | `B ]) result) ->
  'a -> ('b, [> `A | `B ]) result = <fun>
|}];;
