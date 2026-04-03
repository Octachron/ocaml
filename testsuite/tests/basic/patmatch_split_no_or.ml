(* TEST
 flags = "-nostdlib -nopervasives -dlambda -dcanonical-ids";
 expect;
*)

(******************************************************************************)

(* Check that the extra split indeed happens when the last row is made of
   "variables" only *)

let last_is_anys = function
  | true, false -> 1
  | _, false -> 2
  | _, _ -> 3
;;
[%%expect{|
(let
  (last_is_anys/0 =
     (function param/0 : int
       (catch
         (if (field_imm 0 param/0) (if (field_imm 1 param/0) (exit 2) 1)
           (if (field_imm 1 param/0) (exit 2) 2))
        with (2) 3)))
  (apply (field_mut 1 (global Toploop!)) "last_is_anys" last_is_anys/0))
val last_is_anys : bool * bool -> int = <fun>
|}, Principal{|
(let
  (last_is_anys/1 =
     (function param/1 : int
       (catch
         (if (field_imm 0 param/1) (if (field_imm 1 param/1) (exit 5) 1)
           (if (field_imm 1 param/1) (exit 5) 2))
        with (5) 3)))
  (apply (field_mut 1 (global Toploop!)) "last_is_anys" last_is_anys/1))
val last_is_anys : bool * bool -> int = <fun>
|}, Rectypes{|
(let
  (last_is_anys/2 =
     (function param/2 : int
       (catch
         (if (field_imm 0 param/2) (if (field_imm 1 param/2) (exit 8) 1)
           (if (field_imm 1 param/2) (exit 8) 2))
        with (8) 3)))
  (apply (field_mut 1 (global Toploop!)) "last_is_anys" last_is_anys/2))
val last_is_anys : bool * bool -> int = <fun>
|}]

let last_is_vars = function
  | true, false -> 1
  | _, false -> 2
  | _x, _y -> 3
;;
[%%expect{|
(let
  (last_is_vars/0 =
     (function param/3 : int
       (catch
         (if (field_imm 0 param/3) (if (field_imm 1 param/3) (exit 11) 1)
           (if (field_imm 1 param/3) (exit 11) 2))
        with (11) 3)))
  (apply (field_mut 1 (global Toploop!)) "last_is_vars" last_is_vars/0))
val last_is_vars : bool * bool -> int = <fun>
|}, Principal{|
(let
  (last_is_vars/1 =
     (function param/4 : int
       (catch
         (if (field_imm 0 param/4) (if (field_imm 1 param/4) (exit 14) 1)
           (if (field_imm 1 param/4) (exit 14) 2))
        with (14) 3)))
  (apply (field_mut 1 (global Toploop!)) "last_is_vars" last_is_vars/1))
val last_is_vars : bool * bool -> int = <fun>
|}, Rectypes{|
(let
  (last_is_vars/2 =
     (function param/5 : int
       (catch
         (if (field_imm 0 param/5) (if (field_imm 1 param/5) (exit 17) 1)
           (if (field_imm 1 param/5) (exit 17) 2))
        with (17) 3)))
  (apply (field_mut 1 (global Toploop!)) "last_is_vars" last_is_vars/2))
val last_is_vars : bool * bool -> int = <fun>
|}]

(******************************************************************************)

(* Check that the [| _, false, true -> 12] gets raised. *)

type t = ..
type t += A | B of unit | C of bool * int;;
[%%expect{|
0
type t = ..
(let
  (A/0 = (makeblock 248 "A" (caml_fresh_oo_id 0))
   B/0 = (makeblock 248 "B" (caml_fresh_oo_id 0))
   C/0 = (makeblock 248 "C" (caml_fresh_oo_id 0)))
  (seq (apply (field_mut 1 (global Toploop!)) "A/54" A/0)
    (apply (field_mut 1 (global Toploop!)) "B/55" B/0)
    (apply (field_mut 1 (global Toploop!)) "C/56" C/0)))
type t += A | B of unit | C of bool * int
|}, Principal{|
0
type t = ..
(let
  (A/1 = (makeblock 248 "A" (caml_fresh_oo_id 0))
   B/1 = (makeblock 248 "B" (caml_fresh_oo_id 0))
   C/1 = (makeblock 248 "C" (caml_fresh_oo_id 0)))
  (seq (apply (field_mut 1 (global Toploop!)) "A/58" A/1)
    (apply (field_mut 1 (global Toploop!)) "B/59" B/1)
    (apply (field_mut 1 (global Toploop!)) "C/60" C/1)))
type t += A | B of unit | C of bool * int
|}, Rectypes{|
0
type t = ..
(let
  (A/2 = (makeblock 248 "A" (caml_fresh_oo_id 0))
   B/2 = (makeblock 248 "B" (caml_fresh_oo_id 0))
   C/2 = (makeblock 248 "C" (caml_fresh_oo_id 0)))
  (seq (apply (field_mut 1 (global Toploop!)) "A/62" A/2)
    (apply (field_mut 1 (global Toploop!)) "B/63" B/2)
    (apply (field_mut 1 (global Toploop!)) "C/64" C/2)))
type t += A | B of unit | C of bool * int
|}]

let f = function
  | A, true, _ -> 1
  | _, false, false -> 11
  | B _, true, _ -> 2
  | C _, true, _ -> 3
  | _, false, true -> 12
  | _ -> 4
;;
[%%expect{|
(let
  (C/0 = (apply (field_mut 0 (global Toploop!)) "C/56")
   B/0 = (apply (field_mut 0 (global Toploop!)) "B/55")
   A/0 = (apply (field_mut 0 (global Toploop!)) "A/54")
   f/0 =
     (function param/6 : int
       (let (*match*/0 =a (field_imm 0 param/6))
         (catch
           (if (== *match*/0 A/0) (if (field_imm 1 param/6) 1 (exit 23))
             (exit 23))
          with (23)
           (if (field_imm 1 param/6)
             (if (== (field_imm 0 *match*/0) B/0) 2
               (if (== (field_imm 0 *match*/0) C/0) 3 4))
             (if (field_imm 2 param/6) 12 11))))))
  (apply (field_mut 1 (global Toploop!)) "f" f/0))
val f : t * bool * bool -> int = <fun>
|}, Principal{|
(let
  (C/1 = (apply (field_mut 0 (global Toploop!)) "C/60")
   B/1 = (apply (field_mut 0 (global Toploop!)) "B/59")
   A/1 = (apply (field_mut 0 (global Toploop!)) "A/58")
   f/1 =
     (function param/7 : int
       (let (*match*/1 =a (field_imm 0 param/7))
         (catch
           (if (== *match*/1 A/1) (if (field_imm 1 param/7) 1 (exit 28))
             (exit 28))
          with (28)
           (if (field_imm 1 param/7)
             (if (== (field_imm 0 *match*/1) B/1) 2
               (if (== (field_imm 0 *match*/1) C/1) 3 4))
             (if (field_imm 2 param/7) 12 11))))))
  (apply (field_mut 1 (global Toploop!)) "f" f/1))
val f : t * bool * bool -> int = <fun>
|}, Rectypes{|
(let
  (C/2 = (apply (field_mut 0 (global Toploop!)) "C/64")
   B/2 = (apply (field_mut 0 (global Toploop!)) "B/63")
   A/2 = (apply (field_mut 0 (global Toploop!)) "A/62")
   f/2 =
     (function param/8 : int
       (let (*match*/2 =a (field_imm 0 param/8))
         (catch
           (if (== *match*/2 A/2) (if (field_imm 1 param/8) 1 (exit 33))
             (exit 33))
          with (33)
           (if (field_imm 1 param/8)
             (if (== (field_imm 0 *match*/2) B/2) 2
               (if (== (field_imm 0 *match*/2) C/2) 3 4))
             (if (field_imm 2 param/8) 12 11))))))
  (apply (field_mut 1 (global Toploop!)) "f" f/2))
val f : t * bool * bool -> int = <fun>
|}]
