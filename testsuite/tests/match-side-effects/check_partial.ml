(* TEST
 flags = "-dlambda -dcanonical-ids";
 expect;
*)

(* This test exercises pattern-matching examples that mix mutable
   state with code execution (through guards or lazy patterns). Some
   of those tests appear to be exhaustive to the type-checker but are
   in fact not exhaustive, forcing the pattern-matching compiler to
   add Match_failure clauses for soundness. The pattern-matching
   compiler also sometimes conservatively add Match_failure clauses in
   cases that were in fact exhaustive.
*)

type _ t =
  | Int : int -> int t
  | True : bool t
  | False : bool t

let lazy_total : _ * bool t -> int = function
  | ({ contents = _ }, True) -> 0
  | ({ contents = lazy () }, False) -> 12
(* This pattern-matching is total: a Match_failure case is not
   necessary for soundness. *)
[%%expect {|
0
type _ t = Int : int -> int t | True : bool t | False : bool t
(let
  (lazy_total/0 =
     (function param/0 : int
       (let (*match*/0 =o (field_mut 0 (field_imm 0 param/0)))
         (switch* (field_imm 1 param/0)
          case int 0: 0
          case int 1:
           (let
             (*match*/1 =
                (let (tag/0 =a (caml_obj_tag *match*/0))
                  (if (== tag/0 250) (field_mut 0 *match*/0)
                    (if (|| (== tag/0 246) (== tag/0 244))
                      (apply (field_imm 1 (global CamlinternalLazy!))
                        (opaque *match*/0))
                      *match*/0))))
             12)))))
  (apply (field_mut 1 (global Toploop!)) "lazy_total" lazy_total/0))
val lazy_total : unit lazy_t ref * bool t -> int = <fun>
|}, Principal{|
0
type _ t = Int : int -> int t | True : bool t | False : bool t
(let
  (lazy_total/1 =
     (function param/1 : int
       (let (*match*/2 =o (field_mut 0 (field_imm 0 param/1)))
         (switch* (field_imm 1 param/1)
          case int 0: 0
          case int 1:
           (let
             (*match*/3 =
                (let (tag/1 =a (caml_obj_tag *match*/2))
                  (if (== tag/1 250) (field_mut 0 *match*/2)
                    (if (|| (== tag/1 246) (== tag/1 244))
                      (apply (field_imm 1 (global CamlinternalLazy!))
                        (opaque *match*/2))
                      *match*/2))))
             12)))))
  (apply (field_mut 1 (global Toploop!)) "lazy_total" lazy_total/1))
val lazy_total : unit lazy_t ref * bool t -> int = <fun>
|}, Rectypes{|
0
type _ t = Int : int -> int t | True : bool t | False : bool t
(let
  (lazy_total/2 =
     (function param/2 : int
       (let (*match*/4 =o (field_mut 0 (field_imm 0 param/2)))
         (switch* (field_imm 1 param/2)
          case int 0: 0
          case int 1:
           (let
             (*match*/5 =
                (let (tag/2 =a (caml_obj_tag *match*/4))
                  (if (== tag/2 250) (field_mut 0 *match*/4)
                    (if (|| (== tag/2 246) (== tag/2 244))
                      (apply (field_imm 1 (global CamlinternalLazy!))
                        (opaque *match*/4))
                      *match*/4))))
             12)))))
  (apply (field_mut 1 (global Toploop!)) "lazy_total" lazy_total/2))
val lazy_total : unit lazy_t ref * bool t -> int = <fun>
|}];;

let lazy_needs_partial : _ * bool t ref -> int = function
  | (_, { contents = True }) -> 0
  | (lazy (), { contents = False }) -> 12
(* This pattern-matching is partial: a Match_failure case is
   necessary for soundness. *)
[%%expect {|
(let
  (lazy_needs_partial/0 =
     (function param/3 : int
       (catch
         (let
           (*match*/6 =a (field_imm 0 param/3)
            *match*/7 =o (field_mut 0 (field_imm 1 param/3)))
           (switch* *match*/7
            case int 0: 0
            case int 1:
             (let
               (*match*/8 =
                  (let (tag/3 =a (caml_obj_tag *match*/6))
                    (if (== tag/3 250) (field_mut 0 *match*/6)
                      (if (|| (== tag/3 246) (== tag/3 244))
                        (apply (field_imm 1 (global CamlinternalLazy!))
                          (opaque *match*/6))
                        *match*/6)))
                *match*/9 =o (field_mut 0 (field_imm 1 param/3)))
               (if (isint *match*/9) (if *match*/9 12 (exit 7)) (exit 7)))))
        with (7)
         (raise (makeblock 0 (global Match_failure/0!) [0: "" 1 49])))))
  (apply (field_mut 1 (global Toploop!)) "lazy_needs_partial"
    lazy_needs_partial/0))
val lazy_needs_partial : unit lazy_t * bool t ref -> int = <fun>
|}, Principal{|
(let
  (lazy_needs_partial/1 =
     (function param/4 : int
       (catch
         (let
           (*match*/10 =a (field_imm 0 param/4)
            *match*/11 =o (field_mut 0 (field_imm 1 param/4)))
           (switch* *match*/11
            case int 0: 0
            case int 1:
             (let
               (*match*/12 =
                  (let (tag/4 =a (caml_obj_tag *match*/10))
                    (if (== tag/4 250) (field_mut 0 *match*/10)
                      (if (|| (== tag/4 246) (== tag/4 244))
                        (apply (field_imm 1 (global CamlinternalLazy!))
                          (opaque *match*/10))
                        *match*/10)))
                *match*/13 =o (field_mut 0 (field_imm 1 param/4)))
               (if (isint *match*/13) (if *match*/13 12 (exit 9)) (exit 9)))))
        with (9)
         (raise (makeblock 0 (global Match_failure/0!) [0: "" 1 49])))))
  (apply (field_mut 1 (global Toploop!)) "lazy_needs_partial"
    lazy_needs_partial/1))
val lazy_needs_partial : unit lazy_t * bool t ref -> int = <fun>
|}, Rectypes{|
(let
  (lazy_needs_partial/2 =
     (function param/5 : int
       (catch
         (let
           (*match*/14 =a (field_imm 0 param/5)
            *match*/15 =o (field_mut 0 (field_imm 1 param/5)))
           (switch* *match*/15
            case int 0: 0
            case int 1:
             (let
               (*match*/16 =
                  (let (tag/5 =a (caml_obj_tag *match*/14))
                    (if (== tag/5 250) (field_mut 0 *match*/14)
                      (if (|| (== tag/5 246) (== tag/5 244))
                        (apply (field_imm 1 (global CamlinternalLazy!))
                          (opaque *match*/14))
                        *match*/14)))
                *match*/17 =o (field_mut 0 (field_imm 1 param/5)))
               (if (isint *match*/17) (if *match*/17 12 (exit 11)) (exit 11)))))
        with (11)
         (raise (makeblock 0 (global Match_failure/0!) [0: "" 1 49])))))
  (apply (field_mut 1 (global Toploop!)) "lazy_needs_partial"
    lazy_needs_partial/2))
val lazy_needs_partial : unit lazy_t * bool t ref -> int = <fun>
|}];;

let guard_total : bool t ref -> int = function
  | _ when Sys.opaque_identity false -> 1
  | { contents = True } -> 0
  | { contents = False } -> 12
(* This pattern-matching is total: a Match_failure case is not
   necessary for soundness. *)
[%%expect {|
(let
  (guard_total/0 =
     (function param/6 : int
       (if (opaque 0) 1
         (let (*match*/18 =o (field_mut 0 param/6))
           (if (isint *match*/18) (if *match*/18 12 0)
             (raise (makeblock 0 (global Match_failure/0!) [0: "" 1 38])))))))
  (apply (field_mut 1 (global Toploop!)) "guard_total" guard_total/0))
val guard_total : bool t ref -> int = <fun>
|}, Principal{|
(let
  (guard_total/1 =
     (function param/7 : int
       (if (opaque 0) 1
         (let (*match*/19 =o (field_mut 0 param/7))
           (if (isint *match*/19) (if *match*/19 12 0)
             (raise (makeblock 0 (global Match_failure/0!) [0: "" 1 38])))))))
  (apply (field_mut 1 (global Toploop!)) "guard_total" guard_total/1))
val guard_total : bool t ref -> int = <fun>
|}, Rectypes{|
(let
  (guard_total/2 =
     (function param/8 : int
       (if (opaque 0) 1
         (let (*match*/20 =o (field_mut 0 param/8))
           (if (isint *match*/20) (if *match*/20 12 0)
             (raise (makeblock 0 (global Match_failure/0!) [0: "" 1 38])))))))
  (apply (field_mut 1 (global Toploop!)) "guard_total" guard_total/2))
val guard_total : bool t ref -> int = <fun>
|}];;

let guard_needs_partial : bool t ref -> int = function
  | { contents = True } -> 0
  | _ when Sys.opaque_identity false -> 1
  | { contents = False } -> 12
(* This pattern-matching is partial: a Match_failure case is
   necessary for soundness. *)
[%%expect {|
(let
  (guard_needs_partial/0 =
     (function param/9 : int
       (let (*match*/21 =o (field_mut 0 param/9))
         (catch (if (isint *match*/21) (if *match*/21 (exit 21) 0) (exit 21))
          with (21)
           (if (opaque 0) 1
             (if (isint *match*/21) 12
               (raise (makeblock 0 (global Match_failure/0!) [0: "" 1 46]))))))))
  (apply (field_mut 1 (global Toploop!)) "guard_needs_partial"
    guard_needs_partial/0))
val guard_needs_partial : bool t ref -> int = <fun>
|}, Principal{|
(let
  (guard_needs_partial/1 =
     (function param/10 : int
       (let (*match*/22 =o (field_mut 0 param/10))
         (catch (if (isint *match*/22) (if *match*/22 (exit 24) 0) (exit 24))
          with (24)
           (if (opaque 0) 1
             (if (isint *match*/22) 12
               (raise (makeblock 0 (global Match_failure/0!) [0: "" 1 46]))))))))
  (apply (field_mut 1 (global Toploop!)) "guard_needs_partial"
    guard_needs_partial/1))
val guard_needs_partial : bool t ref -> int = <fun>
|}, Rectypes{|
(let
  (guard_needs_partial/2 =
     (function param/11 : int
       (let (*match*/23 =o (field_mut 0 param/11))
         (catch (if (isint *match*/23) (if *match*/23 (exit 27) 0) (exit 27))
          with (27)
           (if (opaque 0) 1
             (if (isint *match*/23) 12
               (raise (makeblock 0 (global Match_failure/0!) [0: "" 1 46]))))))))
  (apply (field_mut 1 (global Toploop!)) "guard_needs_partial"
    guard_needs_partial/2))
val guard_needs_partial : bool t ref -> int = <fun>
|}];;
