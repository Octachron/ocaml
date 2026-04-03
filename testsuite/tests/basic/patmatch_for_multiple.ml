(* TEST
 flags = "-drawlambda -dlambda -dcanonical-ids";
 expect;
*)

(* Note: the tests below contain *both* the -drawlambda and
   the -dlambda intermediate representations:
   -drawlambda is the Lambda code generated directly by the
     pattern-matching compiler; it contain "alias" bindings or static
     exits that are unused, and will be removed by simplification, or
     that are used only once, and will be inlined by simplification.
   -dlambda is the Lambda code resulting from simplification.

  The -drawlambda output more closely matches what the
  pattern-compiler produces, and the -dlambda output more closely
  matches the final generated code.

  In this test we decided to show both to notice that some allocations
  are "optimized away" during simplification (see "here flattening is
  an optimization" below).
*)

match (3, 2, 1) with
| (_, 3, _)
| (1, _, _) -> true
| _ -> false
;;
[%%expect{|
(let (*match*/0 = 3 *match*/1 = 2 *match*/2 = 1)
  (catch
    (catch
      (catch (if (!= *match*/1 3) (exit 4) (exit 2)) with (4)
        (if (!= *match*/0 1) (exit 3) (exit 2)))
     with (3) 0)
   with (2) 1))
(let (*match*/0 = 3 *match*/1 = 2 *match*/2 = 1)
  (catch (if (!= *match*/1 3) (if (!= *match*/0 1) 0 (exit 2)) (exit 2))
   with (2) 1))
- : bool = false
|}, Principal{|
(let (*match*/3 = 3 *match*/4 = 2 *match*/5 = 1)
  (catch
    (catch
      (catch (if (!= *match*/4 3) (exit 8) (exit 6)) with (8)
        (if (!= *match*/3 1) (exit 7) (exit 6)))
     with (7) 0)
   with (6) 1))
(let (*match*/3 = 3 *match*/4 = 2 *match*/5 = 1)
  (catch (if (!= *match*/4 3) (if (!= *match*/3 1) 0 (exit 6)) (exit 6))
   with (6) 1))
- : bool = false
|}, Rectypes{|
(let (*match*/6 = 3 *match*/7 = 2 *match*/8 = 1)
  (catch
    (catch
      (catch (if (!= *match*/7 3) (exit 12) (exit 10)) with (12)
        (if (!= *match*/6 1) (exit 11) (exit 10)))
     with (11) 0)
   with (10) 1))
(let (*match*/6 = 3 *match*/7 = 2 *match*/8 = 1)
  (catch (if (!= *match*/7 3) (if (!= *match*/6 1) 0 (exit 10)) (exit 10))
   with (10) 1))
- : bool = false
|}];;

(* This tests needs to allocate the tuple to bind 'x',
   but this is only done in the branches that use it. *)
match (3, 2, 1) with
| ((_, 3, _) as x)
| ((1, _, _) as x) -> ignore x; true
| _ -> false
;;
[%%expect{|
(let (*match*/9 = 3 *match*/10 = 2 *match*/11 = 1)
  (catch
    (catch
      (catch
        (if (!= *match*/10 3) (exit 16)
          (let (x/0 =a (makeblock 0 *match*/9 *match*/10 *match*/11))
            (exit 14 x/0)))
       with (16)
        (if (!= *match*/9 1) (exit 15)
          (let (x/1 =a (makeblock 0 *match*/9 *match*/10 *match*/11))
            (exit 14 x/1))))
     with (15) 0)
   with (14 x/2) (seq (ignore x/2) 1)))
(let (*match*/9 = 3 *match*/10 = 2 *match*/11 = 1)
  (catch
    (if (!= *match*/10 3)
      (if (!= *match*/9 1) 0
        (exit 14 (makeblock 0 *match*/9 *match*/10 *match*/11)))
      (exit 14 (makeblock 0 *match*/9 *match*/10 *match*/11)))
   with (14 x/2) (seq (ignore x/2) 1)))
- : bool = false
|}, Principal{|
(let (*match*/12 = 3 *match*/13 = 2 *match*/14 = 1)
  (catch
    (catch
      (catch
        (if (!= *match*/13 3) (exit 20)
          (let (x/3 =a (makeblock 0 *match*/12 *match*/13 *match*/14))
            (exit 18 x/3)))
       with (20)
        (if (!= *match*/12 1) (exit 19)
          (let (x/4 =a (makeblock 0 *match*/12 *match*/13 *match*/14))
            (exit 18 x/4))))
     with (19) 0)
   with (18 x/5) (seq (ignore x/5) 1)))
(let (*match*/12 = 3 *match*/13 = 2 *match*/14 = 1)
  (catch
    (if (!= *match*/13 3)
      (if (!= *match*/12 1) 0
        (exit 18 (makeblock 0 *match*/12 *match*/13 *match*/14)))
      (exit 18 (makeblock 0 *match*/12 *match*/13 *match*/14)))
   with (18 x/5) (seq (ignore x/5) 1)))
- : bool = false
|}, Rectypes{|
(let (*match*/15 = 3 *match*/16 = 2 *match*/17 = 1)
  (catch
    (catch
      (catch
        (if (!= *match*/16 3) (exit 24)
          (let (x/6 =a (makeblock 0 *match*/15 *match*/16 *match*/17))
            (exit 22 x/6)))
       with (24)
        (if (!= *match*/15 1) (exit 23)
          (let (x/7 =a (makeblock 0 *match*/15 *match*/16 *match*/17))
            (exit 22 x/7))))
     with (23) 0)
   with (22 x/8) (seq (ignore x/8) 1)))
(let (*match*/15 = 3 *match*/16 = 2 *match*/17 = 1)
  (catch
    (if (!= *match*/16 3)
      (if (!= *match*/15 1) 0
        (exit 22 (makeblock 0 *match*/15 *match*/16 *match*/17)))
      (exit 22 (makeblock 0 *match*/15 *match*/16 *match*/17)))
   with (22 x/8) (seq (ignore x/8) 1)))
- : bool = false
|}];;

(* Regression test for #3780 *)
let _ = fun a b ->
  match a, b with
  | ((true, _) as _g)
  | ((false, _) as _g) -> ()
[%%expect{|
(function a/0[int] b/0 : int 0)
(function a/0[int] b/0 : int 0)
- : bool -> 'a -> unit = <fun>
|}, Principal{|
(function a/1[int] b/1 : int 0)
(function a/1[int] b/1 : int 0)
- : bool -> 'a -> unit = <fun>
|}, Rectypes{|
(function a/2[int] b/2 : int 0)
(function a/2[int] b/2 : int 0)
- : bool -> 'a -> unit = <fun>
|}];;

(* More complete tests.

   The test cases below compare the compiler output on alias patterns
   that are outside an or-pattern (handled during half-simplification,
   then flattened) or inside an or-pattern (handled during simplification).

   We used to have a Cannot_flatten exception that would result in fairly
   different code generated in both cases, but now the compilation strategy
   is fairly similar.
*)
let _ = fun a b -> match a, b with
| (true, _) as p -> p
| (false, _) as p -> p
(* outside, trivial *)
[%%expect {|
(function a/3[int] b/3 (let (p/0 =a (makeblock 0 a/3 b/3)) p/0))
(function a/3[int] b/3 (makeblock 0 a/3 b/3))
- : bool -> 'a -> bool * 'a = <fun>
|}, Principal{|
(function a/4[int] b/4 (let (p/1 =a (makeblock 0 a/4 b/4)) p/1))
(function a/4[int] b/4 (makeblock 0 a/4 b/4))
- : bool -> 'a -> bool * 'a = <fun>
|}, Rectypes{|
(function a/5[int] b/5 (let (p/2 =a (makeblock 0 a/5 b/5)) p/2))
(function a/5[int] b/5 (makeblock 0 a/5 b/5))
- : bool -> 'a -> bool * 'a = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true, _) as p)
| ((false, _) as p) -> p
(* inside, trivial *)
[%%expect{|
(function a/6[int] b/6 (let (p/3 =a (makeblock 0 a/6 b/6)) p/3))
(function a/6[int] b/6 (makeblock 0 a/6 b/6))
- : bool -> 'a -> bool * 'a = <fun>
|}, Principal{|
(function a/7[int] b/7 (let (p/4 =a (makeblock 0 a/7 b/7)) p/4))
(function a/7[int] b/7 (makeblock 0 a/7 b/7))
- : bool -> 'a -> bool * 'a = <fun>
|}, Rectypes{|
(function a/8[int] b/8 (let (p/5 =a (makeblock 0 a/8 b/8)) p/5))
(function a/8[int] b/8 (makeblock 0 a/8 b/8))
- : bool -> 'a -> bool * 'a = <fun>
|}];;

let _ = fun a b -> match a, b with
| (true as x, _) as p -> x, p
| (false as x, _) as p -> x, p
(* outside, simple *)
[%%expect {|
(function a/9[int] b/9
  (let (x/9 =a[int] a/9 p/6 =a (makeblock 0 a/9 b/9))
    (makeblock 0 (int,*) x/9 p/6)))
(function a/9[int] b/9 (makeblock 0 (int,*) a/9 (makeblock 0 a/9 b/9)))
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}, Principal{|
(function a/10[int] b/10
  (let (x/10 =a[int] a/10 p/7 =a (makeblock 0 a/10 b/10))
    (makeblock 0 (int,*) x/10 p/7)))
(function a/10[int] b/10 (makeblock 0 (int,*) a/10 (makeblock 0 a/10 b/10)))
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}, Rectypes{|
(function a/11[int] b/11
  (let (x/11 =a[int] a/11 p/8 =a (makeblock 0 a/11 b/11))
    (makeblock 0 (int,*) x/11 p/8)))
(function a/11[int] b/11 (makeblock 0 (int,*) a/11 (makeblock 0 a/11 b/11)))
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false as x, _) as p) -> x, p
(* inside, simple *)
[%%expect {|
(function a/12[int] b/12
  (let (x/12 =a[int] a/12 p/9 =a (makeblock 0 a/12 b/12))
    (makeblock 0 (int,*) x/12 p/9)))
(function a/12[int] b/12 (makeblock 0 (int,*) a/12 (makeblock 0 a/12 b/12)))
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}, Principal{|
(function a/13[int] b/13
  (let (x/13 =a[int] a/13 p/10 =a (makeblock 0 a/13 b/13))
    (makeblock 0 (int,*) x/13 p/10)))
(function a/13[int] b/13 (makeblock 0 (int,*) a/13 (makeblock 0 a/13 b/13)))
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}, Rectypes{|
(function a/14[int] b/14
  (let (x/14 =a[int] a/14 p/11 =a (makeblock 0 a/14 b/14))
    (makeblock 0 (int,*) x/14 p/11)))
(function a/14[int] b/14 (makeblock 0 (int,*) a/14 (makeblock 0 a/14 b/14)))
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

let _ = fun a b -> match a, b with
| (true as x, _) as p -> x, p
| (false, x) as p -> x, p
(* outside, complex *)
[%%expect{|
(function a/15[int] b/15[int]
  (if a/15
    (let (x/15 =a[int] a/15 p/12 =a (makeblock 0 a/15 b/15))
      (makeblock 0 (int,*) x/15 p/12))
    (let (x/16 =a b/15 p/13 =a (makeblock 0 a/15 b/15))
      (makeblock 0 (int,*) x/16 p/13))))
(function a/15[int] b/15[int]
  (if a/15 (makeblock 0 (int,*) a/15 (makeblock 0 a/15 b/15))
    (makeblock 0 (int,*) b/15 (makeblock 0 a/15 b/15))))
- : bool -> bool -> bool * (bool * bool) = <fun>
|}, Principal{|
(function a/16[int] b/16[int]
  (if a/16
    (let (x/17 =a[int] a/16 p/14 =a (makeblock 0 a/16 b/16))
      (makeblock 0 (int,*) x/17 p/14))
    (let (x/18 =a b/16 p/15 =a (makeblock 0 a/16 b/16))
      (makeblock 0 (int,*) x/18 p/15))))
(function a/16[int] b/16[int]
  (if a/16 (makeblock 0 (int,*) a/16 (makeblock 0 a/16 b/16))
    (makeblock 0 (int,*) b/16 (makeblock 0 a/16 b/16))))
- : bool -> bool -> bool * (bool * bool) = <fun>
|}, Rectypes{|
(function a/17[int] b/17[int]
  (if a/17
    (let (x/19 =a[int] a/17 p/16 =a (makeblock 0 a/17 b/17))
      (makeblock 0 (int,*) x/19 p/16))
    (let (x/20 =a b/17 p/17 =a (makeblock 0 a/17 b/17))
      (makeblock 0 (int,*) x/20 p/17))))
(function a/17[int] b/17[int]
  (if a/17 (makeblock 0 (int,*) a/17 (makeblock 0 a/17 b/17))
    (makeblock 0 (int,*) b/17 (makeblock 0 a/17 b/17))))
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false, x) as p)
  -> x, p
(* inside, complex *)
[%%expect{|
(function a/18[int] b/18[int]
  (catch
    (if a/18
      (let (x/21 =a[int] a/18 p/18 =a (makeblock 0 a/18 b/18))
        (exit 89 x/21 p/18))
      (let (x/22 =a b/18 p/19 =a (makeblock 0 a/18 b/18))
        (exit 89 x/22 p/19)))
   with (89 x/23[int] p/20) (makeblock 0 (int,*) x/23 p/20)))
(function a/18[int] b/18[int]
  (catch
    (if a/18 (exit 89 a/18 (makeblock 0 a/18 b/18))
      (exit 89 b/18 (makeblock 0 a/18 b/18)))
   with (89 x/23[int] p/20) (makeblock 0 (int,*) x/23 p/20)))
- : bool -> bool -> bool * (bool * bool) = <fun>
|}, Principal{|
(function a/19[int] b/19[int]
  (catch
    (if a/19
      (let (x/24 =a[int] a/19 p/21 =a (makeblock 0 a/19 b/19))
        (exit 93 x/24 p/21))
      (let (x/25 =a b/19 p/22 =a (makeblock 0 a/19 b/19))
        (exit 93 x/25 p/22)))
   with (93 x/26[int] p/23) (makeblock 0 (int,*) x/26 p/23)))
(function a/19[int] b/19[int]
  (catch
    (if a/19 (exit 93 a/19 (makeblock 0 a/19 b/19))
      (exit 93 b/19 (makeblock 0 a/19 b/19)))
   with (93 x/26[int] p/23) (makeblock 0 (int,*) x/26 p/23)))
- : bool -> bool -> bool * (bool * bool) = <fun>
|}, Rectypes{|
(function a/20[int] b/20[int]
  (catch
    (if a/20
      (let (x/27 =a[int] a/20 p/24 =a (makeblock 0 a/20 b/20))
        (exit 97 x/27 p/24))
      (let (x/28 =a b/20 p/25 =a (makeblock 0 a/20 b/20))
        (exit 97 x/28 p/25)))
   with (97 x/29[int] p/26) (makeblock 0 (int,*) x/29 p/26)))
(function a/20[int] b/20[int]
  (catch
    (if a/20 (exit 97 a/20 (makeblock 0 a/20 b/20))
      (exit 97 b/20 (makeblock 0 a/20 b/20)))
   with (97 x/29[int] p/26) (makeblock 0 (int,*) x/29 p/26)))
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

(* here flattening is an optimisation: the allocation is moved as an
   alias within each branch, and in the first branch it is unused and
   will be removed by simplification, so the final code
   (see the -dlambda output) will not allocate in the first branch. *)
let _ = fun a b -> match a, b with
| (true as x, _) as _p -> x, (true, true)
| (false as x, _) as p -> x, p
(* outside, onecase *)
[%%expect {|
(function a/21[int] b/21[int]
  (if a/21
    (let (x/30 =a[int] a/21 _p/0 =a (makeblock 0 a/21 b/21))
      (makeblock 0 (int,*) x/30 [0: 1 1]))
    (let (x/31 =a[int] a/21 p/27 =a (makeblock 0 a/21 b/21))
      (makeblock 0 (int,*) x/31 p/27))))
(function a/21[int] b/21[int]
  (if a/21 (makeblock 0 (int,*) a/21 [0: 1 1])
    (makeblock 0 (int,*) a/21 (makeblock 0 a/21 b/21))))
- : bool -> bool -> bool * (bool * bool) = <fun>
|}, Principal{|
(function a/22[int] b/22[int]
  (if a/22
    (let (x/32 =a[int] a/22 _p/1 =a (makeblock 0 a/22 b/22))
      (makeblock 0 (int,*) x/32 [0: 1 1]))
    (let (x/33 =a[int] a/22 p/28 =a (makeblock 0 a/22 b/22))
      (makeblock 0 (int,*) x/33 p/28))))
(function a/22[int] b/22[int]
  (if a/22 (makeblock 0 (int,*) a/22 [0: 1 1])
    (makeblock 0 (int,*) a/22 (makeblock 0 a/22 b/22))))
- : bool -> bool -> bool * (bool * bool) = <fun>
|}, Rectypes{|
(function a/23[int] b/23[int]
  (if a/23
    (let (x/34 =a[int] a/23 _p/2 =a (makeblock 0 a/23 b/23))
      (makeblock 0 (int,*) x/34 [0: 1 1]))
    (let (x/35 =a[int] a/23 p/29 =a (makeblock 0 a/23 b/23))
      (makeblock 0 (int,*) x/35 p/29))))
(function a/23[int] b/23[int]
  (if a/23 (makeblock 0 (int,*) a/23 [0: 1 1])
    (makeblock 0 (int,*) a/23 (makeblock 0 a/23 b/23))))
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false as x, _) as p) -> x, p
(* inside, onecase *)
[%%expect{|
(function a/24[int] b/24
  (let (x/36 =a[int] a/24 p/30 =a (makeblock 0 a/24 b/24))
    (makeblock 0 (int,*) x/36 p/30)))
(function a/24[int] b/24 (makeblock 0 (int,*) a/24 (makeblock 0 a/24 b/24)))
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}, Principal{|
(function a/25[int] b/25
  (let (x/37 =a[int] a/25 p/31 =a (makeblock 0 a/25 b/25))
    (makeblock 0 (int,*) x/37 p/31)))
(function a/25[int] b/25 (makeblock 0 (int,*) a/25 (makeblock 0 a/25 b/25)))
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}, Rectypes{|
(function a/26[int] b/26
  (let (x/38 =a[int] a/26 p/32 =a (makeblock 0 a/26 b/26))
    (makeblock 0 (int,*) x/38 p/32)))
(function a/26[int] b/26 (makeblock 0 (int,*) a/26 (makeblock 0 a/26 b/26)))
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

type 'a tuplist = Nil | Cons of ('a * 'a tuplist)
[%%expect{|
0
0
type 'a tuplist = Nil | Cons of ('a * 'a tuplist)
|}]

(* another example where we avoid an allocation in the first case *)
let _ =fun a b -> match a, b with
| (true, Cons p) -> p
| (_, _) as p -> p
(* outside, tuplist *)
[%%expect {|
(function a/27[int] b/27
  (catch
    (if a/27 (if b/27 (let (p/33 =a (field_imm 0 b/27)) p/33) (exit 122))
      (exit 122))
   with (122) (let (p/34 =a (makeblock 0 a/27 b/27)) p/34)))
(function a/27[int] b/27
  (catch (if a/27 (if b/27 (field_imm 0 b/27) (exit 122)) (exit 122))
   with (122) (makeblock 0 a/27 b/27)))
- : bool -> bool tuplist -> bool * bool tuplist = <fun>
|}, Principal{|
(function a/28[int] b/28
  (catch
    (if a/28 (if b/28 (let (p/35 =a (field_imm 0 b/28)) p/35) (exit 126))
      (exit 126))
   with (126) (let (p/36 =a (makeblock 0 a/28 b/28)) p/36)))
(function a/28[int] b/28
  (catch (if a/28 (if b/28 (field_imm 0 b/28) (exit 126)) (exit 126))
   with (126) (makeblock 0 a/28 b/28)))
- : bool -> bool tuplist -> bool * bool tuplist = <fun>
|}, Rectypes{|
(function a/29[int] b/29
  (catch
    (if a/29 (if b/29 (let (p/37 =a (field_imm 0 b/29)) p/37) (exit 130))
      (exit 130))
   with (130) (let (p/38 =a (makeblock 0 a/29 b/29)) p/38)))
(function a/29[int] b/29
  (catch (if a/29 (if b/29 (field_imm 0 b/29) (exit 130)) (exit 130))
   with (130) (makeblock 0 a/29 b/29)))
- : bool -> bool tuplist -> bool * bool tuplist = <fun>
|}]

let _ = fun a b -> match a, b with
| (true, Cons p)
| ((_, _) as p) -> p
(* inside, tuplist *)
[%%expect{|
(function a/30[int] b/30
  (catch
    (catch
      (if a/30
        (if b/30 (let (p/39 =a (field_imm 0 b/30)) (exit 134 p/39))
          (exit 135))
        (exit 135))
     with (135) (let (p/40 =a (makeblock 0 a/30 b/30)) (exit 134 p/40)))
   with (134 p/41) p/41))
(function a/30[int] b/30
  (catch
    (catch
      (if a/30 (if b/30 (exit 134 (field_imm 0 b/30)) (exit 135)) (exit 135))
     with (135) (exit 134 (makeblock 0 a/30 b/30)))
   with (134 p/41) p/41))
- : bool -> bool tuplist -> bool * bool tuplist = <fun>
|}, Principal{|
(function a/31[int] b/31
  (catch
    (catch
      (if a/31
        (if b/31 (let (p/42 =a (field_imm 0 b/31)) (exit 139 p/42))
          (exit 140))
        (exit 140))
     with (140) (let (p/43 =a (makeblock 0 a/31 b/31)) (exit 139 p/43)))
   with (139 p/44) p/44))
(function a/31[int] b/31
  (catch
    (catch
      (if a/31 (if b/31 (exit 139 (field_imm 0 b/31)) (exit 140)) (exit 140))
     with (140) (exit 139 (makeblock 0 a/31 b/31)))
   with (139 p/44) p/44))
- : bool -> bool tuplist -> bool * bool tuplist = <fun>
|}, Rectypes{|
(function a/32[int] b/32
  (catch
    (catch
      (if a/32
        (if b/32 (let (p/45 =a (field_imm 0 b/32)) (exit 144 p/45))
          (exit 145))
        (exit 145))
     with (145) (let (p/46 =a (makeblock 0 a/32 b/32)) (exit 144 p/46)))
   with (144 p/47) p/47))
(function a/32[int] b/32
  (catch
    (catch
      (if a/32 (if b/32 (exit 144 (field_imm 0 b/32)) (exit 145)) (exit 145))
     with (145) (exit 144 (makeblock 0 a/32 b/32)))
   with (144 p/47) p/47))
- : bool -> bool tuplist -> bool * bool tuplist = <fun>
|}]
