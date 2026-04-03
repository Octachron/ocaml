(* TEST
 readonly_files = "contexts_1.ml contexts_2.ml contexts_3.ml";
 flags = "-dsource -dlambda -dcanonical-ids";
 expect;
*)

#use "contexts_1.ml";;
(* Notice that (field_mut 1 input) occurs twice, it
   is evaluated once in the 'false' branch and once in the 'true'
   branch. The compiler does not assume that its static knowledge about the
   first read (it cannot be a [Right] as we already matched against it
   and failed) also applies to the second read, and it inserts a Match_failure
   case if [Right] is read again.
*)
[%%expect {|

#use  "contexts_1.ml";;

type u = {
  a: bool ;
  mutable b: (bool, int) Either.t };;
0
type u = { a : bool; mutable b : (bool, int) Either.t; }

let example_1 () =
  let input = { a = true; b = (Either.Left true) } in
  match input with
  | { a = false; b = _ } -> Result.Error 1
  | { a = _; b = Either.Right _ } -> Result.Error 2
  | { a = _; b = _ } when input.b <- (Either.Right 3); false ->
      Result.Error 3
  | { a = true; b = Either.Left y } -> Result.Ok y;;
(let
  (example_1/0 =
     (function param/0[int]
       (let (input/0 = (makemutable 0 (int,*) 1 [0: 1]))
         (if (field_int 0 input/0)
           (let (*match*/0 =o (field_mut 1 input/0))
             (switch* *match*/0
              case tag 0:
               (if (seq (setfield_ptr 1 input/0 [1: 3]) 0) [1: 3]
                 (let (*match*/1 =o (field_mut 1 input/0))
                   (switch* *match*/1
                    case tag 0: (makeblock 0 (int) (field_imm 0 *match*/1))
                    case tag 1:
                     (raise
                       (makeblock 0 (global Match_failure/0!)
                         [0: "contexts_1.ml" 17 2])))))
              case tag 1: [1: 2]))
           [1: 1]))))
  (apply (field_mut 1 (global Toploop!)) "example_1" example_1/0))
val example_1 : unit -> (bool, int) Result.t = <fun>
|}, Principal{|

#use  "contexts_1.ml";;

type u = {
  a: bool ;
  mutable b: (bool, int) Either.t };;
0
type u = { a : bool; mutable b : (bool, int) Either.t; }

let example_1 () =
  let input = { a = true; b = (Either.Left true) } in
  match input with
  | { a = false; b = _ } -> Result.Error 1
  | { a = _; b = Either.Right _ } -> Result.Error 2
  | { a = _; b = _ } when input.b <- (Either.Right 3); false ->
      Result.Error 3
  | { a = true; b = Either.Left y } -> Result.Ok y;;
(let
  (example_1/1 =
     (function param/1[int]
       (let (input/1 = (makemutable 0 (int,*) 1 [0: 1]))
         (if (field_int 0 input/1)
           (let (*match*/2 =o (field_mut 1 input/1))
             (switch* *match*/2
              case tag 0:
               (if (seq (setfield_ptr 1 input/1 [1: 3]) 0) [1: 3]
                 (let (*match*/3 =o (field_mut 1 input/1))
                   (switch* *match*/3
                    case tag 0: (makeblock 0 (int) (field_imm 0 *match*/3))
                    case tag 1:
                     (raise
                       (makeblock 0 (global Match_failure/0!)
                         [0: "contexts_1.ml" 17 2])))))
              case tag 1: [1: 2]))
           [1: 1]))))
  (apply (field_mut 1 (global Toploop!)) "example_1" example_1/1))
val example_1 : unit -> (bool, int) Result.t = <fun>
|}, Rectypes{|

#use  "contexts_1.ml";;

type u = {
  a: bool ;
  mutable b: (bool, int) Either.t };;
0
type u = { a : bool; mutable b : (bool, int) Either.t; }

let example_1 () =
  let input = { a = true; b = (Either.Left true) } in
  match input with
  | { a = false; b = _ } -> Result.Error 1
  | { a = _; b = Either.Right _ } -> Result.Error 2
  | { a = _; b = _ } when input.b <- (Either.Right 3); false ->
      Result.Error 3
  | { a = true; b = Either.Left y } -> Result.Ok y;;
(let
  (example_1/2 =
     (function param/2[int]
       (let (input/2 = (makemutable 0 (int,*) 1 [0: 1]))
         (if (field_int 0 input/2)
           (let (*match*/4 =o (field_mut 1 input/2))
             (switch* *match*/4
              case tag 0:
               (if (seq (setfield_ptr 1 input/2 [1: 3]) 0) [1: 3]
                 (let (*match*/5 =o (field_mut 1 input/2))
                   (switch* *match*/5
                    case tag 0: (makeblock 0 (int) (field_imm 0 *match*/5))
                    case tag 1:
                     (raise
                       (makeblock 0 (global Match_failure/0!)
                         [0: "contexts_1.ml" 17 2])))))
              case tag 1: [1: 2]))
           [1: 1]))))
  (apply (field_mut 1 (global Toploop!)) "example_1" example_1/2))
val example_1 : unit -> (bool, int) Result.t = <fun>
|}]

#use "contexts_2.ml";;
[%%expect {|

#use  "contexts_2.ml";;

type 'a myref = {
  mutable mut: 'a };;
0
type 'a myref = { mutable mut : 'a; }

type u = {
  a: bool ;
  b: (bool, int) Either.t myref };;
0
type u = { a : bool; b : (bool, int) Either.t myref; }

let example_2 () =
  let input = { a = true; b = { mut = (Either.Left true) } } in
  match input with
  | { a = false; b = _ } -> Result.Error 1
  | { a = _; b = { mut = Either.Right _ } } -> Result.Error 2
  | { a = _; b = _ } when (input.b).mut <- (Either.Right 3); false ->
      Result.Error 3
  | { a = true; b = { mut = Either.Left y } } -> Result.Ok y;;
(let
  (example_2/0 =
     (function param/3[int]
       (let (input/3 = (makeblock 0 (int,*) 1 (makemutable 0 [0: 1])))
         (if (field_int 0 input/3)
           (let (*match*/6 =o (field_mut 0 (field_imm 1 input/3)))
             (switch* *match*/6
              case tag 0:
               (if (seq (setfield_ptr 0 (field_imm 1 input/3) [1: 3]) 0)
                 [1: 3]
                 (let (*match*/7 =o (field_mut 0 (field_imm 1 input/3)))
                   (switch* *match*/7
                    case tag 0: (makeblock 0 (int) (field_imm 0 *match*/7))
                    case tag 1:
                     (raise
                       (makeblock 0 (global Match_failure/0!)
                         [0: "contexts_2.ml" 11 2])))))
              case tag 1: [1: 2]))
           [1: 1]))))
  (apply (field_mut 1 (global Toploop!)) "example_2" example_2/0))
val example_2 : unit -> (bool, int) Result.t = <fun>
|}, Principal{|

#use  "contexts_2.ml";;

type 'a myref = {
  mutable mut: 'a };;
0
type 'a myref = { mutable mut : 'a; }

type u = {
  a: bool ;
  b: (bool, int) Either.t myref };;
0
type u = { a : bool; b : (bool, int) Either.t myref; }

let example_2 () =
  let input = { a = true; b = { mut = (Either.Left true) } } in
  match input with
  | { a = false; b = _ } -> Result.Error 1
  | { a = _; b = { mut = Either.Right _ } } -> Result.Error 2
  | { a = _; b = _ } when (input.b).mut <- (Either.Right 3); false ->
      Result.Error 3
  | { a = true; b = { mut = Either.Left y } } -> Result.Ok y;;
(let
  (example_2/1 =
     (function param/4[int]
       (let (input/4 = (makeblock 0 (int,*) 1 (makemutable 0 [0: 1])))
         (if (field_int 0 input/4)
           (let (*match*/8 =o (field_mut 0 (field_imm 1 input/4)))
             (switch* *match*/8
              case tag 0:
               (if (seq (setfield_ptr 0 (field_imm 1 input/4) [1: 3]) 0)
                 [1: 3]
                 (let (*match*/9 =o (field_mut 0 (field_imm 1 input/4)))
                   (switch* *match*/9
                    case tag 0: (makeblock 0 (int) (field_imm 0 *match*/9))
                    case tag 1:
                     (raise
                       (makeblock 0 (global Match_failure/0!)
                         [0: "contexts_2.ml" 11 2])))))
              case tag 1: [1: 2]))
           [1: 1]))))
  (apply (field_mut 1 (global Toploop!)) "example_2" example_2/1))
val example_2 : unit -> (bool, int) Result.t = <fun>
|}, Rectypes{|

#use  "contexts_2.ml";;

type 'a myref = {
  mutable mut: 'a };;
0
type 'a myref = { mutable mut : 'a; }

type u = {
  a: bool ;
  b: (bool, int) Either.t myref };;
0
type u = { a : bool; b : (bool, int) Either.t myref; }

let example_2 () =
  let input = { a = true; b = { mut = (Either.Left true) } } in
  match input with
  | { a = false; b = _ } -> Result.Error 1
  | { a = _; b = { mut = Either.Right _ } } -> Result.Error 2
  | { a = _; b = _ } when (input.b).mut <- (Either.Right 3); false ->
      Result.Error 3
  | { a = true; b = { mut = Either.Left y } } -> Result.Ok y;;
(let
  (example_2/2 =
     (function param/5[int]
       (let (input/5 = (makeblock 0 (int,*) 1 (makemutable 0 [0: 1])))
         (if (field_int 0 input/5)
           (let (*match*/10 =o (field_mut 0 (field_imm 1 input/5)))
             (switch* *match*/10
              case tag 0:
               (if (seq (setfield_ptr 0 (field_imm 1 input/5) [1: 3]) 0)
                 [1: 3]
                 (let (*match*/11 =o (field_mut 0 (field_imm 1 input/5)))
                   (switch* *match*/11
                    case tag 0: (makeblock 0 (int) (field_imm 0 *match*/11))
                    case tag 1:
                     (raise
                       (makeblock 0 (global Match_failure/0!)
                         [0: "contexts_2.ml" 11 2])))))
              case tag 1: [1: 2]))
           [1: 1]))))
  (apply (field_mut 1 (global Toploop!)) "example_2" example_2/2))
val example_2 : unit -> (bool, int) Result.t = <fun>
|}]

#use "contexts_3.ml";;
[%%expect {|

#use  "contexts_3.ml";;

type 'a myref = {
  mutable mut: 'a };;
0
type 'a myref = { mutable mut : 'a; }

type u = (bool * (bool, int) Either.t) myref;;
0
type u = (bool * (bool, int) Either.t) myref

let example_3 () =
  let input = { mut = (true, (Either.Left true)) } in
  match input with
  | { mut = (false, _) } -> Result.Error 1
  | { mut = (_, Either.Right _) } -> Result.Error 2
  | { mut = (_, _) } when input.mut <- (true, (Either.Right 3)); false ->
      Result.Error 3
  | { mut = (true, Either.Left y) } -> Result.Ok y;;
(let
  (example_3/0 =
     (function param/6[int]
       (let (input/6 =mut [0: 1 [0: 1]] *match*/12 =o *input/6)
         (if (field_imm 0 *match*/12)
           (switch* (field_imm 1 *match*/12)
            case tag 0:
             (if (seq (assign input/6 [0: 1 [1: 3]]) 0) [1: 3]
               (makeblock 0 (int) (field_imm 0 (field_imm 1 *match*/12))))
            case tag 1: [1: 2])
           [1: 1]))))
  (apply (field_mut 1 (global Toploop!)) "example_3" example_3/0))
val example_3 : unit -> (bool, int) Result.t = <fun>
|}, Principal{|

#use  "contexts_3.ml";;

type 'a myref = {
  mutable mut: 'a };;
0
type 'a myref = { mutable mut : 'a; }

type u = (bool * (bool, int) Either.t) myref;;
0
type u = (bool * (bool, int) Either.t) myref

let example_3 () =
  let input = { mut = (true, (Either.Left true)) } in
  match input with
  | { mut = (false, _) } -> Result.Error 1
  | { mut = (_, Either.Right _) } -> Result.Error 2
  | { mut = (_, _) } when input.mut <- (true, (Either.Right 3)); false ->
      Result.Error 3
  | { mut = (true, Either.Left y) } -> Result.Ok y;;
(let
  (example_3/1 =
     (function param/7[int]
       (let (input/7 =mut [0: 1 [0: 1]] *match*/13 =o *input/7)
         (if (field_imm 0 *match*/13)
           (switch* (field_imm 1 *match*/13)
            case tag 0:
             (if (seq (assign input/7 [0: 1 [1: 3]]) 0) [1: 3]
               (makeblock 0 (int) (field_imm 0 (field_imm 1 *match*/13))))
            case tag 1: [1: 2])
           [1: 1]))))
  (apply (field_mut 1 (global Toploop!)) "example_3" example_3/1))
val example_3 : unit -> (bool, int) Result.t = <fun>
|}, Rectypes{|

#use  "contexts_3.ml";;

type 'a myref = {
  mutable mut: 'a };;
0
type 'a myref = { mutable mut : 'a; }

type u = (bool * (bool, int) Either.t) myref;;
0
type u = (bool * (bool, int) Either.t) myref

let example_3 () =
  let input = { mut = (true, (Either.Left true)) } in
  match input with
  | { mut = (false, _) } -> Result.Error 1
  | { mut = (_, Either.Right _) } -> Result.Error 2
  | { mut = (_, _) } when input.mut <- (true, (Either.Right 3)); false ->
      Result.Error 3
  | { mut = (true, Either.Left y) } -> Result.Ok y;;
(let
  (example_3/2 =
     (function param/8[int]
       (let (input/8 =mut [0: 1 [0: 1]] *match*/14 =o *input/8)
         (if (field_imm 0 *match*/14)
           (switch* (field_imm 1 *match*/14)
            case tag 0:
             (if (seq (assign input/8 [0: 1 [1: 3]]) 0) [1: 3]
               (makeblock 0 (int) (field_imm 0 (field_imm 1 *match*/14))))
            case tag 1: [1: 2])
           [1: 1]))))
  (apply (field_mut 1 (global Toploop!)) "example_3" example_3/2))
val example_3 : unit -> (bool, int) Result.t = <fun>
|}]
