(* TEST
 flags = "-I ${ocamlsrcdir}/typing -I ${ocamlsrcdir}/parsing";
 include ocamlcommon;
 expect;
*)

let run s =
  let pe = Parse.expression (Lexing.from_string s) in
  let te = Typecore.type_expression Env.initial pe in
  let ute = Untypeast.untype_expression te in
  Format.printf "%a@." Pprintast.expression ute
;;

[%%expect{|
val run : string -> unit = <fun>
|}];;

run {| match None with Some (Some _) -> () | _ -> () |};;

[%%expect{|
match None with | Some (Some _) -> () | _ -> ()
- : unit = ()
|}];;

run {| let open struct type t = { mutable x : int [@atomic] } end in
       let _ = fun (v : t) -> [%atomic.loc v.x] in () |};;
[%%expect{|
let open struct type t = {
                  mutable x: int [@atomic ]} end in
  let _ = fun (v : t) -> [%ocaml.atomic.loc v.x] in ()
- : unit = ()
|}];;

(***********************************)
(* Untypeast/pprintast maintain the arity of a function. *)

(* 4-ary function *)
run {| fun x y z -> function w -> x y z w |};;

[%%expect{|
fun x y z -> function | w -> x y z w
- : unit = ()
|}];;

(* 3-ary function returning a 1-ary function *)
run {| fun x y z -> (function w -> x y z w) |};;

[%%expect{|
fun x y z -> (function | w -> x y z w)
- : unit = ()
|}];;

(***********************************)
(* Untypeast/pprintast correctly handle value binding type annotations. *)

run {| let foo : 'a. 'a -> 'a = fun x -> x in foo |}

[%%expect{|
let foo : 'a . 'a -> 'a = fun x -> x in foo
- : unit = ()
|}];;

run {| let foo : type a . a -> a = fun x -> x in foo |}

[%%expect{|
let foo : 'a . 'a -> 'a = fun (type a) -> (fun x -> x : a -> a) in foo
- : unit = ()
|}];;

run {|
  let module MS = struct module type S = sig end end in
  (fun _ -> ())
    (fun (module M1 : MS.S) ((module M2) : (module MS.S)) ->
      (module M1 : MS.S), ((module M2) : (module MS.S)))
|};;

[%%expect{|
let module MS = struct module type S  = sig  end end in
  (fun _ -> ())
    (fun (module M1 : MS.S) ((module M2)  : (module MS.S)) ->
       (((module M1) : (module MS.S)), ((module M2) : (module MS.S))))
- : unit = ()
|}];;

run {|
  let module M = struct type t = { x : int } end in
  fun x -> let M.{ x } = M.{ x } in x
|};;

[%%expect{|
let module M = struct type t = {
                        x: int } end in
  fun x -> let M.{ x }  = let open M in { x } in x
- : unit = ()
|}];;

let run s =
  let pe = Parse.implementation (Lexing.from_string s) in
  let te,_,_,_,_ = Typemod.type_structure Env.initial pe in
  let ute = Untypeast.untype_structure te in
  Format.printf "%a@." Pprintast.structure ute
;;

[%%expect{|
val run : string -> unit = <fun>
|}];;

(* That test would hang before ocaml/ocaml#14105 *)
run {|type t = (::);; let f (x : t) = match x with (::) -> 4|}

[%%expect{|
type t =
  | (::)
let f (x : t) = match x with | (::) -> 4
- : unit = ()
|}];;

run {|
let f g = g ()
let g ?x ~y () = ()
let () = f (g ~y:())
|}
[%%expect {|
let f g = g ()
let g ?x ~y () = ()
let () = f (let arg = g ~y:() in fun eta -> arg ?x:None eta)
- : unit = ()
|}];;


run {|
module type T = sig type t val x: t end
let f = function (module M:T) -> M.x
module I = struct type t = int let x = 0 end
let i = f (module I)
|}
[%%expect {|
Exception:
Typecore.Error
 ({Location.loc_start =
    {Lexing.pos_fname = ""; pos_lnum = 3; pos_bol = 41; pos_cnum = 74};
   loc_end =
    {Lexing.pos_fname = ""; pos_lnum = 3; pos_bol = 41; pos_cnum = 77};
   loc_ghost = false},
 <abstr>,
 Typecore.Expr_type_clash
  ({Errortrace.trace =
     [Errortrace.Diff
       {Errortrace.got = {Errortrace.ty = <abstr>; expanded = <abstr>};
        expected = {Errortrace.ty = <abstr>; expanded = <abstr>}};
      Errortrace.Escape
       {Errortrace.kind =
         Errortrace.Constructor (Path.Pdot (Path.Pident <abstr>, "t"));
        context = None}]},
  None,
  Some
   {Parsetree.pexp_desc =
     Parsetree.Pexp_ident
      {Asttypes.txt =
        Longident.Ldot
         ({Location.txt = Longident.Lident "M";
           loc =
            {Location.loc_start =
              {Lexing.pos_fname = ""; pos_lnum = 3; pos_bol = 41;
               pos_cnum = 74};
             loc_end =
              {Lexing.pos_fname = ""; pos_lnum = 3; pos_bol = 41;
               pos_cnum = 75};
             loc_ghost = false}},
         {Location.txt = "x";
          loc =
           {Location.loc_start =
             {Lexing.pos_fname = ""; pos_lnum = 3; pos_bol = 41;
              pos_cnum = 76};
            loc_end =
             {Lexing.pos_fname = ""; pos_lnum = 3; pos_bol = 41;
              pos_cnum = 77};
            loc_ghost = false}});
       loc =
        {Location.loc_start =
          {Lexing.pos_fname = ""; pos_lnum = 3; pos_bol = 41; pos_cnum = 74};
         loc_end =
          {Lexing.pos_fname = ""; pos_lnum = 3; pos_bol = 41; pos_cnum = 77};
         loc_ghost = false}};
    pexp_loc =
     {Location.loc_start =
       {Lexing.pos_fname = ""; pos_lnum = 3; pos_bol = 41; pos_cnum = 74};
      loc_end =
       {Lexing.pos_fname = ""; pos_lnum = 3; pos_bol = 41; pos_cnum = 77};
      loc_ghost = false};
    pexp_loc_stack = []; pexp_attributes = []})).
|}];;

run {|
module type T = sig type t val x: t end
let f (g: (module M:T) -> M.t) (module M:T) = g (module M)
let g ?x ~y (module M:T) = M.x
let u = f (g ~y:())
|}
[%%expect{|
Line 4, characters 7-8:
1 | .
2 | ..
3 |
4 | .e.........
Warning 16 [unerasable-optional-argument]: this optional argument cannot be erased.
module type T  = sig type t val x : t end
let f (g : (module M : T) -> M.t) (module M : T) = g (module M)
let g ?x ~y (module M : T) = M.x
let u =
  f (let arg = g ~y:() in fun (module Eta : T) -> arg ?x:None (module Eta))

- : unit = ()
|}]
