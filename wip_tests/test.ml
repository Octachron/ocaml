(* TEST
  * expect
*)

module type Arg = sig
    module type A
    module type Honorificabilitudinitatibus
    module X:   Honorificabilitudinitatibus
    module Y:   A
end

module I: Arg = struct
  module type A = sig type t = T end
  module type Honorificabilitudinitatibus = sig type u = U end
  module X = struct type u = U end
  module Y = struct type t = T end
end;;
[%%expect {|
module type Arg =
  sig
    module type A
    module type Honorificabilitudinitatibus
    module X : Honorificabilitudinitatibus
    module Y : A
  end
module I : Arg
|}];;

module S = struct
  open I
  module G
      (_:A)(_:A)(_:A)(_:A)
      (_:A)(_:A)(_:A)(_:A)
      (_:A)(_:A)(_:A)(_:A) =
  struct end
  module Error:
    Honorificabilitudinitatibus -> A -> Honorificabilitudinitatibus -> A ->
    Honorificabilitudinitatibus -> A -> Honorificabilitudinitatibus -> A ->
    Honorificabilitudinitatibus -> A -> Honorificabilitudinitatibus -> A ->
   sig end  = G(Y)
end;;
[%%expect {|
Line 12, characters 14-18:
12 |    sig end  = G(Y)
                   ^^^^
Error: Signature mismatch:
       Modules do not match:
         functor (Arg : I.A) (Arg : I.A) (Arg : I.A) (Arg : I.A) (Arg :
           I.A) (Arg : I.A) (Arg : I.A) (Arg : I.A) (Arg : I.A) (Arg :
           I.A) (Arg : I.A) -> sig end
       is not included in
         I.Honorificabilitudinitatibus -> I.A ->
           I.Honorificabilitudinitatibus -> I.A ->
           I.Honorificabilitudinitatibus -> I.A ->
           I.Honorificabilitudinitatibus -> I.A ->
           I.Honorificabilitudinitatibus -> I.A ->
           I.Honorificabilitudinitatibus -> I.A -> sig end
       Parameters do not match:
         (Arg : I.A) (Arg : I.A) (Arg : I.A) (Arg : I.A) (Arg : I.A)
         (Arg : I.A) (Arg : I.A) (Arg : I.A) (Arg : I.A) (Arg : I.A)
         (Arg : I.A)
       does not match
         I.Honorificabilitudinitatibus I.A I.Honorificabilitudinitatibus
         I.A I.Honorificabilitudinitatibus I.A I.Honorificabilitudinitatibus
         I.A I.Honorificabilitudinitatibus I.A I.Honorificabilitudinitatibus
         I.A
|}];;


module S = struct
  open I
  module G
      (_:A)(_:A)(_:A)(_:A)
      (_:A)(_:A)(_:A)(_:A)
      (_:A)(_:A)(_:A)(_:A) =
  struct end
  module M = G(Y)(X)(Y)(X)
end;;
[%%expect {|
Line 8, characters 18-19:
8 |   module M = G(Y)(X)(Y)(X)
                      ^
Error: Signature mismatch:
       Modules do not match:
         I.Honorificabilitudinitatibus
       is not included in
         I.A
|}];;

module F(A:Arg)
= struct
  open A
  module G(_:A)(_:A)(_:A)(_:A) = struct end
  type u = G(X)(Y)(X)(Y)(X).t
end;;
[%%expect {|
Line 5, characters 19-20:
5 |   module Error = G(X)(Y)(X)(Y)(X)
                       ^
Error: Signature mismatch:
       Modules do not match:
         A.Honorificabilitudinitatibus
       is not included in
         A.A
|}];;
