(* TEST
  expect;
*)

(* PR#12959  *)

module rec A: sig
  val x: 'a B.t -> unit
end = struct
  let x _ = ()
end
and B: sig
  type +'a t
end = struct
  type t
end
  [%%expect {|
Line 1:
Error: Modules do not match:
         sig type t end
       is not included in
         sig type 'a t end
       Type declarations do not match: type t is not included in type 'a t
       They have different arities.
|}]

module rec A: sig
  val x: 'a F(B).t -> unit
end = struct
  let x _ = ()
end
and B: sig
  type 'a t
  type x
end = struct
  type 'a t
  type x
end
and F: functor(X:sig type x end) -> sig type 'a t = 'a * X.x end =
  functor(X:sig type y end) -> struct type t = int end
  [%%expect {|
Line 1:
Error: Modules do not match:
         functor () -> sig type t = int end
       is not included in
         functor () -> sig type 'a t end
       Modules do not match:
         sig type t = int end
       is not included in
         sig type 'a t end
       Type declarations do not match:
         type t = int
       is not included in
         type 'a t
       They have different arities.
|}]
