(*TEST
  expect;
*)
(* Error messages for non syntactic type mismatches *)
module type A = sig type a = char type b type c = float  end
module type B = sig type a type b = int type c type err end
let f (x: (module A with type b = int))=
  (x:(module B with type a = char and type c = float and type err = string))
[%%expect {|
module type A = sig type a = char type b type c = float end
module type B = sig type a type b = int type c type err end
Line 4, characters 3-4:
4 |   (x:(module B with type a = char and type c = float and type err = string))
       ^
Error: The value "x" has type "(module A with type b = int)"
       but an expression was expected of type
         "(module B with type a = char and type c = float and type err =
          string)"
       There is no type "err" in the first module type.
|}]
