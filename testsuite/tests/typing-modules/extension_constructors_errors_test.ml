(* TEST
 expect;
*)

type t = ..;;

module M : sig type t += E | F end = struct type t += E | F of int end;;
[%%expect{|
type t = ..
Line 3, characters 37-70:
3 | module M : sig type t += E | F end = struct type t += E | F of int end;;
                                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig type t += E | F of int  end
       is not included in
         sig type t += E | F  end
|}];;

module M1 : sig type t += A end = struct type t += private A end;;
[%%expect{|
Line 1, characters 34-64:
1 | module M1 : sig type t += A end = struct type t += private A end;;
                                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig type t += private A end
       is not included in
         sig type t += A end
|}];;

module M2 : sig type t += A end = struct type t += private A | B end;;
[%%expect{|
Line 1, characters 34-68:
1 | module M2 : sig type t += A end = struct type t += private A | B end;;
                                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig type t += private A | B  end
       is not included in
         sig type t += A end
|}];;
