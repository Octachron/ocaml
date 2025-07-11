(* TEST
 expect;
*)

type 'a x = private [> `x] as 'a;;
[%%expect {|
Line 1:
Error: Try changing type "x" to type 'a x
|}];;


type int;;
[%%expect {|
type int
|}];;

let x = 0;;
[%%expect {|
val x : int/2 = 0
|}];;


type float;;
[%%expect {|
type float
|}];;

0.;;
[%%expect {|
- : float/2 = 0.
|}];;
