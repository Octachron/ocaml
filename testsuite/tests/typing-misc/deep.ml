(* TEST
 expect;
*)

module M : sig
  val x : bool * int
end = struct
  let x = false , "not an int"
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let x = false , "not an int"
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig val x : bool * string end
       is not included in
         sig val x : bool * int end
       Try changing value "x" to be a "bool * int"
|}]

module T : sig
  val f : int -> (float * string option) list
end = struct
  let f x = x + List.length [0.0, Some true]
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f x = x + List.length [0.0, Some true]
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : int -> int end
       is not included in
         sig val f : int -> (float * string option) list end
       Try changing value "f" to be a "int -> (float * string option) list"
|}]

(* Alpha-equivalence *)
module T : sig
  val f : ('a list * 'b list -> int)
end = struct
  let f : ('c list * 'd option  -> int) = assert false
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f : ('c list * 'd option  -> int) = assert false
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : 'c list * 'd option -> int end
       is not included in
         sig val f : 'a list * 'b list -> int end
       Try changing value "f" to be a "'a list * 'b list -> int"
|}]

module T : sig
  type t = int * float
end = struct
  type t = bool * float
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = bool * float
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = bool * float end
       is not included in
         sig type t = int * float end
       Try changing type "t" to
       type t = int * float
|}]
