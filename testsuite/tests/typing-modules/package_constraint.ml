(* TEST
 expect;
*)

(* You may constrain abstract types in packages. *)
module type S = sig
  type t
end

type m = (module S with type t = int);;
[%%expect{|
module type S = sig type t end
type m = (module S with type t = int)
|}];;

(* You may use variables in the current environment in the new definitions. *)
module type S = sig
  type t
end

type 'a m = (module S with type t = 'a);;
[%%expect{|
module type S = sig type t end
type 'a m = (module S with type t = 'a)
|}];;

(* It works with non-trivial paths. *)
module type S = sig
  module M : sig
    type t
  end
end

type m = (module S with type M.t = int)
[%%expect{|
module type S = sig module M : sig type t end end
type m = (module S with type M.t = int)
|}];;

(* It should respect immediacy - [m1] should typecheck but not [m2]. *)
module type S = sig
  type t [@@immediate]
end

type m1 = (module S with type t = int)
type m2 = (module S with type t = string);;
[%%expect{|
module type S = sig type t [@@immediate] end
type m1 = (module S with type t = int)
Line 6, characters 10-41:
6 | type m2 = (module S with type t = string);;
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this "with" constraint, the new definition of "t"
       does not match its original definition in the constrained signature:
       Type declarations do not match:
         type t = string
       is not included in
         type t [@@immediate]
       The first is not an immediate type.
|}];;

(* You may not constrain types with a manifest in a package *)
module type S = sig
  type t = int
end

type m = (module S with type t = string);;
[%%expect{|
module type S = sig type t = int end
Line 5, characters 9-40:
5 | type m = (module S with type t = string);;
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this "with" constraint, the new definition of "t"
       does not match its original definition in the constrained signature:
       Type declarations do not match:
         type t = string
       is not included in
         type t = int
       The type "string" is not equal to the type "int"
|}];;

(* Even if your constraint would be satisfied. *)
(* It would be nice if this worked. *)
module type S = sig
  type t = int
end

type m = (module S with type t = int);;
[%%expect{|
module type S = sig type t = int end
type m = (module S with type t = int)
|}];;

(* And even if the manifest is not obvious in the original definition. *)
module M = struct
  type t
end

module type S = sig
  module P = M
end

type m = (module S with type P.t = int);;
[%%expect{|
module M : sig type t end
module type S = sig module P = M end
Line 9, characters 9-39:
9 | type m = (module S with type P.t = int);;
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this "with" constraint, the new definition of "P.t"
       does not match its original definition in the constrained signature:
       Type declarations do not match:
         type t = int
       is not included in
         type t = M.t
       The type "int" is not equal to the type "M.t"
|}];;

(* If writing a package constraint in a mutually recursive group of type decls,
   checking that the constraint's immediacy may not rely on the definitions of
   other elements of the mutually recursive group. *)
(* It would be nice if this worked. *)
module type S = sig
  type t [@@immediate]
end

type t1 = int
and t2 = (module S with type t = t1);;
[%%expect{|
module type S = sig type t [@@immediate] end
Line 6, characters 9-36:
6 | and t2 = (module S with type t = t1);;
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this "with" constraint, the new definition of "t"
       does not match its original definition in the constrained signature:
       Type declarations do not match:
         type t = t1
       is not included in
         type t [@@immediate]
       The first is not an immediate type.
|}];;

(* When using a package with constraint to give an abstract type a definition
   that is immediate, that immediacy information should be usable after
   unpacking. *)
module type S = sig
  type t
end

type m = (module S with type t = int)

module F (X : sig val x : m end) = struct
  module M = (val X.x)
  type t = M.t [@@immediate]
end;;
[%%expect{|
module type S = sig type t end
type m = (module S with type t = int)
module F :
  functor (X : sig val x : m end) ->
    sig module M : sig type t = int end type t = M.t [@@immediate] end
|}];;

(* Checking such a constraint may require expanding definitions from the module
   being updated. *)
module type S = sig
  module type S1 = sig
    type t
  end
  module M : S1
end

type t = (module S with type M.t = int)
[%%expect{|
module type S = sig module type S1 = sig type t end module M : S1 end
type t = (module S with type M.t = int)
|}];;
