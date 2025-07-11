(* TEST
 expect;
*)
class type ct = object end
module type s = sig type a val one:int type b class two:ct type c type exn+=Three type d end
module type c12 = sig type a class two:ct type b val one:int type c type exn+=Three type d end
module type c123 = sig type a type exn+=Three type b class two:ct type c val one:int type d end

module type expected = sig module type x = s end

module A: expected = struct module type x = c12 end
[%%expect {|
class type ct = object  end
module type s =
  sig
    type a
    val one : int
    type b
    class two : ct
    type c
    type exn += Three
    type d
  end
module type c12 =
  sig
    type a
    class two : ct
    type b
    val one : int
    type c
    type exn += Three
    type d
  end
module type c123 =
  sig
    type a
    type exn += Three
    type b
    class two : ct
    type c
    val one : int
    type d
  end
module type expected = sig module type x = s end
Line 8, characters 21-51:
8 | module A: expected = struct module type x = c12 end
                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig module type x = c12 end
       is not included in
         expected
       Try changing module type "x" to
       module type x = s
|}]

module B: expected = struct module type x = c123 end
[%%expect {|
Line 1, characters 21-52:
1 | module B: expected = struct module type x = c123 end
                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig module type x = c123 end
       is not included in
         expected
       Try changing module type "x" to
       module type x = s
|}]


module Far: sig
  module type x = sig
    val a:int
    val b: int
    val c: int
    val d: int
    val e:int
  end
end = struct
  module type x = sig
    val a:int
    val b:int
    val e:int
    val d:int
    val c:int
  end
end
[%%expect {|
Lines 9-17, characters 6-3:
 9 | ......struct
10 |   module type x = sig
11 |     val a:int
12 |     val b:int
13 |     val e:int
14 |     val d:int
15 |     val c:int
16 |   end
17 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           module type x =
             sig
               val a : int
               val b : int
               val e : int
               val d : int
               val c : int
             end
         end
       is not included in
         sig
           module type x =
             sig
               val a : int
               val b : int
               val c : int
               val d : int
               val e : int
             end
         end
       Try changing module type "x" to
       module type x =
         sig val a : int val b : int val c : int val d : int val e : int end
|}]

module Confusing: sig
  module type x= sig
    class x:ct
    val x:int
  end
end = struct
  module type x= sig
    val x:int
    class x:ct
  end
end
[%%expect {|
Lines 6-11, characters 6-3:
 6 | ......struct
 7 |   module type x= sig
 8 |     val x:int
 9 |     class x:ct
10 |   end
11 | end
Error: Signature mismatch:
       Modules do not match:
         sig module type x = sig val x : int class x : ct end end
       is not included in
         sig module type x = sig class x : ct val x : int end end
       Try changing module type "x" to
       module type x = sig class x : ct val x : int end
|}]

module MT: sig
  module type a = sig
    module type b = sig
      val x:int
      val y:int
    end
  end
end = struct
  module type a = sig
    module type b = sig
      val y:int
      val x:int
    end
  end
end
[%%expect {|
Lines 8-15, characters 6-3:
 8 | ......struct
 9 |   module type a = sig
10 |     module type b = sig
11 |       val y:int
12 |       val x:int
13 |     end
14 |   end
15 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           module type a =
             sig module type b = sig val y : int val x : int end end
         end
       is not included in
         sig
           module type a =
             sig module type b = sig val x : int val y : int end end
         end
       Try changing module type "a" to
       module type a =
         sig module type b = sig val x : int val y : int end end
|}]

class type ct = object end
module Classes: sig
  module type x = sig
    class a: ct
    class b: ct
  end
end = struct
  module type x = sig
    class b: ct
    class a: ct
  end
end
[%%expect{|
class type ct = object  end
Lines 7-12, characters 6-3:
 7 | ......struct
 8 |   module type x = sig
 9 |     class b: ct
10 |     class a: ct
11 |   end
12 | end
Error: Signature mismatch:
       Modules do not match:
         sig module type x = sig class b : ct class a : ct end end
       is not included in
         sig module type x = sig class a : ct class b : ct end end
       Try changing module type "x" to
       module type x = sig class a : ct class b : ct end
|}]

module Ext: sig
  module type x = sig
    type exn+=A
    type exn+=B
  end
end = struct
  module type x = sig
    type exn+=B
    type exn+=A
  end
end
[%%expect{|
Lines 6-11, characters 6-3:
 6 | ......struct
 7 |   module type x = sig
 8 |     type exn+=B
 9 |     type exn+=A
10 |   end
11 | end
Error: Signature mismatch:
       Modules do not match:
         sig module type x = sig type exn += B type exn += A end end
       is not included in
         sig module type x = sig type exn += A type exn += B end end
       Try changing module type "x" to
       module type x = sig type exn += A type exn += B end
|}]


module type w = sig
  module One:s
  module Two:s
end

module type w21 = sig
  module Two:s
  module One:s
end

module type wOne21 = sig
  module One:c12
  module Two:s
end

module C: sig module type x = w end = struct module type x = w21 end
[%%expect {|
module type w = sig module One : s module Two : s end
module type w21 = sig module Two : s module One : s end
module type wOne21 = sig module One : c12 module Two : s end
Line 16, characters 38-68:
16 | module C: sig module type x = w end = struct module type x = w21 end
                                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig module type x = w21 end
       is not included in
         sig module type x = w end
       Try changing module type "x" to
       module type x = w
|}]

module D: sig module type x = w end = struct module type x = wOne21 end
[%%expect {|
Line 1, characters 38-71:
1 | module D: sig module type x = w end = struct module type x = wOne21 end
                                          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig module type x = wOne21 end
       is not included in
         sig module type x = w end
       Try changing module type "x" to
       module type x = w
|}]

module F1: sig module type x = functor(X:s) -> s end =
struct
  module type x = functor(X:c12) -> s
end
[%%expect {|
Lines 2-4, characters 0-3:
2 | struct
3 |   module type x = functor(X:c12) -> s
4 | end
Error: Signature mismatch:
       Modules do not match:
         sig module type x = (X : c12) -> s end
       is not included in
         sig module type x = (X : s) -> s end
       Try changing module type "x" to
       module type x = (X : s) -> s
|}]

module F2: sig module type x = functor(X:s) -> s end =
struct
  module type x = functor(X:s) -> c12
end
[%%expect {|
Lines 2-4, characters 0-3:
2 | struct
3 |   module type x = functor(X:s) -> c12
4 | end
Error: Signature mismatch:
       Modules do not match:
         sig module type x = (X : s) -> c12 end
       is not included in
         sig module type x = (X : s) -> s end
       Try changing module type "x" to
       module type x = (X : s) -> s
|}]

module Nested: sig
  module type x = sig
    module A: sig
      module B: sig
        module C: functor(X:sig end)(Y:sig end)
          (Z:
           sig
             module D: sig
               module E: sig
                 module F:functor(X:sig end)
                   (Arg:sig
                      val one:int
                      val two:int
                    end) -> sig end
               end
             end
           end)
          -> sig end
      end
    end
  end
end=struct
  module type x = sig
    module A: sig
      module B: sig
        module C: functor(X:sig end)(Y:sig end)
          (Z:
           sig
             module D: sig
               module E: sig
                 module F:functor(X:sig end)
                   (Arg:sig
                      val two:int
                      val one:int
                    end) -> sig end
               end
             end
           end)
          -> sig end
      end
    end
  end
end
[%%expect {|
Lines 22-43, characters 4-3:
22 | ....struct
23 |   module type x = sig
24 |     module A: sig
25 |       module B: sig
26 |         module C: functor(X:sig end)(Y:sig end)
...
40 |       end
41 |     end
42 |   end
43 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           module type x =
             sig
               module A :
                 sig
                   module B :
                     sig
                       module C :
                         (X : sig end) (Y : sig end)
                         (Z : sig
                                module D :
                                  sig
                                    module E :
                                      sig
                                        module F :
                                          (X : sig end)
                                          (Arg : sig
                                                   val two : int
                                                   val one : int
                                                 end)
                                            -> sig end
                                      end
                                  end
                              end)
                           -> sig end
                     end
                 end
             end
         end
       is not included in
         sig
           module type x =
             sig
               module A :
                 sig
                   module B :
                     sig
                       module C :
                         (X : sig end) (Y : sig end)
                         (Z : sig
                                module D :
                                  sig
                                    module E :
                                      sig
                                        module F :
                                          (X : sig end)
                                          (Arg : sig
                                                   val one : int
                                                   val two : int
                                                 end)
                                            -> sig end
                                      end
                                  end
                              end)
                           -> sig end
                     end
                 end
             end
         end
       Try changing module type "x" to
       module type x =
         sig
           module A :
             sig
               module B :
                 sig
                   module C :
                     (X : sig end) (Y : sig end)
                     (Z : sig
                            module D :
                              sig
                                module E :
                                  sig
                                    module F :
                                      (X : sig end)
                                      (Arg : sig
                                               val one : int
                                               val two : int
                                             end)
                                        -> sig end
                                  end
                              end
                          end)
                       -> sig end
                 end
             end
         end
|}]
