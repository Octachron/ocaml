(* TEST
   flags ="-keep-original-error-size"
   * expect
  *)

module A = struct
  type a and b and c and d
end

module type S = sig
  module type B = sig
    module C = A
  end
end

module D : S = struct
  module type B = sig
    module C: sig
      type a and b and c and d and e and f and g and h
    end
  end
end
[%%expect{|
module A : sig type a and b and c and d end
module type S = sig module type B = sig module C = A end end
Lines 11-17, characters 15-3:
11 | ...............struct
12 |   module type B = sig
13 |     module C: sig
14 |       type a and b and c and d and e and f and g and h
15 |     end
16 |   end
17 | end
Error: Signature mismatch:
       ...
       At position module type B = sig module C : <here> end
       Modules do not match:
         sig
           type a = C.a
           and b = C.b
           and c = C.c
           and d = C.d
           and e = C.e
           and f = C.f
           and g = C.g
           and h = C.h
         end
       is not included in
         (module A)
|}]


(** Deeply nested errors *)


module M: sig
  module F: functor
      (X:
         functor(A: sig type xa end)(B:sig type xz end) -> sig end
      )
      (Y:
         functor(A: sig type ya end)(B:sig type yb end) -> sig end
      )
      (Z:
         functor(A: sig type za end)(B:sig type zb end) -> sig end
      ) -> sig end
end = struct
  module F
      (X:
         functor (A: sig type xa end)(B:sig type xz end) -> sig end
      )
      (Y:
         functor (A: sig type ya end)(B:sig type ybb end) -> sig end
      )
      (Z:
         functor (A: sig type za end)(B:sig type zbb end) -> sig end
      )
  = struct end
end
[%%expect {|
Lines 15-27, characters 6-3:
15 | ......struct
16 |   module F
17 |       (X:
18 |          functor (A: sig type xa end)(B:sig type xz end) -> sig end
19 |       )
...
24 |          functor (A: sig type za end)(B:sig type zbb end) -> sig end
25 |       )
26 |   = struct end
27 | end
Error: Signature mismatch:
       ...
       In module F:
       Modules do not match:
         functor (X : $S1) (Y : $S2) (Z : $S3) -> ...
       is not included in
         functor (X : $T1) (Y : $T2) (Z : $T3) -> ...
       1. Module types $S1 and $T1 match
       2. Module types do not match:
            $S2 =
            functor (A : sig type ya end) (B : sig type ybb end) -> sig end
          does not include
            $T2 =
            functor (A : sig type ya end) (B : sig type yb end) -> sig end
          Modules do not match:
            functor (A : $S1) (B : $S2) -> ...
          is not included in
            functor (A : $T1) (B : $T2) -> ...
          1. Module types $S1 and $T1 match
          2. Module types do not match:
               $S2 = sig type yb end
             does not include
               $T2 = sig type ybb end
             The type `yb' is required but not provided
       3. Module types do not match:
            $S3 =
            functor (A : sig type za end) (B : sig type zbb end) -> sig end
          does not include
            $T3 =
            functor (A : sig type za end) (B : sig type zb end) -> sig end
          Modules do not match:
            functor (A : $S1) (B : $S2) -> ...
          is not included in
            functor (A : $T1) (B : $T2) -> ...
|}]


module M: sig
  module F: functor
      (X:
         functor(A: sig type xa end)(B:sig type xz end) -> sig end
      )
      (Y:
         functor(A: sig type ya end)(B:sig type yb end) -> sig end
      )
      (Z:
         functor(A: sig type za end)(B:sig type zb end) -> sig end
      ) -> sig end
end = struct
  module F
      (X:
         functor (A: sig type xa end)(B:sig type xz end) -> sig end
      )
      (Y:
         functor (A: sig type ya end)(B:sig type yb end) -> sig end
      )
  = struct end
end
[%%expect {|
Lines 12-21, characters 6-3:
12 | ......struct
13 |   module F
14 |       (X:
15 |          functor (A: sig type xa end)(B:sig type xz end) -> sig end
16 |       )
17 |       (Y:
18 |          functor (A: sig type ya end)(B:sig type yb end) -> sig end
19 |       )
20 |   = struct end
21 | end
Error: Signature mismatch:
       ...
       In module F:
       Modules do not match:
         functor (X : $S1) (Y : $S2) -> ...
       is not included in
         functor (X : $T1) (Y : $T2) (Z : $T3) -> ...
       1. Module types $S1 and $T1 match
       2. Module types $S2 and $T2 match
       3. An argument appears to be missing with module type
              $T3 =
              functor (A : sig type za end) (B : sig type zb end) -> sig end
|}]

module M: sig
  module F: functor
      (X:
         functor(A: sig type xa end)(B:sig type xz end) -> sig end
      )
      (Y:
         functor(A: sig type ya end)(B:sig type yb end) -> sig end
      )
      (Z:
         functor(A: sig type za end)(B:sig type zb end) -> sig end
      ) -> sig end
end = struct
  module F
      (X:
         functor (A: sig type xaa end)(B:sig type xz end) -> sig end
      )
      (Y:
         functor (A: sig type ya end)(B:sig type ybb end) -> sig end
      )
      (Z:
         functor (A: sig type za end)(B:sig type zbb end) -> sig end
      )
  = struct end
end
[%%expect {|
Lines 12-24, characters 6-3:
12 | ......struct
13 |   module F
14 |       (X:
15 |          functor (A: sig type xaa end)(B:sig type xz end) -> sig end
16 |       )
...
21 |          functor (A: sig type za end)(B:sig type zbb end) -> sig end
22 |       )
23 |   = struct end
24 | end
Error: Signature mismatch:
       ...
       In module F:
       Modules do not match:
         functor (X : $S1) (Y : $S2) (Z : $S3) -> ...
       is not included in
         functor (X : $T1) (Y : $T2) (Z : $T3) -> ...
       1. Module types do not match:
            $S1 =
            functor (A : sig type xaa end) (B : sig type xz end) -> sig end
          does not include
            $T1 =
            functor (A : sig type xa end) (B : sig type xz end) -> sig end
          Modules do not match:
            functor (A : $S1) (B : $S2) -> ...
          is not included in
            functor (A : $T1) (B : $T2) -> ...
          1. Module types do not match:
               $S1 = sig type xa end
             does not include
               $T1 = sig type xaa end
             The type `xa' is required but not provided
          2. Module types $S2 and $T2 match
       2. Module types do not match:
            $S2 =
            functor (A : sig type ya end) (B : sig type ybb end) -> sig end
          does not include
            $T2 =
            functor (A : sig type ya end) (B : sig type yb end) -> sig end
          Modules do not match:
            functor (A : $S1) (B : $S2) -> ...
          is not included in
            functor (A : $T1) (B : $T2) -> ...
       3. Module types do not match:
            $S3 =
            functor (A : sig type za end) (B : sig type zbb end) -> sig end
          does not include
            $T3 =
            functor (A : sig type za end) (B : sig type zb end) -> sig end
          Modules do not match:
            functor (A : $S1) (B : $S2) -> ...
          is not included in
            functor (A : $T1) (B : $T2) -> ...
|}]

module A: sig
  module B: sig
    module C: sig
      module D: sig
        module E: sig
          module F: sig type x end -> sig type y end
          -> sig type z end -> sig type w end -> sig end
        end
      end
    end
  end
end = struct
  module B = struct
    module C = struct
      module D = struct
        module E = struct
          module F(X:sig type x end)(Y:sig type y' end)
            (W:sig type w end) = struct end
        end
      end
    end
  end
end
[%%expect {|
Lines 12-23, characters 6-3:
12 | ......struct
13 |   module B = struct
14 |     module C = struct
15 |       module D = struct
16 |         module E = struct
...
20 |       end
21 |     end
22 |   end
23 | end
Error: Signature mismatch:
       ...
       In module B:
       Modules do not match:
         sig module C = B.C end
       is not included in
         sig
           module C :
             sig
               module D :
                 sig
                   module E :
                     sig
                       module F :
                         sig type x end -> sig type y end ->
                           sig type z end -> sig type w end -> sig end
                     end
                 end
             end
         end
       In module B.C:
       Modules do not match:
         sig module D = B.C.D end
       is not included in
         sig
           module D :
             sig
               module E :
                 sig
                   module F :
                     sig type x end -> sig type y end -> sig type z end ->
                       sig type w end -> sig end
                 end
             end
         end
       In module B.C.D:
       Modules do not match:
         sig module E = B.C.D.E end
       is not included in
         sig
           module E :
             sig
               module F :
                 sig type x end -> sig type y end -> sig type z end ->
                   sig type w end -> sig end
             end
         end
       In module B.C.D.E:
       Modules do not match:
         sig module F = B.C.D.E.F end
       is not included in
         sig
           module F :
             sig type x end -> sig type y end -> sig type z end ->
               sig type w end -> sig end
         end
       In module B.C.D.E.F:
       Modules do not match:
         functor (X : $S1) (Y : $S3) (W : $S4) -> ...
       is not included in
         functor $T1 $T2 $T3 $T4 -> ...
       1. Module types $S1 and $T1 match
       2. An argument appears to be missing with module type
              $T2 = sig type y end
       3. Module types do not match:
            $S3 = sig type y' end
          does not include
            $T3 = sig type z end
       4. Module types $S4 and $T4 match
|}]
