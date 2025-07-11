(* TEST
 flags = "-keep-original-error-size";
 expect;
*)


module A = struct
  type a and b and c and d
end

module type S = sig
  module B = A
end

module C : S = struct
  module B = struct
    type a and b and c and d and e and f and g and h
  end
end
[%%expect {|
module A : sig type a and b and c and d end
module type S = sig module B = A end
Lines 9-13, characters 15-3:
 9 | ...............struct
10 |   module B = struct
11 |     type a and b and c and d and e and f and g and h
12 |   end
13 | end
Error: Signature mismatch:
       ...
       Try changing module "B" to be a
       (module A)
|}]

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
       Try changing module type "B" to
       module type B = sig module C = A end
|}]
