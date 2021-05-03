(* TEST
  * expect
*)

module C = struct
  class c = object end
end

module R = struct
  include C
  type c
end
[%%expect {|
module C : sig class c : object  end end
module R : sig type c end
|}]


module CT = struct
  include C
  class type c = object end
end
[%%expect {|
module CT : sig class type c = object  end end
|}]


module P = struct
  type t = private < .. >
end

module M = struct
  include P
  type t = A
end
[%%expect {|
module P : sig type t = private < .. > end
module M : sig type t = A end
|}]



module Root = struct
  type u
  and t = private < .. >
end

module Trunk = struct
  include Root
  type t = A
  type u
end


module M: sig
  module type s = module type of Trunk
end = struct
  module type s = sig
    type t = A
    type u
  end
end
[%%expect {|
module Root : sig type u and t = private < .. > end
module Trunk : sig type t = A type u end
module M : sig module type s = sig type t = A type u end end
|}]
