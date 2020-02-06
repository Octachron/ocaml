(* TEST
   * expect
*)

module M: sig
  module type S = sig type t end
end = struct
  module type S = sig type s type t end
end;;
[%%expect {||}]

module M: sig
  module type S = sig type t type u end
end = struct
  module type S = sig type t end
end;;
  [%%expect {||}]
