
module type Arg = sig
    module type A
    module type Honorificabilitudinitatibus
    module X:   Honorificabilitudinitatibus
    module Y:   A
end

module I: Arg = struct
  module type A = sig type t = T end
  module type Honorificabilitudinitatibus = sig type u = U end
  module X = struct type u = U end
  module Y = struct type t = T end
end;;

module S = struct
  open I
  module G
      (_:A)(_:A)(_:A)(_:A)
      (_:A)(_:A)(_:A)(_:A)
      (_:A)(_:A)(_:A)(_:A) =
  struct end
  module M = G(Y)(X)(Y)(X)
end;;
