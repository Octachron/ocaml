module type Arg = sig
    module type A
    module type Honorificabilitudinitatibus
    module X:   Honorificabilitudinitatibus
    module Y:   A
end

module F(A:Arg)
= struct
  open A
  module G(X:A)(Y:A)(_:A)(Z:A) = struct end
  type u = G(X)(Y)(X)(Y)(X).t
end;;
