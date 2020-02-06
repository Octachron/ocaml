module F(Ctx: sig
  module type t
  module type u
  module X: t
  module Y:u
end) = struct
  open Ctx
  module F(A:t)(B:u) = struct end
  module M = F(Y)(X)
end
