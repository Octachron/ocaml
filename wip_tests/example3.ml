module F(X:sig type t end) = struct end

module M = F(struct type x end)
