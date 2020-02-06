module type x = sig type x end
module type y = sig type y end
module type z = sig type z end

module X: x = struct type x end
module Y: y = struct type y end
module Z: z = struct type z end

module F(X:x)(Y:y)(Z:z) = struct end

module M = F(X)(Z)
