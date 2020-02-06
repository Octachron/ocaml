module F(X:sig type x end)(Y:sig type y end)(Z:sig type z end) = struct
    type t = X of X.x | Y of Y.y | Z of Z.z
end

module X = struct type x end
module Z = struct type z end

type u = F(X)(Z).t
