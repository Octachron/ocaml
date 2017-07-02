
module N: sig
        type c = A | B | C | D | E | F | G
end = struct
        type c = A | B | C | G | H | I | J
end


module N: sig
 type inferretque = A type deos = B type latio = C type genus = D
 type unde = E type latinum = F type albanique = G type patres = H
 type atque = I type altae = J type moenia = K
end = struct
 type inferretque = A type deos = X type latio = C type genus = X
 type unde = E type latinum = F type albanique = X type patres = X
 type atque = I type altae = J type moenia = X
end;;

module M: sig
  type t = A | B | C | D | E | F end = struct
  type t = A | B | C | D | E | G end;;

module M: sig
type t = ..
type t += A | B | A2
type t += C0 | C | C2 | D
type t += E | F
end = struct
type t = ..
type t += A | A2 | B | C0 | C | C2 | E | F
end;;
