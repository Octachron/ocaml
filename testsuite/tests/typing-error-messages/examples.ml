
module F: sig
        val f:int -> int -> int -> int -> int -> int -> int
end = struct 
        let f _ _ _ _ x y = x +. y
end;;

module G: sig
        val f: ?xyz:int -> int -> int -> int -> int -> int -> int
end = struct 
        let f ?xzy:_ _ _ _ x y = x + y
end;;


type c =
  < arma:int; virumque:int; cano:int; troiae:float; qui:float;
    primus:char; ab:int; oris:float; italiam:float; fato:unit; profugus:int list >
let o : c =  object
        method primus='x'
        method ab=0
        method oris=1
        method italiam=1.
        method fato = ()
        method profugus = [fun x -> x]
        method laviniaque = "u"
        method venit = ()
        method litora x = 1 + x
        method multum x = 1. +. x
        method et l = 1 :: l
        method terris = 1,0
        method jactatus _ _ = ()
        method et' = [2.]
        method alto = [[]]
end;;

type x
type ui type superum type saevae type memorem type luonis type ob type iram
type multa type quoque type et type bello type passus type dum type conderet
type urbem

module M: sig
  val f: ui -> superum -> saevae -> memorem -> luonis -> ob -> iram 
  -> multa -> quoque -> et -> bello -> passus -> dum -> conderet 
  -> urbem -> unit 
end = struct 
  let f (_:ui) (_:x) (_:saevae) (_:x) (_:luonis) (_:x) (_:iram)
  (_:x) (x:quoque) (_:x) (_:bello) (_:x) (_:dum)
  (_:x) (_:urbem) = ()
end;;



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
