let o = object method f=0; method g =1; method h = 2 end
let f o = o#f + o#g + o#h + o#i
;; f o;;


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
