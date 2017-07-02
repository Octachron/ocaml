
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


module M:
        sig
                val f: lbl:unit -> ?opt:unit -> unit -> unit
        end=
        struct
                let f ~lbll ?optt () = ()
        end
;;

module M:
        sig
                val f: unit -> unit -> unit -> unit -> unit -> unit
        end=
        struct
                let f x y z w t= x + y + z + w + t
        end
;;

let f: int -> int -> int -> int -> int -> int = 0

module M: sig
  val odd:  int -> unit -> int -> unit -> int -> unit
  val even: unit -> int -> unit -> int -> unit -> int
  val t_3: unit -> unit -> int -> unit -> unit -> int -> unit -> unit -> int
  val t_2: unit -> int -> unit -> unit -> int -> unit -> unit -> int -> unit
  val t_1: int -> unit -> unit -> int -> unit -> unit -> int -> unit -> unit
end = struct

  let odd a b c d e = a + b + c + d + e

  let t_3 a b c d e f g h = a + b + c + d + e + f + g + h

  let t_1 a b c d e f g h = a + b + c + d + e + f + g + h

end;;
