(* TEST
 expect;
*)

module Fn = struct
  let id x = x
end
;;
[%%expect{|
module Fn : sig val id : 'a -> 'a end
|}]

let f = fun x -> Fn.id x
;;
[%%expect{|
val f : 'a -> 'a = <fun>
|}]

let g = Fn.(fun x -> id x)
let h = let open Fn in fun x -> id x
;;
[%%expect{|
val g : 'a -> 'a = <fun>
val h : 'a -> 'a = <fun>
|}]

let i =
  let open struct
    let id x = x
  end in
  fun x -> id x

let iM =
  let module M = struct
    let id x = x
  end in
  fun x -> M.id x
;;
[%%expect{|
val i : 'a -> 'a = <fun>
val iM : 'a -> 'a = <fun>
|}]

let j =
  let open struct
    exception E
    let id x = x
  end in
  fun x -> id x

let jM =
  let module M = struct
    exception E
    let id x = x
  end in
  fun x -> M.id x
;;
[%%expect{|
val j : '_weak1 -> '_weak1 = <fun>
val jM : '_weak2 -> '_weak2 = <fun>
|}, Principal{|
val j : '_weak3 -> '_weak3 = <fun>
val jM : '_weak4 -> '_weak4 = <fun>
|}, Rectypes{|
val j : '_weak5 -> '_weak5 = <fun>
val jM : '_weak6 -> '_weak6 = <fun>
|}]

module Square(X : sig val x : int end) = struct
  let result = X.x * X.x
end
;;
[%%expect{|
module Square : (X : sig val x : int end) -> sig val result : int end
|}]

let k =
  let open Square(struct let x = 3 end) in
  fun x -> x

let kM =
  let module M = Square(struct let x = 3 end) in
  fun x -> x
;;
[%%expect{|
val k : '_weak7 -> '_weak7 = <fun>
val kM : '_weak8 -> '_weak8 = <fun>
|}, Principal{|
val k : '_weak9 -> '_weak9 = <fun>
val kM : '_weak10 -> '_weak10 = <fun>
|}, Rectypes{|
val k : '_weak11 -> '_weak11 = <fun>
val kM : '_weak12 -> '_weak12 = <fun>
|}]

let op =
  let module M = struct
      open struct let r = ref [] end
      let s = r
  end in
  M.s
;;
[%%expect{|
val op : '_weak13 list ref = {contents = []}
|}, Principal{|
val op : '_weak14 list ref = {contents = []}
|}, Rectypes{|
val op : '_weak15 list ref = {contents = []}
|}]
