let explicit (type a) (x: (a,int) optional): unit -> a =
  match x with
  | Default -> let x = 0 in fun () -> x
  | Specific x -> fun () -> x;;

let f (type a) ?( x = 0 : a = int ) (): a = x;;

let k = 1 + f ();;
let h = f ~x:"Hello" () ^" world";;
let m = f ?x:Default () + 2;;

let filter (type a b) ?map:?(a->b=a->a) (pred: b -> bool): a list -> a list =
  match map with
  | Default -> List.filter pred
  | Specific f -> List.filter (fun x -> pred (f x))

let l = filter ~map:float ( fun x -> x > 0. ) [2;5;-5;9;-3]
let l' = filter (fun x -> x < 0 ) [-5;1;8;-3];;

let error = f () ();;

[%%expect{|
val explicit : ('a, int) optional -> unit -> 'a = <fun>
val f : ?x:?('a=int) -> unit -> 'a = <fun>
val k : int = 1
val h : string = "Hello world"
val m : int = 2
val filter : ?map:?('a -> 'b='a -> 'a) -> ('b -> bool) -> 'a list -> 'a list =
  <fun>
val l : int list = [2; 5; 9]
val l' : int list = [-5; -3]
Line _:
Error: Type ('a -> 'b, 'a -> 'b) optional is not a subtype of
         ('a -> 'b, int) optional
       Type 'a -> 'b is not compatible with type int
|}]
