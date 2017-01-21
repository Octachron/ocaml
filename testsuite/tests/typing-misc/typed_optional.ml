let explicit (type a) (x: (a,int) typed_option): unit -> a =
  match x with
  | None' -> let x = 0 in fun () -> x
  | Some' x -> fun () -> x;;

let f (type a) ?( x = 0 : a = int ) (): a = x;;

let k = 1 + f ();;
let h = f ~x:"Hello" () ^" world";;

let filter (type a b) ?map:?(a->b=a->a) (pred: b -> bool): a list -> a list =
  match map with
  | None' -> List.filter pred
  | Some' f -> List.filter (fun x -> pred (f x))

let l = filter ~map:float ( fun x -> x > 0. ) [2;5;-5;9;-3]
let l' = filter (fun x -> x < 0 ) [-5;1;8;-3];;

let error = f () ();;

[%%expect{|
val explicit : ('a, int) typed_option -> unit -> 'a = <fun>
val f : ?x:?('a=int) -> unit -> 'a = <fun>
val k : int = 1
val h : string = "Hello world"
val filter : ?map:?('a -> 'b='a -> 'a) -> ('b -> bool) -> 'a list -> 'a list =
  <fun>
val l : int list = [2; 5; 9]
val l' : int list = [-5; -3]
Line _:
Error: Type ('a -> 'b, 'a -> 'b) typed_option is not a subtype of
         ('a -> 'b, int) typed_option
       Type 'a -> 'b is not compatible with type int
|}]
