(* TEST
 flags = "-dshape";
 expect;
*)

(* We depart slightly from the example in the PLDI'19 paper, which actually
   doesn't type... *)

module type Stringable = sig
  type t
  val to_string : t -> string
end
[%%expect{|
{
 "Stringable"[module type] -> <.2>;
 }
module type Stringable = sig type t val to_string : t -> string end
|}, Principal{|
{
 "Stringable"[module type] -> <.5>;
 }
module type Stringable = sig type t val to_string : t -> string end
|}, Rectypes{|
{
 "Stringable"[module type] -> <.8>;
 }
module type Stringable = sig type t val to_string : t -> string end
|}]

module Pair (X : Stringable) (Y : Stringable) = struct
  type t = X.t * Y.t
  let to_string (x, y) =
    X.to_string x ^ " " ^ Y.to_string y
end
[%%expect{|
{
 "Pair"[module] ->
   Abs<.15>(X, Y, {
                   "t"[type] -> <.11>;
                   "to_string"[value] -> <.12>;
                   });
 }
module Pair :
  (X : Stringable) (Y : Stringable) ->
    sig type t = X.t * Y.t val to_string : X.t * Y.t -> string end
|}, Principal{|
{
 "Pair"[module] ->
   Abs<.22>(X, Y, {
                   "t"[type] -> <.18>;
                   "to_string"[value] -> <.19>;
                   });
 }
module Pair :
  (X : Stringable) (Y : Stringable) ->
    sig type t = X.t * Y.t val to_string : X.t * Y.t -> string end
|}, Rectypes{|
{
 "Pair"[module] ->
   Abs<.29>(X, Y, {
                   "t"[type] -> <.25>;
                   "to_string"[value] -> <.26>;
                   });
 }
module Pair :
  (X : Stringable) (Y : Stringable) ->
    sig type t = X.t * Y.t val to_string : X.t * Y.t -> string end
|}]

module Int = struct
  type t = int
  let to_string i = string_of_int i
end
[%%expect{|
{
 "Int"[module] -> {<.33>
                   "t"[type] -> <.30>;
                   "to_string"[value] -> <.31>;
                   };
 }
module Int : sig type t = int val to_string : int -> string end
|}, Principal{|
{
 "Int"[module] -> {<.37>
                   "t"[type] -> <.34>;
                   "to_string"[value] -> <.35>;
                   };
 }
module Int : sig type t = int val to_string : int -> string end
|}, Rectypes{|
{
 "Int"[module] -> {<.41>
                   "t"[type] -> <.38>;
                   "to_string"[value] -> <.39>;
                   };
 }
module Int : sig type t = int val to_string : int -> string end
|}]

module String = struct
  type t = string
  let to_string s = s
end
[%%expect{|
{
 "String"[module] -> {<.45>
                      "t"[type] -> <.42>;
                      "to_string"[value] -> <.43>;
                      };
 }
module String : sig type t = string val to_string : 'a -> 'a end
|}, Principal{|
{
 "String"[module] -> {<.49>
                      "t"[type] -> <.46>;
                      "to_string"[value] -> <.47>;
                      };
 }
module String : sig type t = string val to_string : 'a -> 'a end
|}, Rectypes{|
{
 "String"[module] -> {<.53>
                      "t"[type] -> <.50>;
                      "to_string"[value] -> <.51>;
                      };
 }
module String : sig type t = string val to_string : 'a -> 'a end
|}]

module P = Pair(Int)(Pair(String)(Int))
[%%expect{|
{
 "P"[module] -> {<.54>
                 "t"[type] -> <.11>;
                 "to_string"[value] -> <.12>;
                 };
 }
module P :
  sig
    type t = Int.t * Pair(String)(Int).t
    val to_string : Int.t * Pair(String)(Int).t -> string
  end
|}, Principal{|
{
 "P"[module] -> {<.55>
                 "t"[type] -> <.18>;
                 "to_string"[value] -> <.19>;
                 };
 }
module P :
  sig
    type t = Int.t * Pair(String)(Int).t
    val to_string : Int.t * Pair(String)(Int).t -> string
  end
|}, Rectypes{|
{
 "P"[module] -> {<.56>
                 "t"[type] -> <.25>;
                 "to_string"[value] -> <.26>;
                 };
 }
module P :
  sig
    type t = Int.t * Pair(String)(Int).t
    val to_string : Int.t * Pair(String)(Int).t -> string
  end
|}];;

P.to_string (0, ("!=", 1))
[%%expect{|
{}
- : string = "0 != 1"
|}]
