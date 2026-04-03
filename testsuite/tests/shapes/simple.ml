(* TEST
 flags = "-dshape";
 expect;
*)

let x = ()
[%%expect{|
{
 "x"[value] -> <.0>;
 }
val x : unit = ()
|}, Principal{|
{
 "x"[value] -> <.1>;
 }
val x : unit = ()
|}, Rectypes{|
{
 "x"[value] -> <.2>;
 }
val x : unit = ()
|}]

external y : int -> int = "%identity"
[%%expect{|
{
 "y"[value] -> <.3>;
 }
external y : int -> int = "%identity"
|}, Principal{|
{
 "y"[value] -> <.4>;
 }
external y : int -> int = "%identity"
|}, Rectypes{|
{
 "y"[value] -> <.5>;
 }
external y : int -> int = "%identity"
|}]

type t = A of foo
and foo = Bar
[%%expect{|
{
 "foo"[type] -> {<.7>
                 "Bar"[constructor] -> {<.9>};
                 };
 "t"[type] -> {<.6>
               "A"[constructor] -> {<.8>};
               };
 }
type t = A of foo
and foo = Bar
|}, Principal{|
{
 "foo"[type] -> {<.11>
                 "Bar"[constructor] -> {<.13>};
                 };
 "t"[type] -> {<.10>
               "A"[constructor] -> {<.12>};
               };
 }
type t = A of foo
and foo = Bar
|}, Rectypes{|
{
 "foo"[type] -> {<.15>
                 "Bar"[constructor] -> {<.17>};
                 };
 "t"[type] -> {<.14>
               "A"[constructor] -> {<.16>};
               };
 }
type t = A of foo
and foo = Bar
|}]

module type S = sig
  type t
end
[%%expect{|
{
 "S"[module type] -> <.19>;
 }
module type S = sig type t end
|}, Principal{|
{
 "S"[module type] -> <.21>;
 }
module type S = sig type t end
|}, Rectypes{|
{
 "S"[module type] -> <.23>;
 }
module type S = sig type t end
|}]

exception E
[%%expect{|
{
 "E"[extension constructor] -> {<.24>};
 }
exception E
|}, Principal{|
{
 "E"[extension constructor] -> {<.25>};
 }
exception E
|}, Rectypes{|
{
 "E"[extension constructor] -> {<.26>};
 }
exception E
|}]

type ext = ..
[%%expect{|
{
 "ext"[type] -> <.27>;
 }
type ext = ..
|}, Principal{|
{
 "ext"[type] -> <.28>;
 }
type ext = ..
|}, Rectypes{|
{
 "ext"[type] -> <.29>;
 }
type ext = ..
|}]

type ext += A | B
[%%expect{|
{
 "A"[extension constructor] -> {<.30>};
 "B"[extension constructor] -> {<.31>};
 }
type ext += A | B
|}, Principal{|
{
 "A"[extension constructor] -> {<.32>};
 "B"[extension constructor] -> {<.33>};
 }
type ext += A | B
|}, Rectypes{|
{
 "A"[extension constructor] -> {<.34>};
 "B"[extension constructor] -> {<.35>};
 }
type ext += A | B
|}]

module M = struct
  type ext += C
end
[%%expect{|
{
 "M"[module] -> {<.37>
                 "C"[extension constructor] -> {<.36>};
                 };
 }
module M : sig type ext += C end
|}, Principal{|
{
 "M"[module] -> {<.39>
                 "C"[extension constructor] -> {<.38>};
                 };
 }
module M : sig type ext += C end
|}, Rectypes{|
{
 "M"[module] -> {<.41>
                 "C"[extension constructor] -> {<.40>};
                 };
 }
module M : sig type ext += C end
|}]

module _ = struct
  type t = Should_not_appear_in_shape
end
[%%expect{|
{}
|}]

module rec M1 : sig
  type t = C of M2.t
end = struct
  type t = C of M2.t
end

and M2 : sig
  type t
  val x : t
end = struct
  type t = T
  let x = T
end
[%%expect{|
{
 "M1"[module] -> {
                  "t"[type] -> {<.61>
                                "C"[constructor] -> {<.62>};
                                };
                  };
 "M2"[module] ->
   {
    "t"[type] -> {<.63>
                  "T"[constructor] -> {<.64>};
                  };
    "x"[value] -> <.65>;
    };
 }
module rec M1 : sig type t = C of M2.t end
and M2 : sig type t val x : t end
|}, Principal{|
{
 "M1"[module] -> {
                  "t"[type] -> {<.76>
                                "C"[constructor] -> {<.77>};
                                };
                  };
 "M2"[module] ->
   {
    "t"[type] -> {<.78>
                  "T"[constructor] -> {<.79>};
                  };
    "x"[value] -> <.80>;
    };
 }
module rec M1 : sig type t = C of M2.t end
and M2 : sig type t val x : t end
|}, Rectypes{|
{
 "M1"[module] -> {
                  "t"[type] -> {<.91>
                                "C"[constructor] -> {<.92>};
                                };
                  };
 "M2"[module] ->
   {
    "t"[type] -> {<.93>
                  "T"[constructor] -> {<.94>};
                  };
    "x"[value] -> <.95>;
    };
 }
module rec M1 : sig type t = C of M2.t end
and M2 : sig type t val x : t end
|}]

class c = object end
[%%expect{|
{
 "c"[type] -> <.96>;
 "c"[class] -> <.96>;
 "c"[class type] -> <.96>;
 }
class c : object  end
|}, Principal{|
{
 "c"[type] -> <.99>;
 "c"[class] -> <.99>;
 "c"[class type] -> <.99>;
 }
class c : object  end
|}, Rectypes{|
{
 "c"[type] -> <.102>;
 "c"[class] -> <.102>;
 "c"[class type] -> <.102>;
 }
class c : object  end
|}]

class type c = object end
[%%expect{|
{
 "c"[type] -> <.105>;
 "c"[class type] -> <.105>;
 }
class type c = object  end
|}, Principal{|
{
 "c"[type] -> <.106>;
 "c"[class type] -> <.106>;
 }
class type c = object  end
|}, Rectypes{|
{
 "c"[type] -> <.107>;
 "c"[class type] -> <.107>;
 }
class type c = object  end
|}]

type u = t
[%%expect{|
{
 "u"[type] -> <.108>;
 }
type u = t
|}, Principal{|
{
 "u"[type] -> <.109>;
 }
type u = t
|}, Rectypes{|
{
 "u"[type] -> <.110>;
 }
type u = t
|}]
