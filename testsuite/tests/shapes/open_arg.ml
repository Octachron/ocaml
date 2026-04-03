(* TEST
 flags = "-dshape";
 expect;
*)

module type Make = functor (I : sig end) -> sig
  open I
end
;;

[%%expect{|
{
 "Make"[module type] -> <.1>;
 }
module type Make = (I : sig end) -> sig end
|}, Principal{|
{
 "Make"[module type] -> <.3>;
 }
module type Make = (I : sig end) -> sig end
|}, Rectypes{|
{
 "Make"[module type] -> <.5>;
 }
module type Make = (I : sig end) -> sig end
|}]

module Make (I : sig end) : sig
  open I
end = struct end
;;

[%%expect{|
{
 "Make"[module] -> Abs<.7>(I, {});
 }
module Make : (I : sig end) -> sig end
|}, Principal{|
{
 "Make"[module] -> Abs<.9>(I, {});
 }
module Make : (I : sig end) -> sig end
|}, Rectypes{|
{
 "Make"[module] -> Abs<.11>(I, {});
 }
module Make : (I : sig end) -> sig end
|}]

module type Make = functor (I : sig end) ->
module type of struct
  open I
end

[%%expect{|
{
 "Make"[module type] -> <.13>;
 }
module type Make = (I : sig end) -> sig end
|}, Principal{|
{
 "Make"[module type] -> <.15>;
 }
module type Make = (I : sig end) -> sig end
|}, Rectypes{|
{
 "Make"[module type] -> <.17>;
 }
module type Make = (I : sig end) -> sig end
|}]
