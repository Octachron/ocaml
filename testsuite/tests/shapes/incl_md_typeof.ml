(* TEST
 flags = "-dshape";
 expect;
*)

module Foo : sig
  module Bar : sig
  end
end = struct
  module Bar = struct
  end
end
;;
[%%expect{|
{
 "Foo"[module] -> {<.2>
                   "Bar"[module] -> {<.0>};
                   };
 }
module Foo : sig module Bar : sig end end
|}, Principal{|
{
 "Foo"[module] -> {<.5>
                   "Bar"[module] -> {<.3>};
                   };
 }
module Foo : sig module Bar : sig end end
|}, Rectypes{|
{
 "Foo"[module] -> {<.8>
                   "Bar"[module] -> {<.6>};
                   };
 }
module Foo : sig module Bar : sig end end
|}]

module type Extended = sig
  include module type of struct include Foo end
  module Bar : sig
    include module type of struct include Bar end
  end
end
;;
[%%expect{|
{
 "Extended"[module type] -> <.10>;
 }
module type Extended = sig module Bar : sig end end
|}, Principal{|
{
 "Extended"[module type] -> <.12>;
 }
module type Extended = sig module Bar : sig end end
|}, Rectypes{|
{
 "Extended"[module type] -> <.14>;
 }
module type Extended = sig module Bar : sig end end
|}]

module E : Extended = struct
  module Bar = struct end
end

[%%expect{|
{
 "E"[module] -> {<.16>
                 "Bar"[module] -> {<.15>};
                 };
 }
module E : Extended
|}, Principal{|
{
 "E"[module] -> {<.18>
                 "Bar"[module] -> {<.17>};
                 };
 }
module E : Extended
|}, Rectypes{|
{
 "E"[module] -> {<.20>
                 "Bar"[module] -> {<.19>};
                 };
 }
module E : Extended
|}]
