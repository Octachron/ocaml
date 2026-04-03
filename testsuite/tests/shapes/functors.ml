(* TEST
 flags = "-dshape";
 expect;
*)

module type S = sig
  type t
  val x : t
end
[%%expect{|
{
 "S"[module type] -> <.2>;
 }
module type S = sig type t val x : t end
|}, Principal{|
{
 "S"[module type] -> <.5>;
 }
module type S = sig type t val x : t end
|}, Rectypes{|
{
 "S"[module type] -> <.8>;
 }
module type S = sig type t val x : t end
|}]

module Falias (X : S) = X
[%%expect{|
{
 "Falias"[module] -> Abs<.10>(X, X<.9>);
 }
module Falias : (X : S) -> sig type t = X.t val x : t end
|}, Principal{|
{
 "Falias"[module] -> Abs<.12>(X, X<.11>);
 }
module Falias : (X : S) -> sig type t = X.t val x : t end
|}, Rectypes{|
{
 "Falias"[module] -> Abs<.14>(X, X<.13>);
 }
module Falias : (X : S) -> sig type t = X.t val x : t end
|}]

module Finclude (X : S) = struct
  include X
end
[%%expect{|
{
 "Finclude"[module] ->
   Abs<.16>
      (X,
       {
        "t"[type] -> X<.15> . "t"[type];
        "x"[value] -> X<.15> . "x"[value];
        });
 }
module Finclude : (X : S) -> sig type t = X.t val x : t end
|}, Principal{|
{
 "Finclude"[module] ->
   Abs<.18>
      (X,
       {
        "t"[type] -> X<.17> . "t"[type];
        "x"[value] -> X<.17> . "x"[value];
        });
 }
module Finclude : (X : S) -> sig type t = X.t val x : t end
|}, Rectypes{|
{
 "Finclude"[module] ->
   Abs<.20>
      (X,
       {
        "t"[type] -> X<.19> . "t"[type];
        "x"[value] -> X<.19> . "x"[value];
        });
 }
module Finclude : (X : S) -> sig type t = X.t val x : t end
|}]

module Fredef (X : S) = struct
  type t = X.t
  let x = X.x
end
[%%expect{|
{
 "Fredef"[module] -> Abs<.24>(X, {
                                  "t"[type] -> <.22>;
                                  "x"[value] -> <.23>;
                                  });
 }
module Fredef : (X : S) -> sig type t = X.t val x : X.t end
|}, Principal{|
{
 "Fredef"[module] -> Abs<.28>(X, {
                                  "t"[type] -> <.26>;
                                  "x"[value] -> <.27>;
                                  });
 }
module Fredef : (X : S) -> sig type t = X.t val x : X.t end
|}, Rectypes{|
{
 "Fredef"[module] -> Abs<.32>(X, {
                                  "t"[type] -> <.30>;
                                  "x"[value] -> <.31>;
                                  });
 }
module Fredef : (X : S) -> sig type t = X.t val x : X.t end
|}]

module Fignore (_ : S) = struct
  type t = Fresh
  let x = Fresh
end
[%%expect{|
{
 "Fignore"[module] ->
   Abs<.36>
      ((),
       {
        "t"[type] -> {<.33>
                      "Fresh"[constructor] -> {<.34>};
                      };
        "x"[value] -> <.35>;
        });
 }
module Fignore : S -> sig type t = Fresh val x : t end
|}, Principal{|
{
 "Fignore"[module] ->
   Abs<.40>
      ((),
       {
        "t"[type] -> {<.37>
                      "Fresh"[constructor] -> {<.38>};
                      };
        "x"[value] -> <.39>;
        });
 }
module Fignore : S -> sig type t = Fresh val x : t end
|}, Rectypes{|
{
 "Fignore"[module] ->
   Abs<.44>
      ((),
       {
        "t"[type] -> {<.41>
                      "Fresh"[constructor] -> {<.42>};
                      };
        "x"[value] -> <.43>;
        });
 }
module Fignore : S -> sig type t = Fresh val x : t end
|}]

module Arg : S = struct
  type t = T
  let x = T
end
[%%expect{|
{
 "Arg"[module] ->
   {<.48>
    "t"[type] -> {<.45>
                  "T"[constructor] -> {<.46>};
                  };
    "x"[value] -> <.47>;
    };
 }
module Arg : S
|}, Principal{|
{
 "Arg"[module] ->
   {<.52>
    "t"[type] -> {<.49>
                  "T"[constructor] -> {<.50>};
                  };
    "x"[value] -> <.51>;
    };
 }
module Arg : S
|}, Rectypes{|
{
 "Arg"[module] ->
   {<.56>
    "t"[type] -> {<.53>
                  "T"[constructor] -> {<.54>};
                  };
    "x"[value] -> <.55>;
    };
 }
module Arg : S
|}]

include Falias(Arg)
[%%expect{|
{
 "t"[type] -> {<.45>
               "T"[constructor] -> {<.46>};
               };
 "x"[value] -> <.47>;
 }
type t = Arg.t
val x : t = <abstr>
|}, Principal{|
{
 "t"[type] -> {<.49>
               "T"[constructor] -> {<.50>};
               };
 "x"[value] -> <.51>;
 }
type t = Arg.t
val x : t = <abstr>
|}, Rectypes{|
{
 "t"[type] -> {<.53>
               "T"[constructor] -> {<.54>};
               };
 "x"[value] -> <.55>;
 }
type t = Arg.t
val x : t = <abstr>
|}]

include Finclude(Arg)
[%%expect{|
{
 "t"[type] -> {<.45>
               "T"[constructor] -> {<.46>};
               };
 "x"[value] -> <.47>;
 }
type t = Arg.t
val x : t = <abstr>
|}, Principal{|
{
 "t"[type] -> {<.49>
               "T"[constructor] -> {<.50>};
               };
 "x"[value] -> <.51>;
 }
type t = Arg.t
val x : t = <abstr>
|}, Rectypes{|
{
 "t"[type] -> {<.53>
               "T"[constructor] -> {<.54>};
               };
 "x"[value] -> <.55>;
 }
type t = Arg.t
val x : t = <abstr>
|}]

include Fredef(Arg)
[%%expect{|
{
 "t"[type] -> <.22>;
 "x"[value] -> <.23>;
 }
type t = Arg.t
val x : Arg.t = <abstr>
|}, Principal{|
{
 "t"[type] -> <.26>;
 "x"[value] -> <.27>;
 }
type t = Arg.t
val x : Arg.t = <abstr>
|}, Rectypes{|
{
 "t"[type] -> <.30>;
 "x"[value] -> <.31>;
 }
type t = Arg.t
val x : Arg.t = <abstr>
|}]

include Fignore(Arg)
[%%expect{|
{
 "t"[type] -> {<.33>
               "Fresh"[constructor] -> {<.34>};
               };
 "x"[value] -> <.35>;
 }
type t = Fignore(Arg).t = Fresh
val x : t = Fresh
|}, Principal{|
{
 "t"[type] -> {<.37>
               "Fresh"[constructor] -> {<.38>};
               };
 "x"[value] -> <.39>;
 }
type t = Fignore(Arg).t = Fresh
val x : t = Fresh
|}, Rectypes{|
{
 "t"[type] -> {<.41>
               "Fresh"[constructor] -> {<.42>};
               };
 "x"[value] -> <.43>;
 }
type t = Fignore(Arg).t = Fresh
val x : t = Fresh
|}]

include Falias(struct type t = int let x = 0 end)
[%%expect{|
{
 "t"[type] -> <.57>;
 "x"[value] -> <.58>;
 }
type t = int
val x : t = 0
|}, Principal{|
{
 "t"[type] -> <.59>;
 "x"[value] -> <.60>;
 }
type t = int
val x : t = 0
|}, Rectypes{|
{
 "t"[type] -> <.61>;
 "x"[value] -> <.62>;
 }
type t = int
val x : t = 0
|}]

include Finclude(struct type t = int let x = 0 end)
[%%expect{|
{
 "t"[type] -> <.63>;
 "x"[value] -> <.64>;
 }
type t = int
val x : t = 0
|}, Principal{|
{
 "t"[type] -> <.65>;
 "x"[value] -> <.66>;
 }
type t = int
val x : t = 0
|}, Rectypes{|
{
 "t"[type] -> <.67>;
 "x"[value] -> <.68>;
 }
type t = int
val x : t = 0
|}]

include Fredef(struct type t = int let x = 0 end)
[%%expect{|
{
 "t"[type] -> <.22>;
 "x"[value] -> <.23>;
 }
type t = int
val x : int = 0
|}, Principal{|
{
 "t"[type] -> <.26>;
 "x"[value] -> <.27>;
 }
type t = int
val x : int = 0
|}, Rectypes{|
{
 "t"[type] -> <.30>;
 "x"[value] -> <.31>;
 }
type t = int
val x : int = 0
|}]

include Fignore(struct type t = int let x = 0 end)
[%%expect{|
{
 "t"[type] -> {<.33>
               "Fresh"[constructor] -> {<.34>};
               };
 "x"[value] -> <.35>;
 }
type t = Fresh
val x : t = Fresh
|}, Principal{|
{
 "t"[type] -> {<.37>
               "Fresh"[constructor] -> {<.38>};
               };
 "x"[value] -> <.39>;
 }
type t = Fresh
val x : t = Fresh
|}, Rectypes{|
{
 "t"[type] -> {<.41>
               "Fresh"[constructor] -> {<.42>};
               };
 "x"[value] -> <.43>;
 }
type t = Fresh
val x : t = Fresh
|}]

module Fgen () = struct
  type t = Fresher
  let x = Fresher
end
[%%expect{|
{
 "Fgen"[module] ->
   Abs<.84>
      ((),
       {
        "t"[type] -> {<.81>
                      "Fresher"[constructor] -> {<.82>};
                      };
        "x"[value] -> <.83>;
        });
 }
module Fgen : () -> sig type t = Fresher val x : t end
|}, Principal{|
{
 "Fgen"[module] ->
   Abs<.88>
      ((),
       {
        "t"[type] -> {<.85>
                      "Fresher"[constructor] -> {<.86>};
                      };
        "x"[value] -> <.87>;
        });
 }
module Fgen : () -> sig type t = Fresher val x : t end
|}, Rectypes{|
{
 "Fgen"[module] ->
   Abs<.92>
      ((),
       {
        "t"[type] -> {<.89>
                      "Fresher"[constructor] -> {<.90>};
                      };
        "x"[value] -> <.91>;
        });
 }
module Fgen : () -> sig type t = Fresher val x : t end
|}]

include Fgen ()
[%%expect{|
{
 "t"[type] -> {<.81>
               "Fresher"[constructor] -> {<.82>};
               };
 "x"[value] -> <.83>;
 }
type t = Fresher
val x : t = Fresher
|}, Principal{|
{
 "t"[type] -> {<.85>
               "Fresher"[constructor] -> {<.86>};
               };
 "x"[value] -> <.87>;
 }
type t = Fresher
val x : t = Fresher
|}, Rectypes{|
{
 "t"[type] -> {<.89>
               "Fresher"[constructor] -> {<.90>};
               };
 "x"[value] -> <.91>;
 }
type t = Fresher
val x : t = Fresher
|}]

(***************************************************************************)
(* Make sure we restrict shapes even when constraints imply [Tcoerce_none] *)
(***************************************************************************)

module type Small = sig
  type t
end
[%%expect{|
{
 "Small"[module type] -> <.94>;
 }
module type Small = sig type t end
|}, Principal{|
{
 "Small"[module type] -> <.96>;
 }
module type Small = sig type t end
|}, Rectypes{|
{
 "Small"[module type] -> <.98>;
 }
module type Small = sig type t end
|}]

module type Big = sig
  type t
  type u
end
[%%expect{|
{
 "Big"[module type] -> <.101>;
 }
module type Big = sig type t type u end
|}, Principal{|
{
 "Big"[module type] -> <.104>;
 }
module type Big = sig type t type u end
|}, Rectypes{|
{
 "Big"[module type] -> <.107>;
 }
module type Big = sig type t type u end
|}]

module type B2S = functor (X : Big) -> Small with type t = X.t
[%%expect{|
{
 "B2S"[module type] -> <.110>;
 }
module type B2S = (X : Big) -> sig type t = X.t end
|}, Principal{|
{
 "B2S"[module type] -> <.113>;
 }
module type B2S = (X : Big) -> sig type t = X.t end
|}, Rectypes{|
{
 "B2S"[module type] -> <.116>;
 }
module type B2S = (X : Big) -> sig type t = X.t end
|}]

module Big_to_small1 : B2S = functor (X : Big) -> X
[%%expect{|
{
 "Big_to_small1"[module] ->
   Abs<.118>(X, {<.117>
                 "t"[type] -> X<.117> . "t"[type];
                 });
 }
module Big_to_small1 : B2S
|}, Principal{|
{
 "Big_to_small1"[module] ->
   Abs<.120>(X, {<.119>
                 "t"[type] -> X<.119> . "t"[type];
                 });
 }
module Big_to_small1 : B2S
|}, Rectypes{|
{
 "Big_to_small1"[module] ->
   Abs<.122>(X, {<.121>
                 "t"[type] -> X<.121> . "t"[type];
                 });
 }
module Big_to_small1 : B2S
|}]

module Big_to_small2 : B2S = functor (X : Big) -> struct include X end
[%%expect{|
{
 "Big_to_small2"[module] ->
   Abs<.124>(X, {
                 "t"[type] -> X<.123> . "t"[type];
                 });
 }
module Big_to_small2 : B2S
|}, Principal{|
{
 "Big_to_small2"[module] ->
   Abs<.126>(X, {
                 "t"[type] -> X<.125> . "t"[type];
                 });
 }
module Big_to_small2 : B2S
|}, Rectypes{|
{
 "Big_to_small2"[module] ->
   Abs<.128>(X, {
                 "t"[type] -> X<.127> . "t"[type];
                 });
 }
module Big_to_small2 : B2S
|}]
