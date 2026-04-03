(* TEST
 flags = "-dshape";
 expect;
*)

module M = struct end (* uid 0 *)
module F(X : sig end) = M
module App = F(List)
[%%expect{|
{
 "M"[module] -> {<.0>};
 }
module M : sig end
{
 "F"[module] -> Abs<.2>(X, {<.0>});
 }
module F : (X : sig end) -> sig end
{
 "App"[module] -> {<.3>};
 }
module App : sig end
|}, Principal{|
{
 "M"[module] -> {<.4>};
 }
module M : sig end
{
 "F"[module] -> Abs<.6>(X, {<.4>});
 }
module F : (X : sig end) -> sig end
{
 "App"[module] -> {<.7>};
 }
module App : sig end
|}, Rectypes{|
{
 "M"[module] -> {<.8>};
 }
module M : sig end
{
 "F"[module] -> Abs<.10>(X, {<.8>});
 }
module F : (X : sig end) -> sig end
{
 "App"[module] -> {<.11>};
 }
module App : sig end
|}]


module M = struct end (* uid 4 *)
module F(X : sig end) = struct include M type t end
module App = F(List)
[%%expect{|
{
 "M"[module] -> {<.12>};
 }
module M : sig end
{
 "F"[module] -> Abs<.15>(X, {
                             "t"[type] -> <.14>;
                             });
 }
module F : (X : sig end) -> sig type t end
{
 "App"[module] -> {<.16>
                   "t"[type] -> <.14>;
                   };
 }
module App : sig type t = F(List).t end
|}, Principal{|
{
 "M"[module] -> {<.17>};
 }
module M : sig end
{
 "F"[module] -> Abs<.20>(X, {
                             "t"[type] -> <.19>;
                             });
 }
module F : (X : sig end) -> sig type t end
{
 "App"[module] -> {<.21>
                   "t"[type] -> <.19>;
                   };
 }
module App : sig type t = F(List).t end
|}, Rectypes{|
{
 "M"[module] -> {<.22>};
 }
module M : sig end
{
 "F"[module] -> Abs<.25>(X, {
                             "t"[type] -> <.24>;
                             });
 }
module F : (X : sig end) -> sig type t end
{
 "App"[module] -> {<.26>
                   "t"[type] -> <.24>;
                   };
 }
module App : sig type t = F(List).t end
|}]

module M = struct end (* uid 9 *)
module F(X : sig end) = X
module App = F(M)
[%%expect{|
{
 "M"[module] -> {<.27>};
 }
module M : sig end
{
 "F"[module] -> Abs<.29>(X, X<.28>);
 }
module F : (X : sig end) -> sig end
{
 "App"[module] -> {<.30>};
 }
module App : sig end
|}, Principal{|
{
 "M"[module] -> {<.31>};
 }
module M : sig end
{
 "F"[module] -> Abs<.33>(X, X<.32>);
 }
module F : (X : sig end) -> sig end
{
 "App"[module] -> {<.34>};
 }
module App : sig end
|}, Rectypes{|
{
 "M"[module] -> {<.35>};
 }
module M : sig end
{
 "F"[module] -> Abs<.37>(X, X<.36>);
 }
module F : (X : sig end) -> sig end
{
 "App"[module] -> {<.38>};
 }
module App : sig end
|}]

module Id(X : sig end) = X
module Struct = struct
  module L = List
end
[%%expect{|
{
 "Id"[module] -> Abs<.40>(X, X<.39>);
 }
module Id : (X : sig end) -> sig end
{
 "Struct"[module] ->
   {<.42>
    "L"[module] -> Alias(<.41>
                         CU Stdlib . "List"[module]);
    };
 }
module Struct : sig module L = List end
|}, Principal{|
{
 "Id"[module] -> Abs<.44>(X, X<.43>);
 }
module Id : (X : sig end) -> sig end
{
 "Struct"[module] ->
   {<.46>
    "L"[module] -> Alias(<.45>
                         CU Stdlib . "List"[module]);
    };
 }
module Struct : sig module L = List end
|}, Rectypes{|
{
 "Id"[module] -> Abs<.48>(X, X<.47>);
 }
module Id : (X : sig end) -> sig end
{
 "Struct"[module] ->
   {<.50>
    "L"[module] -> Alias(<.49>
                         CU Stdlib . "List"[module]);
    };
 }
module Struct : sig module L = List end
|}]

module App = Id(List) (* this should have the App uid *)
module Proj = Struct.L
  (* this should have the Proj uid and be an alias to Struct.L *)
[%%expect{|
{
 "App"[module] -> (CU Stdlib . "List"[module])<.51>;
 }
module App : sig end
{
 "Proj"[module] -> Alias(<.52>
                         Alias(<.41>
                               CU Stdlib . "List"[module]));
 }
module Proj = Struct.L
|}, Principal{|
{
 "App"[module] -> (CU Stdlib . "List"[module])<.53>;
 }
module App : sig end
{
 "Proj"[module] -> Alias(<.54>
                         Alias(<.45>
                               CU Stdlib . "List"[module]));
 }
module Proj = Struct.L
|}, Rectypes{|
{
 "App"[module] -> (CU Stdlib . "List"[module])<.55>;
 }
module App : sig end
{
 "Proj"[module] -> Alias(<.56>
                         Alias(<.49>
                               CU Stdlib . "List"[module]));
 }
module Proj = Struct.L
|}]

module F (X :sig end ) = struct module M = X end
module N = F(struct end)
module O = N.M
[%%expect{|
{
 "F"[module] -> Abs<.59>(X, {
                             "M"[module] -> X<.57>;
                             });
 }
module F : (X : sig end) -> sig module M : sig end end
{
 "N"[module] -> {<.60>
                 "M"[module] -> {<.57>};
                 };
 }
module N : sig module M : sig end end
{
 "O"[module] -> Alias(<.61>
                      {<.57>});
 }
module O = N.M
|}, Principal{|
{
 "F"[module] -> Abs<.64>(X, {
                             "M"[module] -> X<.62>;
                             });
 }
module F : (X : sig end) -> sig module M : sig end end
{
 "N"[module] -> {<.65>
                 "M"[module] -> {<.62>};
                 };
 }
module N : sig module M : sig end end
{
 "O"[module] -> Alias(<.66>
                      {<.62>});
 }
module O = N.M
|}, Rectypes{|
{
 "F"[module] -> Abs<.69>(X, {
                             "M"[module] -> X<.67>;
                             });
 }
module F : (X : sig end) -> sig module M : sig end end
{
 "N"[module] -> {<.70>
                 "M"[module] -> {<.67>};
                 };
 }
module N : sig module M : sig end end
{
 "O"[module] -> Alias(<.71>
                      {<.67>});
 }
module O = N.M
|}]
