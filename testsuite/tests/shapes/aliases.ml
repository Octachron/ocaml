(* TEST
 flags = "-dshape";
 expect;
*)

module A = struct type t end
module B = A
[%%expect{|
{
 "A"[module] -> {<.1>
                 "t"[type] -> <.0>;
                 };
 }
module A : sig type t end
{
 "B"[module] -> Alias(<.2>
                      {<.1>
                       "t"[type] -> <.0>;
                       });
 }
module B = A
|}, Principal{|
{
 "A"[module] -> {<.4>
                 "t"[type] -> <.3>;
                 };
 }
module A : sig type t end
{
 "B"[module] -> Alias(<.5>
                      {<.4>
                       "t"[type] -> <.3>;
                       });
 }
module B = A
|}, Rectypes{|
{
 "A"[module] -> {<.7>
                 "t"[type] -> <.6>;
                 };
 }
module A : sig type t end
{
 "B"[module] -> Alias(<.8>
                      {<.7>
                       "t"[type] -> <.6>;
                       });
 }
module B = A
|}]

type u = B.t

[%%expect{|
{
 "u"[type] -> <.9>;
 }
type u = B.t
|}, Principal{|
{
 "u"[type] -> <.10>;
 }
type u = B.t
|}, Rectypes{|
{
 "u"[type] -> <.11>;
 }
type u = B.t
|}]

module F (X : sig type t end) = X
module F' = F
[%%expect{|
{
 "F"[module] -> Abs<.14>(X, X<.13>);
 }
module F : (X : sig type t end) -> sig type t = X.t end
{
 "F'"[module] -> Alias(<.15>
                       Abs<.14>(X, X<.13>));
 }
module F' = F
|}, Principal{|
{
 "F"[module] -> Abs<.18>(X, X<.17>);
 }
module F : (X : sig type t end) -> sig type t = X.t end
{
 "F'"[module] -> Alias(<.19>
                       Abs<.18>(X, X<.17>));
 }
module F' = F
|}, Rectypes{|
{
 "F"[module] -> Abs<.22>(X, X<.21>);
 }
module F : (X : sig type t end) -> sig type t = X.t end
{
 "F'"[module] -> Alias(<.23>
                       Abs<.22>(X, X<.21>));
 }
module F' = F
|}]

module C = F'(A)
[%%expect{|
{
 "C"[module] -> {<.24>
                 "t"[type] -> <.0>;
                 };
 }
module C : sig type t = A.t end
|}, Principal{|
{
 "C"[module] -> {<.25>
                 "t"[type] -> <.3>;
                 };
 }
module C : sig type t = A.t end
|}, Rectypes{|
{
 "C"[module] -> {<.26>
                 "t"[type] -> <.6>;
                 };
 }
module C : sig type t = A.t end
|}]


module C = F(B)

[%%expect{|
{
 "C"[module] -> Alias(<.27>
                      {<.1>
                       "t"[type] -> <.0>;
                       });
 }
module C : sig type t = B.t end
|}, Principal{|
{
 "C"[module] -> Alias(<.28>
                      {<.4>
                       "t"[type] -> <.3>;
                       });
 }
module C : sig type t = B.t end
|}, Rectypes{|
{
 "C"[module] -> Alias(<.29>
                      {<.7>
                       "t"[type] -> <.6>;
                       });
 }
module C : sig type t = B.t end
|}]

module D = C

[%%expect{|
{
 "D"[module] -> Alias(<.30>
                      Alias(<.27>
                            {<.1>
                             "t"[type] -> <.0>;
                             }));
 }
module D = C
|}, Principal{|
{
 "D"[module] -> Alias(<.31>
                      Alias(<.28>
                            {<.4>
                             "t"[type] -> <.3>;
                             }));
 }
module D = C
|}, Rectypes{|
{
 "D"[module] -> Alias(<.32>
                      Alias(<.29>
                            {<.7>
                             "t"[type] -> <.6>;
                             }));
 }
module D = C
|}]

module G (X : sig type t end) = struct include X end
[%%expect{|
{
 "G"[module] -> Abs<.35>(X, {
                             "t"[type] -> X<.34> . "t"[type];
                             });
 }
module G : (X : sig type t end) -> sig type t = X.t end
|}, Principal{|
{
 "G"[module] -> Abs<.38>(X, {
                             "t"[type] -> X<.37> . "t"[type];
                             });
 }
module G : (X : sig type t end) -> sig type t = X.t end
|}, Rectypes{|
{
 "G"[module] -> Abs<.41>(X, {
                             "t"[type] -> X<.40> . "t"[type];
                             });
 }
module G : (X : sig type t end) -> sig type t = X.t end
|}]

module E = G(B)
[%%expect{|
{
 "E"[module] -> {<.42>
                 "t"[type] -> <.0>;
                 };
 }
module E : sig type t = B.t end
|}, Principal{|
{
 "E"[module] -> {<.43>
                 "t"[type] -> <.3>;
                 };
 }
module E : sig type t = B.t end
|}, Rectypes{|
{
 "E"[module] -> {<.44>
                 "t"[type] -> <.6>;
                 };
 }
module E : sig type t = B.t end
|}]

module M = struct type t let x = 1 end
module N : sig type t end = M
module O = N
[%%expect{|
{
 "M"[module] -> {<.47>
                 "t"[type] -> <.45>;
                 "x"[value] -> <.46>;
                 };
 }
module M : sig type t val x : int end
{
 "N"[module] -> {<.49>
                 "t"[type] -> <.45>;
                 };
 }
module N : sig type t end
{
 "O"[module] -> Alias(<.50>
                      {<.49>
                       "t"[type] -> <.45>;
                       });
 }
module O = N
|}, Principal{|
{
 "M"[module] -> {<.53>
                 "t"[type] -> <.51>;
                 "x"[value] -> <.52>;
                 };
 }
module M : sig type t val x : int end
{
 "N"[module] -> {<.55>
                 "t"[type] -> <.51>;
                 };
 }
module N : sig type t end
{
 "O"[module] -> Alias(<.56>
                      {<.55>
                       "t"[type] -> <.51>;
                       });
 }
module O = N
|}, Rectypes{|
{
 "M"[module] -> {<.59>
                 "t"[type] -> <.57>;
                 "x"[value] -> <.58>;
                 };
 }
module M : sig type t val x : int end
{
 "N"[module] -> {<.61>
                 "t"[type] -> <.57>;
                 };
 }
module N : sig type t end
{
 "O"[module] -> Alias(<.62>
                      {<.61>
                       "t"[type] -> <.57>;
                       });
 }
module O = N
|}]
