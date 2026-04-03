(* TEST
 flags = "-dshape";
 expect;
*)

(* Everything that couldn't go anywhere else. *)

open struct
  module M = struct
    type t = A
  end
end
[%%expect{|
{}
module M : sig type t = A end
|}]

include M
[%%expect{|
{
 "t"[type] -> {<.0>
               "A"[constructor] -> {<.1>};
               };
 }
type t = M.t = A
|}, Principal{|
{
 "t"[type] -> {<.3>
               "A"[constructor] -> {<.4>};
               };
 }
type t = M.t = A
|}, Rectypes{|
{
 "t"[type] -> {<.6>
               "A"[constructor] -> {<.7>};
               };
 }
type t = M.t = A
|}]

module N = M
[%%expect{|
{
 "N"[module] ->
   Alias(<.9>
         {<.2>
          "t"[type] -> {<.0>
                        "A"[constructor] -> {<.1>};
                        };
          });
 }
module N = M
|}, Principal{|
{
 "N"[module] ->
   Alias(<.10>
         {<.5>
          "t"[type] -> {<.3>
                        "A"[constructor] -> {<.4>};
                        };
          });
 }
module N = M
|}, Rectypes{|
{
 "N"[module] ->
   Alias(<.11>
         {<.8>
          "t"[type] -> {<.6>
                        "A"[constructor] -> {<.7>};
                        };
          });
 }
module N = M
|}]

(* Not open structs, but the code handling the following is currently very
   similar to the one for open struct (i.e. calls [Env.enter_signature]), and
   so we are likely to encounter the same bugs, if any. *)

include struct
  module M' = struct
    type t = A
  end
end
[%%expect{|
{
 "M'"[module] -> {<.14>
                  "t"[type] -> {<.12>
                                "A"[constructor] -> {<.13>};
                                };
                  };
 }
module M' : sig type t = A end
|}, Principal{|
{
 "M'"[module] -> {<.17>
                  "t"[type] -> {<.15>
                                "A"[constructor] -> {<.16>};
                                };
                  };
 }
module M' : sig type t = A end
|}, Rectypes{|
{
 "M'"[module] -> {<.20>
                  "t"[type] -> {<.18>
                                "A"[constructor] -> {<.19>};
                                };
                  };
 }
module M' : sig type t = A end
|}]

module N' = M'
[%%expect{|
{
 "N'"[module] ->
   Alias(<.21>
         {<.14>
          "t"[type] -> {<.12>
                        "A"[constructor] -> {<.13>};
                        };
          });
 }
module N' = M'
|}, Principal{|
{
 "N'"[module] ->
   Alias(<.22>
         {<.17>
          "t"[type] -> {<.15>
                        "A"[constructor] -> {<.16>};
                        };
          });
 }
module N' = M'
|}, Rectypes{|
{
 "N'"[module] ->
   Alias(<.23>
         {<.20>
          "t"[type] -> {<.18>
                        "A"[constructor] -> {<.19>};
                        };
          });
 }
module N' = M'
|}]

module Test = struct
  module M = struct
    type t = A
  end
end
[%%expect{|
{
 "Test"[module] ->
   {<.27>
    "M"[module] -> {<.26>
                    "t"[type] -> {<.24>
                                  "A"[constructor] -> {<.25>};
                                  };
                    };
    };
 }
module Test : sig module M : sig type t = A end end
|}, Principal{|
{
 "Test"[module] ->
   {<.31>
    "M"[module] -> {<.30>
                    "t"[type] -> {<.28>
                                  "A"[constructor] -> {<.29>};
                                  };
                    };
    };
 }
module Test : sig module M : sig type t = A end end
|}, Rectypes{|
{
 "Test"[module] ->
   {<.35>
    "M"[module] -> {<.34>
                    "t"[type] -> {<.32>
                                  "A"[constructor] -> {<.33>};
                                  };
                    };
    };
 }
module Test : sig module M : sig type t = A end end
|}]

include Test
[%%expect{|
{
 "M"[module] -> {<.26>
                 "t"[type] -> {<.24>
                               "A"[constructor] -> {<.25>};
                               };
                 };
 }
module M = Test.M
|}, Principal{|
{
 "M"[module] -> {<.30>
                 "t"[type] -> {<.28>
                               "A"[constructor] -> {<.29>};
                               };
                 };
 }
module M = Test.M
|}, Rectypes{|
{
 "M"[module] -> {<.34>
                 "t"[type] -> {<.32>
                               "A"[constructor] -> {<.33>};
                               };
                 };
 }
module M = Test.M
|}]

module N = M
[%%expect{|
{
 "N"[module] ->
   Alias(<.36>
         {<.26>
          "t"[type] -> {<.24>
                        "A"[constructor] -> {<.25>};
                        };
          });
 }
module N = M
|}, Principal{|
{
 "N"[module] ->
   Alias(<.37>
         {<.30>
          "t"[type] -> {<.28>
                        "A"[constructor] -> {<.29>};
                        };
          });
 }
module N = M
|}, Rectypes{|
{
 "N"[module] ->
   Alias(<.38>
         {<.34>
          "t"[type] -> {<.32>
                        "A"[constructor] -> {<.33>};
                        };
          });
 }
module N = M
|}]
