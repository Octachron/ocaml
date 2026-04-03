(* TEST
 flags = "-dshape";
 expect;
*)

module M : sig

  exception Exn of { lbl_exn : int }
  type l = { lbl : int }
  type ext = ..
  type ext += Ext of { lbl_ext : int }
  type t = C of { lbl_cstr : int }
end = struct
  exception Exn of { lbl_exn : int }
  type l = { lbl : int }
  type ext = ..
  type ext += Ext of { lbl_ext : int }
  type t = C of { lbl_cstr : int }
end
[%%expect{|
{
 "M"[module] ->
   {<.37>
    "Exn"[extension constructor] -> {<.1>
                                     "lbl_exn"[label] -> <.0>;
                                     };
    "Ext"[extension constructor] -> {<.7>
                                     "lbl_ext"[label] -> <.6>;
                                     };
    "ext"[type] -> <.5>;
    "l"[type] -> {<.3>
                  "lbl"[label] -> <.4>;
                  };
    "t"[type] ->
      {<.9>
       "C"[constructor] -> {<.11>
                            "lbl_cstr"[label] -> <.10>;
                            };
       };
    };
 }
module M :
  sig
    exception Exn of { lbl_exn : int; }
    type l = { lbl : int; }
    type ext = ..
    type ext += Ext of { lbl_ext : int; }
    type t = C of { lbl_cstr : int; }
  end
|}, Principal{|
{
 "M"[module] ->
   {<.81>
    "Exn"[extension constructor] -> {<.45>
                                     "lbl_exn"[label] -> <.44>;
                                     };
    "Ext"[extension constructor] -> {<.51>
                                     "lbl_ext"[label] -> <.50>;
                                     };
    "ext"[type] -> <.49>;
    "l"[type] -> {<.47>
                  "lbl"[label] -> <.48>;
                  };
    "t"[type] ->
      {<.53>
       "C"[constructor] -> {<.55>
                            "lbl_cstr"[label] -> <.54>;
                            };
       };
    };
 }
module M :
  sig
    exception Exn of { lbl_exn : int; }
    type l = { lbl : int; }
    type ext = ..
    type ext += Ext of { lbl_ext : int; }
    type t = C of { lbl_cstr : int; }
  end
|}, Rectypes{|
{
 "M"[module] ->
   {<.125>
    "Exn"[extension constructor] -> {<.89>
                                     "lbl_exn"[label] -> <.88>;
                                     };
    "Ext"[extension constructor] -> {<.95>
                                     "lbl_ext"[label] -> <.94>;
                                     };
    "ext"[type] -> <.93>;
    "l"[type] -> {<.91>
                  "lbl"[label] -> <.92>;
                  };
    "t"[type] ->
      {<.97>
       "C"[constructor] -> {<.99>
                            "lbl_cstr"[label] -> <.98>;
                            };
       };
    };
 }
module M :
  sig
    exception Exn of { lbl_exn : int; }
    type l = { lbl : int; }
    type ext = ..
    type ext += Ext of { lbl_ext : int; }
    type t = C of { lbl_cstr : int; }
  end
|}]
