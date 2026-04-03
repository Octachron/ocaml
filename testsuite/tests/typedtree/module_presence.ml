(* TEST
 flags = "-dtypedtree -dno-locations";
 expect;
*)

module X = struct end
[%%expect{|
[
  structure_item
    Tstr_module (Present)
    X/281
      module_expr
        Tmod_structure
        []
]

module X : sig end
|}, Principal{|
[
  structure_item
    Tstr_module (Present)
    X/282
      module_expr
        Tmod_structure
        []
]

module X : sig end
|}, Rectypes{|
[
  structure_item
    Tstr_module (Present)
    X/283
      module_expr
        Tmod_structure
        []
]

module X : sig end
|}]

module X = struct end [@foo]
[%%expect{|
[
  structure_item
    Tstr_module (Present)
    X/284
      module_expr
        attribute "foo"
          []
        Tmod_structure
        []
]

module X : sig end
|}, Principal{|
[
  structure_item
    Tstr_module (Present)
    X/285
      module_expr
        attribute "foo"
          []
        Tmod_structure
        []
]

module X : sig end
|}, Rectypes{|
[
  structure_item
    Tstr_module (Present)
    X/286
      module_expr
        attribute "foo"
          []
        Tmod_structure
        []
]

module X : sig end
|}]

module Y = X
[%%expect{|
[
  structure_item
    Tstr_module (Absent)
    Y/287
      module_expr
        Tmod_ident "X/284"
]

module Y = X
|}, Principal{|
[
  structure_item
    Tstr_module (Absent)
    Y/288
      module_expr
        Tmod_ident "X/285"
]

module Y = X
|}, Rectypes{|
[
  structure_item
    Tstr_module (Absent)
    Y/289
      module_expr
        Tmod_ident "X/286"
]

module Y = X
|}]

module type T = sig module Y = X end
[%%expect{|
[
  structure_item
    Tstr_modtype "T/291"
      module_type
        Tmty_signature
        [
          signature_item
            Tsig_module (Absent)
            Y/290
              module_type
                Tmty_alias "X/284"
        ]
]

module type T = sig module Y = X end
|}, Principal{|
[
  structure_item
    Tstr_modtype "T/294"
      module_type
        Tmty_signature
        [
          signature_item
            Tsig_module (Absent)
            Y/293
              module_type
                Tmty_alias "X/285"
        ]
]

module type T = sig module Y = X end
|}, Rectypes{|
[
  structure_item
    Tstr_modtype "T/297"
      module_type
        Tmty_signature
        [
          signature_item
            Tsig_module (Absent)
            Y/296
              module_type
                Tmty_alias "X/286"
        ]
]

module type T = sig module Y = X end
|}]
