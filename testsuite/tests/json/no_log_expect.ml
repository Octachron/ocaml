(* TEST
  * expect
   flags += "-json -dtypedtree -dsource "
*)

let kas a = match a with _ -> true;;

[%%expect{|

let kas a = match a with | _ -> true;;
{"dumped_typed_tree":
    "[\n  structure_item ([1,65+0]..[1,65+34])\n    Tstr_value Nonrec\n    [\n      <def>\n        pattern ([1,65+4]..[1,65+7])\n          Tpat_var \"kas/87\"\n        expression ([1,65+8]..[1,65+34]) ghost\n          Texp_function\n          Nolabel\n          [\n            <case>\n              pattern ([1,65+8]..[1,65+9])\n                Tpat_var \"a/89\"\n              expression ([1,65+12]..[1,65+34])\n                Texp_match\n                expression ([1,65+18]..[1,65+19])\n                  Texp_ident \"a/89\"\n                [\n                  <case>\n                    pattern ([1,65+25]..[1,65+26])\n                      Tpat_value\n                      pattern ([1,65+25]..[1,65+26])\n                        Tpat_any\n                    expression ([1,65+30]..[1,65+34])\n                      Texp_construct \"true\"\n                      []\n                ]\n          ]\n    ]\n]\n",
 "phrase": "val kas : 'a -> bool = <fun>\n"
}
|}]
