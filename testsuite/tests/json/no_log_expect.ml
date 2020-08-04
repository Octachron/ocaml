(* TEST
  * expect
   flags += "-json -dtypedtree -dsource "
*)

let kas a = match a with _ -> true;;

[%%expect{|

let kas a = match a with | _ -> true;;
[
  structure_item ([1,65+0]..[1,65+34])
    Tstr_value Nonrec
    [
      <def>
        pattern ([1,65+4]..[1,65+7])
          Tpat_var "kas/87"
        expression ([1,65+8]..[1,65+34]) ghost
          Texp_function
          Nolabel
          [
            <case>
              pattern ([1,65+8]..[1,65+9])
                Tpat_var "a/89"
              expression ([1,65+12]..[1,65+34])
                Texp_match
                expression ([1,65+18]..[1,65+19])
                  Texp_ident "a/89"
                [
                  <case>
                    pattern ([1,65+25]..[1,65+26])
                      Tpat_value
                      pattern ([1,65+25]..[1,65+26])
                        Tpat_any
                    expression ([1,65+30]..[1,65+34])
                      Texp_construct "true"
                      []
                ]
          ]
    ]
]

{"phrase": "val kas : 'a -> bool = <fun>\n"}
|}]
