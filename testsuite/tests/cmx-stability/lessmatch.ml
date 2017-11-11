(* TEST
   * native
   ocamlopt_flags="-inline 20"
*)

(* Test that the cmx produced by ocamlopt and ocamlopt.opt are the
   same even in presence of inlining
*)

let f = function _ -> assert false
let g (`A _ as x) = `B x
