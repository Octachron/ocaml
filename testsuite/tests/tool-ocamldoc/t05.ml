(* TEST
   plugins="odoc_test.ml"
    * ocamldoc with unix,str
    flags="-I ${ocamlsrcdir}/ocamldoc"
*)
module rec A : sig type t end = B and B : sig type t = A.t end = A;;

