let () = A.M.r#m (* work *)
let () = B.N.y#m (* work *)

 let () = B.N.z#m
(* fail with:
   File "test.ml", line 3, characters 9-14:
   Error: This expression has type < m : unit > M.t
       It has no method m
 *)

let () = B.N.x#m
(* fail with:
   File "test.ml", line 3, characters 9-14:
   Error: This expression has type < m : unit > M.t
       It has no method m
   Did you mean m?
 *)
