(* TEST
   * toplevel
   flags = "-json -dparsetree -dtypedtree -dsource -drawlambda -dlambda -dinstr"
*)

Printexc.record_backtrace true;;

let f () = raise Not_found;;
let g () = f ();;
g ();;