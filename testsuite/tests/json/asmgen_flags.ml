(* TEST
  * native
   flags += "-json -dparsetree -dtypedtree -dsource -dlambda -dclambda -drawclambda -linscan -dinterval -dalloc -dlive -dreload -dsel -dcombine -dcse -dspill -dsplit -dlinear -dscheduling"
*)

let kas a = match a with _ -> true;;