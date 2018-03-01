(* TEST
   * ocamldoc with html
     ocamldoc_flags = " -hide-warnings "
*)
module M = Set.Make(struct
        type t = int
        let compare = compare
end)
