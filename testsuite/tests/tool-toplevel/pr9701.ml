(* TEST
ocaml_script_as_argument = "true"
ocaml_exit_status = "2"
* setup-ocaml-build-env
** ocaml
*)

Printexc.record_backtrace true;;

let f () = failwith "test";;
let proc () = f ();;
let () = proc ();;
