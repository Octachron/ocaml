(* TEST
 flags = "-stop-after parsing -dsource -dparsetree -dno-locations";
 ocamlparam=",_,metaocaml-mode=1";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(** In the MetaOCaml mode, the lexer always emits the DOTTILDE token for .~*)
let escape = .~ x

(** Contrarily, >. is only translated to the MetaOCaml token after the first
    .> *)
let (>.): float -> float -> bool = Stdlib.(>)
let _ = 1. >. 4.;;


let test = .< x >. ;;

let test2 = .~ x

(** Test that the extension syntax is normalized to the constructor version *)
let _normalize = [%metaocaml.escape y]
let _normalize = [%metaocaml.bracket z]
