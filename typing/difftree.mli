(** Compute the difference between two outcome tree *)

type 'a mk_diff = 'a -> 'a -> 'a * 'a

val typ: Outcometree.out_type mk_diff

val sig_item: Outcometree.out_sig_item mk_diff
