(** Compute the difference between two outcome tree *)

type 'a mk_diff = 'a * 'a -> 'a * 'a

(** Control how much difference tree are extended *)
val fuel: int ref

val typ: Outcometree.out_type mk_diff

val sig_item: Outcometree.out_sig_item mk_diff

val class_type: Outcometree.out_class_type mk_diff

val modtype: Outcometree.out_module_type mk_diff
