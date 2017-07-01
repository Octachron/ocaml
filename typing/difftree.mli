(** Compute the difference between two outcome tree *)

(** Control how much difference tree are extended *)
val fuel: int ref

module Gen: sig
  type 'a t = 'a * 'a -> int -> 'a * 'a
  val typ: Outcometree.out_type t
  val sig_item: Outcometree.out_sig_item t
  val class_type: Outcometree.out_class_type t
  val modtype: Outcometree.out_module_type t
end

type 'a t = 'a * 'a -> 'a * 'a

val typ: Outcometree.out_type t
val sig_item: Outcometree.out_sig_item t
val class_type: Outcometree.out_class_type t
val modtype: Outcometree.out_module_type t
