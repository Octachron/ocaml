(** Compute the difference between two outcome tree *)

(** Control how much difference tree are extended *)
val fuel: int ref

module Gen: sig
  type ('a,'b) t = 'a * 'a -> int -> 'b * 'b
  val typ: (Outcometree.out_type, Outcometree.Decorated.out_type) t
  val sig_item: (Outcometree.out_sig_item, Outcometree.Decorated.out_sig_item) t
  val class_type:
    (Outcometree.out_class_type, Outcometree.Decorated.out_class_type) t
  val modtype:
    (Outcometree.out_module_type, Outcometree.Decorated.out_module_type) t
end

type ('a,'b) t = 'a * 'a -> 'b * 'b

val typ: (Outcometree.out_type, Outcometree.Decorated.out_type) t
val sig_item: (Outcometree.out_sig_item, Outcometree.Decorated.out_sig_item) t
val class_type:
  (Outcometree.out_class_type, Outcometree.Decorated.out_class_type) t
val modtype:
  (Outcometree.out_module_type, Outcometree.Decorated.out_module_type) t
