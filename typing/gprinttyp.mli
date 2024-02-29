type node_label =
| Left
| Right
| Important
| Normal

type 'a context
val global: string context
val loc: Warnings.loc context
val set_context: 'a context -> 'a -> unit

val types: string -> (node_label * Types.type_expr) list -> unit
