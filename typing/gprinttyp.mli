type color =
  | Red
  | Green
  | Blue
  | Purple
  | Black

type style =
  | Filled
  | Dotted
  | Dash

type modal =
| Color of color
| Background of color
| Style of style
| Label of string

type label = modal list

type 'a context
val global: string context
val loc: Warnings.loc context
val set_context: 'a context -> 'a -> unit

val register_node: label * Types.type_expr -> unit
val forget : unit -> unit

type entity =
  | Node of Types.type_expr
  | Edge of Types.type_expr * Types.type_expr
  | Hyperedge of Types.type_expr list

val dump_to: string -> (label * entity) list -> unit
val types: string -> (label * Types.type_expr) list -> unit
