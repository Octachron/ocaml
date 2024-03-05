val debug_on: bool ref
val debug_off: (unit -> 'a) -> 'a
val debug: (unit -> unit) -> unit

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
val with_context: 'a context -> 'a -> (unit -> 'b) -> 'b

val register_node: label * Types.type_expr -> unit
val forget : unit -> unit


type dir = Toward | From
type entity
  | Node of Types.type_expr
  | Edge of Types.type_expr * Types.type_expr
  | Hyperedge of (dir * label * Types.type_expr) list

type params
val params:
  ?ellide_links:bool ->
  ?expansion_as_hyperedge:bool ->
  ?short_ids:bool ->
  unit -> params

val dump_to: string -> params -> (label * entity) list -> unit
val types: string -> params -> (label * Types.type_expr) list -> unit
