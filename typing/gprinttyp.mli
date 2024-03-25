(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Florian Angeletti, projet Cambium, Inria Paris             *)
(*                                                                        *)
(*   Copyright 2024 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type params
type decoration
type element
type digraph


val types: title:string -> params -> (decoration * Types.type_expr) list -> unit
(** Print a graph to the file
    [asprintf "%s/%04d-%s-%a.dot"
       dump_dir
       session_unique_id
       title
       pp_context context
    ]

 If the [dump_dir] flag is not set, the local directory is used.
 See below for how to setup the context *)

(** Full version of {!types} that allow to print any kind of graph entity *)
val nodes: title:string -> params -> (decoration * element) list -> unit

val params:
  ?ellide_links:bool ->
  ?expansion_as_hyperedge:bool ->
  ?short_ids:bool ->
  ?colorize:bool ->
  unit -> params
(** Choice of details for printing type graphes:
    - if [ellide_links] is [true] link nodes are not displayed
    - with [expansion_as_hyperedge], memoized constructor expansion are displayed
as a hyperedge between the node storing the memoized expansion, the expanded
node and the expansion.
    - with [short_ids], we use an independent counter for node ids, in order to
     have shorter ids for small digraphs
    - with [colorize] nodes are colorized according to their typechecker ids.
*)

type dir = Toward | From
val node: Types.type_expr -> element
val edge: Types.type_expr -> Types.type_expr -> element
val hyperedge: (dir * decoration * Types.type_expr) list -> element


(** {1 Node and decoration types} *)
module Decoration: sig
  type color =
    | Named of string
    | HSL of {h:float;s:float;l:float}

  val green: color
  val blue: color
  val red:color
  val purple:color
  val hsl: h:float -> s:float -> l:float -> color

  type style =
    | Filled of color option
    | Dotted
    | Dash

  type shape =
    | Ellipse
    | Circle
    | Diamond

  type property =
    | Color of color
    | Font_color of color
    | Style of style
    | Label of string list
    | Shape of shape
  val filled: color -> property
  val txt: string -> property
  val make: property list -> decoration
end

(** {1 Digraph construction}*)

val make: params -> (decoration * element) list -> digraph

val add: params -> (decoration * element) list -> digraph -> digraph

(** add a subgraph to a digraph, only fresh nodes are added to the subgraph *)
val add_subgraph:
  params -> decoration -> (decoration * element) list -> digraph -> digraph

(** groups existing nodes inside a subgraph *)
val group: decoration * digraph -> digraph -> digraph

val pp: Format.formatter -> digraph -> unit


(** {1 Debugging helper functions } *)

(** {2 Generic print debugging function} *)

(** Conditional graph printing *)
val debug_on: (unit -> bool) ref
val debug_off: (unit -> 'a) -> 'a
val debug: (unit -> unit) -> unit

(** {2 Node tracking functions }*)

(** [register_type (lbl,ty)] adds the type [t] to all graph printed until
    {!forget} is called *)
val register_type: decoration * Types.type_expr -> unit

(** [register_subgraph params tys] groups together all types reachable from
    [tys] at this point in printed digraphs, until {!forget} is called *)
val register_subgraph: params -> ?decoration:decoration -> Types.type_expr list -> unit

(** Forget all recorded context types *)
val forget : unit -> unit

(** {2 Contextual information}

  Those function can be used to modify the filename of the generated digraphs.*)
type 'a context
val global: string context
val loc: Warnings.loc context
val set_context: 'a context -> 'a -> unit
val with_context: 'a context -> 'a -> (unit -> 'b) -> 'b
