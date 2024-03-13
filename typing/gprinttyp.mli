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
type label
type element

val types: title:string -> params -> (label * Types.type_expr) list -> unit
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
val nodes: title:string -> params -> (label * element) list -> unit

val params:
  ?ellide_links:bool ->
  ?expansion_as_hyperedge:bool ->
  ?short_ids:bool ->
  unit -> params
(** Choice of details for printing type graphes:
    - if [ellide_links] is [true] link nodes are not displayed
    - with [expansion_as_hyperedge], memoized constructor expansion are displayed
as a hyperedge between the node storing the memoized expansion, the expanded
node and the expansion.
    - with [short_ids], we use an independent counter for node ids, in order to
     have shorter ids for small digraphs
*)

(** {2 Contextual information for generated graphs} *)
type 'a context
val global: string context
val loc: Warnings.loc context
val set_context: 'a context -> 'a -> unit
val with_context: 'a context -> 'a -> (unit -> 'b) -> 'b

(** {1 Generic print debugging function} *)

(** Conditional graph printing *)
val debug_on: (unit -> bool) ref
val debug_off: (unit -> 'a) -> 'a
val debug: (unit -> unit) -> unit

(** {1 Node and decoration types} *)

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
| Label of string list

val label: modal list -> label


type dir = Toward | From
val node: Types.type_expr -> element
val edge: Types.type_expr -> Types.type_expr -> element
val hyperedge: (dir * label * Types.type_expr) list -> element

(** {1 Node tracking functions }*)


(** [register_type (lbl,ty)] adds the type [t] to all graph printed until
    {!forget} is called *)
val register_type: label * Types.type_expr -> unit

(** Forget all recorded context types *)
val forget : unit -> unit
