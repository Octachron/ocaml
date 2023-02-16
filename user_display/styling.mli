(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Gabriel Scherer, projet Parsifal, INRIA Saclay             *)
(*                                                                        *)
(*   Copyright 2023 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

 (** Colored terminal output  *)

type color =
  | Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White

type style =
  | FG of color (** foreground *)
  | BG of color (** background *)
  | Bold
  | Reset
type Format.stag += Style of style list

val ansi_of_style_l : style list -> string
(** ANSI escape sequence for the given style *)

type styles = {
  error: style list;
  warning: style list;
  loc: style list;
  hint: style list;
}

val default_styles: styles
val get_styles: unit -> styles
val set_styles: styles -> unit

type setting = Auto | Always | Never

val default_setting : setting

val setup : setting option -> unit
(** [setup opt] will enable or disable color handling on standard formatters
    according to the value of color setting [opt].
    Only the first call to this function has an effect. *)

val set_color_tag_handling : Format.formatter -> unit
(** adds functions to support color tags to the given formatter. *)


(** See the -error-style option *)
module Error_style : sig
  type setting =
    | Contextual
    | Short

  val default_setting : setting
end
