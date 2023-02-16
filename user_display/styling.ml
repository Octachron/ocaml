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


 (* Color handling *)

(* use ANSI color codes, see https://en.wikipedia.org/wiki/ANSI_escape_code *)
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
  | FG of color (* foreground *)
  | BG of color (* background *)
  | Bold
  | Reset

let ansi_of_color = function
  | Black -> "0"
  | Red -> "1"
  | Green -> "2"
  | Yellow -> "3"
  | Blue -> "4"
  | Magenta -> "5"
  | Cyan -> "6"
  | White -> "7"

let code_of_style = function
  | FG c -> "3" ^ ansi_of_color c
  | BG c -> "4" ^ ansi_of_color c
  | Bold -> "1"
  | Reset -> "0"

let ansi_of_style_l l =
  let s = match l with
    | [] -> code_of_style Reset
    | [s] -> code_of_style s
    | _ -> String.concat ";" (List.map code_of_style l)
  in
  "\x1b[" ^ s ^ "m"


type Format.stag += Style of style list
type styles = {
  error: style list;
  warning: style list;
  loc: style list;
  hint:style list;
}

let default_styles = {
  warning = [Bold; FG Magenta];
  error = [Bold; FG Red];
  loc = [Bold];
  hint = [Bold; FG Blue];
}

let cur_styles = ref default_styles
let get_styles () = !cur_styles
let set_styles s = cur_styles := s

(* map a tag to a style, if the tag is known.
   @raise Not_found otherwise *)
let style_of_tag s = match s with
  | Format.String_tag "error" -> (!cur_styles).error
  | Format.String_tag "warning" -> (!cur_styles).warning
  | Format.String_tag "loc" -> (!cur_styles).loc
  | Format.String_tag "hint" -> (!cur_styles).hint
  | Style s -> s
  | _ -> raise Not_found

let color_enabled = ref true

(* either prints the tag of [s] or delegates to [or_else] *)
let mark_open_tag ~or_else s =
  try
    let style = style_of_tag s in
    if !color_enabled then ansi_of_style_l style else ""
  with Not_found -> or_else s

let mark_close_tag ~or_else s =
  try
    let _ = style_of_tag s in
    if !color_enabled then ansi_of_style_l [Reset] else ""
  with Not_found -> or_else s

(* add color handling to formatter [ppf] *)
let set_color_tag_handling ppf =
  let open Format in
  let functions = pp_get_formatter_stag_functions ppf () in
  let functions' =
    {functions with
     mark_open_stag=(mark_open_tag ~or_else:functions.mark_open_stag);
     mark_close_stag=(mark_close_tag ~or_else:functions.mark_close_stag);
    } in
  pp_set_mark_tags ppf true; (* enable tags *)
  pp_set_formatter_stag_functions ppf functions';
  ()

external isatty : out_channel -> bool = "caml_sys_isatty"

(* reasonable heuristic on whether colors should be enabled *)
let should_enable_color () =
  let term = try Sys.getenv "TERM" with Not_found -> "" in
  term <> "dumb"
  && term <> ""
  && isatty stderr

type setting = Auto | Always | Never

let default_setting = Auto

let setup =
  let first = ref true in (* initialize only once *)
  let formatter_l =
    [Format.std_formatter; Format.err_formatter; Format.str_formatter]
  in
  let enable_color = function
    | Auto -> should_enable_color ()
    | Always -> true
    | Never -> false
  in
  fun o ->
    if !first then (
      first := false;
      Format.set_mark_tags true;
      List.iter set_color_tag_handling formatter_l;
      color_enabled := (match o with
          | Some s -> enable_color s
          | None -> enable_color default_setting)
    );
    ()


module Error_style = struct
  type setting =
    | Contextual
    | Short

  let default_setting = Contextual
end
