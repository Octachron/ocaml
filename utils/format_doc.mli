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

(** Composable document for the {!Format} formatting engine. *)

(** This module introduces a pure and immutable document type which represents a
    sequence of formatting instructions to be printed by a formatting engine at
    later point. At the same time, it also provides format string interpreter
    which produces this document type from format string and their associated
    printer.


    The module is designed to be source compatible with code defining format
    printers: replacing `Format` by `Format_doc` in your code will convert
    `Format` printers to `Format_doc` printers.
*)

(** Format box types as an ADT *)
type box_type =
  | H
  | V
  | HV
  | HoV
  | B

type stag = Format.stag

(** Base formatting instruction recognized by {!Format} *)
type element =
  | Data of string
  | With_size of int
  | Open_box of { kind: box_type ; indent:int }
  | Close_box
  | Open_tag of Format.stag
  | Close_tag
  | Open_tbox
  | Tab_break of { width : int; offset : int }
  | Set_tab
  | Close_tbox
  | Simple_break of { spaces : int; indent : int }
  | Break of { fits : string * int * string as 'a; breaks : 'a }
  | Flush of { newline:bool }
  | Newline
  | If_newline

 (** Immutable document type*)
type t
type doc = t

(** Empty document *)
val empty: t

(** [format ppf doc] sends the format instruction of [doc] to the Format's
    formatter [doc]. *)
val format: Format.formatter -> doc -> unit

(** Fold over a document as a sequence of instructions *)
val fold: ('acc -> element -> 'acc) -> 'acc -> doc -> 'acc


module Immutable: sig
  type ('a,'b) fmt = ('a, doc, doc,'b) format4

  type printer0 = doc -> doc
  type 'a printer = 'a -> printer0

  val printf: ('a, printer0) fmt -> 'a
  val msg: ('a,doc) fmt -> 'a
  val kmsg: (doc -> 'b) -> ('a,'b) fmt -> 'a
  val kprintf: (doc -> 'b) -> ('a, doc -> 'b) fmt -> 'a

  val open_box: box_type -> int -> printer0
  val close_box: printer0

  val text: string printer
  val string: string printer
  val bytes: bytes printer
  val with_size: int printer

  val int: int printer
  val float: float printer
  val char: char printer
  val bool: bool printer

  val space: printer0
  val cut: printer0
  val break: spaces:int -> indent:int -> printer0

  val custom_break:
    fits:(string * int * string as 'a) -> breaks:'a -> printer0
  val force_newline: printer0
  val if_newline: printer0

  val flush: printer0
  val force_stop: printer0

  val open_tbox: printer0
  val set_tab: printer0
  val tab: printer0
  val tab_break: width:int -> offset:int -> printer0
  val close_tbox: printer0

  val open_tag: stag printer
  val close_tag: printer0

  val list: ?sep:printer0 -> 'a printer -> 'a list printer
  val iter:
    ?sep:printer0 -> iter:(('a -> unit) -> 'b -> unit) -> 'a printer
    ->'b printer
  val array: ?sep:printer0 -> 'a printer -> 'a array printer
  val seq: ?sep:printer0 -> 'a printer -> 'a Seq.t printer


  val option: ?none:printer0 -> 'a printer -> 'a option printer
  val result: ok:'a printer -> error:'e printer -> ('a,'e) result printer
  val either: left:'a printer -> right:'b printer -> ('a,'b) Either.t printer

end

module Ref: sig
  type ('a,'b) fmt = ('a, doc ref, unit,'b) format4
  val printf: doc ref -> ('a, unit) fmt -> 'a
  val kprintf: (doc ref -> 'b) -> doc ref -> ('a, 'b) fmt -> 'a
end

type formatter

type 'a printer = formatter -> 'a -> unit


val make_formatter: Format.formatter -> formatter
val formatter_of_out_channel: out_channel -> formatter
val make_doc: doc ref -> formatter
val compat: 'a printer -> Format.formatter -> 'a -> unit
val approx: (Format.formatter -> unit) -> formatter -> unit

val doc: formatter -> doc option
val formatter: formatter -> Format.formatter option


val fprintf : formatter -> ('a, formatter,unit) format -> 'a
val kfprintf:
  (formatter -> 'a) -> formatter ->
  ('b, formatter, unit, 'a) format4 -> 'b


val asprintf :  ('a, formatter, unit, string) format4 -> 'a
val kasprintf : (string -> 'a) -> ('b, formatter, unit, 'a) format4 -> 'b


val dprintf : ('a, formatter, unit, formatter -> unit) format4 -> 'a
val kdprintf:
  ((formatter -> unit) -> 'a) ->
  ('b, formatter, unit, 'a) format4 -> 'b

val doc_printf: ('a, formatter, unit, doc) format4 -> 'a
val kdoc_printf: (doc -> 'r) -> ('a, formatter, unit, 'r) format4 -> 'a

val doc_printer: 'a printer -> 'a Immutable.printer
val pp_doc: doc printer

(** {1 String printers } *)
val pp_print_string: string printer
val pp_print_substring: pos:int -> len:int -> string printer
val pp_print_text: string printer
val pp_print_bytes: bytes printer

val pp_print_as: formatter -> int -> string -> unit
val pp_print_substring_as:
  pos:int -> len:int -> formatter -> int -> string -> unit

val pp_print_char: char printer
val pp_print_int: int printer
val pp_print_float: float printer
val pp_print_bool: bool printer
val pp_print_nothing: unit printer

(** {1 Printer combinator }*)
val pp_print_iter:
  ?pp_sep:unit printer -> (('a -> unit) -> 'b -> unit) ->
  'a printer -> 'b printer

val pp_print_list: ?pp_sep:unit printer -> 'a printer -> 'a list printer
val pp_print_array: ?pp_sep:unit printer -> 'a printer -> 'a array printer
val pp_print_seq: ?pp_sep:unit printer -> 'a printer -> 'a Seq.t printer

val pp_print_option: ?none:unit printer -> 'a printer -> 'a option printer

val pp_print_result:
  ok:'a printer -> error:'e printer -> ('a,'e) result printer

val pp_print_either:
  left:'a printer -> right:'b printer -> ('a,'b) Either.t printer



(** {1 Boxes and tags }*)
val pp_open_stag: Format.stag printer
val pp_close_stag: unit printer

val pp_open_box: int printer
val pp_close_box: unit printer

(** {1 Break hints} *)
val pp_print_space: unit printer
val pp_print_cut: unit printer
val pp_print_break: formatter -> int -> int -> unit
val pp_print_custom_break:
  formatter -> fits:(string * int * string as 'c) -> breaks:'c -> unit

(** {1 Tabulations }*)
val pp_open_tbox: unit printer
val pp_close_tbox: unit printer
val pp_set_tab: unit printer
val pp_print_tab: unit printer
val pp_print_tbreak: formatter -> int -> int -> unit

(** {1 Newlines and flushing }*)
val pp_print_if_newline: unit printer
val pp_force_newline: unit printer
val pp_print_flush: unit printer
val pp_print_newline: unit printer


(** {1 Separators }*)

val comma: unit printer

(** {1 Compiler output} *)


val pp_two_columns :
  ?sep:string -> ?max_lines:int ->
  formatter -> (string * string) list -> unit
(** [pp_two_columns ?sep ?max_lines ppf l] prints the lines in [l] as two
   columns separated by [sep] ("|" by default). [max_lines] can be used to
   indicate a maximum number of lines to print -- an ellipsis gets inserted at
   the middle if the input has too many lines.

   Example:

    {v pp_two_columns ~max_lines:3 Format.std_formatter [
      "abc", "hello";
      "def", "zzz";
      "a"  , "bllbl";
      "bb" , "dddddd";
    ] v}

    prints

    {v
    abc | hello
    ...
    bb  | dddddd
    v}
*)
