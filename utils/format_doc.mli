(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Florian Angeletti, projet Cambium, Inria Paris             *)
(*                                                                        *)
(*   Copyright 2023 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Composable document for the Format formatting engine *)

type box_type =
  | H
  | V
  | HV
  | HoV
  | B

type stag = Format.stag

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

type t
type doc = t

val empty: t
val format: Format.formatter -> doc -> unit
val fold: ('acc -> element -> 'acc) -> 'acc -> doc -> 'acc


module Immutable: sig
  type ('a,'b) fmt = ('a, doc, doc,'b) format4
  type printer = doc -> doc
  val printf: ('a, printer) fmt -> 'a
  val msg: ('a,doc) fmt -> 'a
  val kmsg: (doc -> 'b) -> ('a,'b) fmt -> 'a
  val kprintf: (doc -> 'b) -> ('a, doc -> 'b) fmt -> 'a
end


val open_box: box_type -> int -> Immutable.printer
val close_box: Immutable.printer

val text: string -> Immutable.printer
val string: string -> Immutable.printer
val bytes: bytes -> Immutable.printer
val with_size: int -> Immutable.printer

val int: int -> Immutable.printer
val float: float -> Immutable.printer
val char: char -> Immutable.printer
val bool: bool -> Immutable.printer

val space: Immutable.printer
val cut: Immutable.printer
val break: spaces:int -> indent:int -> Immutable.printer

val custom_break:
  fits:(string * int * string as 'a) -> breaks:'a -> Immutable.printer
val force_newline: Immutable.printer
val if_newline: Immutable.printer

val flush: Immutable.printer
val force_stop: Immutable.printer

val open_tbox: Immutable.printer
val set_tab: Immutable.printer
val tab: Immutable.printer
val tab_break: width:int -> offset:int -> Immutable.printer
val close_tbox: Immutable.printer

val open_tag: stag -> Immutable.printer
val close_tag: Immutable.printer

val list:
  ?sep:(doc->doc) -> ('a -> Immutable.printer) -> 'a list -> Immutable.printer

 (*
val iter: ?sep:(doc -> doc)
  -> (('a -> unit) -> 'b -> unit)
  -> ('a -> doc -> doc) -> 'b ->
  doc -> doc


val array: ?sep:(doc->doc) -> ('a -> doc -> 'doc) -> 'a array -> doc -> doc
val seq: ?sep:(doc->doc) -> ('a -> doc -> 'doc) -> 'a Seq.t -> doc -> doc

val text: string -> doc -> doc

val option: ?none:(doc->doc) -> ('a -> doc -> doc) -> 'a option -> doc -> doc
val result: ok:('a -> doc -> doc) -> error:('b -> doc -> doc) ->
  ('a,'b) result -> doc -> doc

val either: left:('a -> doc -> doc) -> right:('b -> doc -> doc) ->
  ('a,'b) Either.t -> doc -> doc
*)




module Ref: sig
  type ('a,'b) fmt = ('a, doc ref, unit,'b) format4
  val printf: doc ref -> ('a, unit) fmt -> 'a
  val kprintf: (doc ref -> 'b) -> doc ref -> ('a, 'b) fmt -> 'a
end



module Compat: sig

  type _ formatter
  type rdoc
  type doc_fmt = rdoc formatter

  val make_formatter: Format.formatter -> Format.formatter formatter
  val formatter_of_out_channel: out_channel -> Format.formatter formatter
  val make_doc: doc ref -> doc_fmt

  val doc: doc_fmt -> doc
  val formatter: Format.formatter formatter -> Format.formatter


  val fprintf : 'impl formatter -> ('a,'impl formatter,unit) format -> 'a
  val kfprintf:
    ('impl formatter -> 'a) -> 'impl formatter ->
    ('b, 'impl formatter, unit, 'a) format4 -> 'b


  val asprintf :  ('a, Format.formatter formatter, unit, string) format4 -> 'a
  val kasprintf : (string -> 'a) ->
    ('b, Format.formatter formatter, unit, 'a) format4 -> 'b


  val dprintf : ('a,'impl formatter, unit, 'impl formatter -> unit) format4 -> 'a
  val kdprintf:
    (('impl formatter -> unit) -> 'a) ->
    ('b, 'impl formatter, unit, 'a) format4 -> 'b

  val doc_printf: ('a, rdoc formatter, unit, doc) format4 -> 'a


  type ('a,'impl) printer = 'impl formatter -> 'a -> unit
  type 'a final_printer = ('a, Format.formatter) printer
  type 'a generic_printer = { printer: 'impl. ('a,'impl) printer }

  val format_printer: 'a final_printer -> Format.formatter -> 'a -> unit
  val doc_printer:('a, rdoc) printer -> 'a -> Immutable.printer


  val pp_doc: (doc,_) printer

  val pp_print_string: (string,_) printer
  val pp_print_text: (string,_) printer
  val pp_print_char: (char,_) printer
  val pp_print_int: (int,_) printer
  val pp_print_float: (float,_) printer


  val pp_print_list:
    ?pp_sep:(unit,'impl) printer -> ('a,'impl) printer -> ('a list, 'impl) printer


  val pp_print_option:
    ?none:(unit,'impl) printer -> ('a,'impl) printer -> ('a option, 'impl) printer
  val pp_open_stag: (Format.stag,_) printer
  val pp_close_stag: (unit,_) printer

  val pp_open_box: (int,_) printer
  val pp_close_box: (unit,_) printer

  val pp_print_space: (unit,_) printer
  val pp_print_cut: (unit,_) printer
  val pp_print_break: _ formatter -> int -> int -> unit


  val pp_open_tbox: (unit,_) printer
  val pp_close_tbox: (unit,_) printer
  val pp_set_tab: (unit,_) printer
  val pp_print_tab: (unit,_) printer
  val pp_print_tbreak: 'impl formatter -> int -> int -> unit


(** {1 Compiler output} *)


val pp_two_columns :
  ?sep:string -> ?max_lines:int ->
  'impl formatter -> (string * string) list -> unit
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

end


val compat: ('a,Format.formatter) Compat.printer -> Format.formatter -> 'a -> unit
