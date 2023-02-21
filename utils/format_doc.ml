(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Florian Angeletti, projet Cambium, Inria Paris             *)
(*                                                                        *)
(*   Copyright 2021 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)


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
  | Simple_break of { spaces : int; indent: int }
  | Break of { fits : string * int * string as 'a; breaks : 'a }
  | Flush of { newline:bool }
  | Newline
  | If_newline

type doc = {front: element list; back:element list}
type t = doc

let empty : doc = { front = []; back = [] }

let to_list doc = doc.front @ List.rev doc.back

let prepend x doc = { doc with front = x :: doc.front }
let append x doc = { doc with back = x :: doc.back }

let interpret_elt ppf = function
  | Data x -> Format.pp_print_string ppf x
  | Open_box { kind; indent } ->
      begin match kind with
      | H-> Format.pp_open_hbox ppf ()
      | V -> Format.pp_open_vbox ppf indent
      | HV -> Format.pp_open_hvbox ppf indent
      | HoV -> Format.pp_open_hovbox ppf indent
      | B -> Format.pp_open_box ppf indent
      end
  | Close_box -> Format.pp_close_box ppf ()
  | Open_tag tag -> Format.pp_open_stag ppf tag
  | Close_tag -> Format.pp_close_stag ppf ()
  | Open_tbox -> Format.pp_open_tbox ppf ()
  | Tab_break {width;offset} -> Format.pp_print_tbreak ppf width offset
  | Set_tab -> Format.pp_set_tab ppf ()
  | Close_tbox -> Format.pp_close_tbox ppf ()
  | Simple_break {spaces;indent} -> Format.pp_print_break ppf spaces indent
  | Break {fits;breaks} -> Format.pp_print_custom_break ppf ~fits ~breaks
  | Flush {newline=true} -> Format.pp_print_newline ppf ()
  | Flush {newline=false} -> Format.pp_print_flush ppf ()
  | Newline -> Format.pp_force_newline ppf ()
  | If_newline -> Format.pp_print_if_newline ppf ()
  | With_size _ -> ()

let rec interpret ppf = function
  | [] -> ()
  | With_size n :: Data x :: q ->
      Format.pp_print_as ppf n x;
      interpret ppf q
  | x :: q ->
      interpret_elt ppf x;
      interpret ppf q

let format doc ppf = interpret ppf (to_list doc)

let fold f acc doc =
  let first = List.fold_left f acc doc.front in
  let back = List.fold_left f first (List.rev doc.back) in
  back


let open_box kind indent doc = append (Open_box {kind;indent}) doc
let close_box doc = append Close_box doc

let string s doc = append (Data s) doc
let bytes b doc = append (Data (Bytes.to_string b)) doc
let with_size n doc = append (With_size n) doc

let int n doc = append (Data (string_of_int n)) doc
let float f doc = append (Data (string_of_float f)) doc
let char c doc = append (Data (String.make 1 c)) doc
let bool c doc = append (Data (Bool.to_string c)) doc


let break ~spaces ~indent doc = append (Simple_break {spaces; indent}) doc
let space doc = break ~spaces:1 ~indent:0 doc
let cut = break ~spaces:0 ~indent:0

let custom_break ~fits ~breaks doc = append (Break {fits;breaks}) doc

let force_newline doc = append Newline doc
let if_newline doc = append If_newline doc

let flush doc = append (Flush {newline=false}) doc
let force_stop = append (Flush {newline=true})

let open_tbox doc = append Open_tbox doc
let set_tab doc = append Set_tab doc
let tab_break ~width ~offset doc = append (Tab_break {width;offset}) doc
let tab doc = tab_break ~width:0 ~offset:0 doc
let close_tbox doc = append Close_tbox doc

let open_tag stag doc = append (Open_tag stag) doc
let close_tag doc = append Close_tag doc

let output_formatting_lit fmting_lit doc = match fmting_lit with
  | CamlinternalFormatBasics.Close_box    -> close_box doc
  | Close_tag                 -> close_tag doc
  | Break (_, width, offset)  -> break ~spaces:width ~indent:offset doc
  | FFlush                    -> flush doc
  | Force_newline             -> force_newline doc
  | Flush_newline             -> force_stop doc
  | Magic_size (_, n)         -> with_size n doc
  | Escaped_at                -> char '@' doc
  | Escaped_percent           -> char '%' doc
  | Scan_indic c              -> doc |> char '@' |> char c

let to_string doc =
  let b = Buffer.create 20 in
  let convert = function
    | Data s -> Buffer.add_string b s
    | _ -> ()
  in
  fold (fun () x -> convert x) () doc;
  Buffer.contents b

let box_type = function
    | CamlinternalFormatBasics.Pp_fits -> H
    | Pp_hbox -> H
    | Pp_vbox -> V
    | Pp_hovbox -> HoV
    | Pp_hvbox -> HV
    | Pp_box -> B

(* Recursively output an "accumulator" containing a reversed list of
   printing entities (string, char, flus, ...) in an output_stream. *)
(* Differ from Printf.output_acc by the interpretation of formatting. *)
(* Used as a continuation of CamlinternalFormat.make_printf. *)
let rec output_acc acc doc = match acc with
  | CamlinternalFormat.Acc_formatting_lit (p, f) ->
    doc |> output_acc p |> output_formatting_lit f
  | Acc_formatting_gen (p, Acc_open_tag acc') ->
      let tag = to_string (output_acc acc' empty) in
      let doc = output_acc p doc in
      doc |> open_tag (Format.String_tag tag)
  | Acc_formatting_gen (p, Acc_open_box acc') ->
      let doc = output_acc p doc in
      let box = to_string (output_acc acc' empty) in
      let (indent, bty) = CamlinternalFormat.open_box_of_string box in
      doc |> open_box (box_type bty) indent
  | Acc_string_literal (p, s)
  | Acc_data_string (p, s)   ->
      doc |> output_acc p |> string s
  | Acc_char_literal (p, c)
  | Acc_data_char (p, c)     -> doc |> output_acc p |> char c
  | Acc_delay (p, f)         -> doc |> output_acc p |> f
  | Acc_flush p              -> doc |> output_acc p |> flush
  | Acc_invalid_arg (p, msg) ->  invalid_arg msg;
  | End_of_acc               -> doc


type ('a,'b) fmt = ('a, doc, doc, 'b) format4

let kprintf k doc =
  CamlinternalFormat.make_printf
    (fun acc doc -> doc |> output_acc acc |> k )
    End_of_acc doc

let printf doc = kprintf Fun.id doc
