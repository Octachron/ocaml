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

type doc = { rev:element list } [@@unboxed]

type t = doc

let empty : doc = { rev = [] }

let to_list doc = List.rev doc.rev
let add x doc = { rev = x :: doc.rev }
let fold f acc doc = List.fold_left f acc (to_list doc)
let append left right = { rev = right.rev @ left.rev }

let format_open_box_gen ppf kind indent =
  match kind with
  | H-> Format.pp_open_hbox ppf ()
  | V -> Format.pp_open_vbox ppf indent
  | HV -> Format.pp_open_hvbox ppf indent
  | HoV -> Format.pp_open_hovbox ppf indent
  | B -> Format.pp_open_box ppf indent

let interpret_elt ppf = function
  | Data x -> Format.pp_print_string ppf x
  | Open_box { kind; indent } -> format_open_box_gen ppf kind indent
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

let format ppf doc = interpret ppf (to_list doc)

module Immutable = struct

  let open_box kind indent doc = add (Open_box {kind;indent}) doc
  let close_box doc = add Close_box doc

  let string s doc = add (Data s) doc
  let bytes b doc = add (Data (Bytes.to_string b)) doc
  let with_size n doc = add (With_size n) doc

  let int n doc = add (Data (string_of_int n)) doc
  let float f doc = add (Data (string_of_float f)) doc
  let char c doc = add (Data (String.make 1 c)) doc
  let bool c doc = add (Data (Bool.to_string c)) doc

  let break ~spaces ~indent doc = add (Simple_break {spaces; indent}) doc
  let space doc = break ~spaces:1 ~indent:0 doc
  let cut = break ~spaces:0 ~indent:0

  let custom_break ~fits ~breaks doc = add (Break {fits;breaks}) doc

  let force_newline doc = add Newline doc
  let if_newline doc = add If_newline doc

  let flush doc = add (Flush {newline=false}) doc
  let force_stop = add (Flush {newline=true})

  let open_tbox doc = add Open_tbox doc
  let set_tab doc = add Set_tab doc
  let tab_break ~width ~offset doc = add (Tab_break {width;offset}) doc
  let tab doc = tab_break ~width:0 ~offset:0 doc
  let close_tbox doc = add Close_tbox doc

  let open_tag stag doc = add (Open_tag stag) doc
  let close_tag doc = add Close_tag doc

  let iter ?(sep=Fun.id) ~iter:iterator elt l doc =
    let first = ref false in
    let rdoc = ref doc in
    let print x =
      if !first then (first := false; rdoc := elt x !rdoc)
      else rdoc := !rdoc |> sep |> elt x
    in
    iterator print l;
    !rdoc

  let rec list ?(sep=Fun.id) elt l doc = match l with
    | [] -> doc
    | [a] -> elt a doc
    | a :: (_ :: _ as q) ->
        doc |> elt a |> sep |> list ~sep elt q

  let array ?sep elt a doc = iter ?sep ~iter:Array.iter elt a doc
  let seq ?sep elt s doc = iter ?sep ~iter:Seq.iter elt s doc

  let option ?(none=Fun.id) elt o doc = match o with
    | None -> none doc
    | Some x -> elt x doc

  let either ~left ~right x doc = match x with
    | Either.Left x -> left x doc
    | Either.Right x -> right x doc

  let result ~ok ~error x doc = match x with
    | Ok x -> ok x doc
    | Error x -> error x doc

  (* To format free-flowing text *)
  let rec subtext len left right s doc =
    let flush doc =
      doc |> string (String.sub s left (right - left))
    in
    let after_flush doc = subtext len (right+1) (right+1) s doc in
    if right = len then
      if left <> len then flush doc else doc
    else
      match s.[right] with
      | '\n' ->
          doc |> flush |> force_newline |> after_flush
      | ' ' ->
          doc |> flush |> space |> after_flush
      (* there is no specific support for '\t'
         as it is unclear what a right semantics would be *)
      | _ -> subtext len left (right + 1) s doc

  let text s doc =
    subtext (String.length s) 0 0 s doc

  type ('a,'b) fmt = ('a, doc, doc, 'b) format4
  type printer0 = doc -> doc
  type 'a printer = 'a -> printer0

  let output_formatting_lit fmting_lit doc =
    let open CamlinternalFormatBasics in
    match fmting_lit with
    | Close_box    -> close_box doc
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

  let box_type =
    let open CamlinternalFormatBasics in
    function
    | Pp_fits -> H
    | Pp_hbox -> H
    | Pp_vbox -> V
    | Pp_hovbox -> HoV
    | Pp_hvbox -> HV
    | Pp_box -> B

  let rec compose_acc acc doc =
    let open CamlinternalFormat in
    match acc with
    | CamlinternalFormat.Acc_formatting_lit (p, f) ->
        doc |> compose_acc p |> output_formatting_lit f
    | Acc_formatting_gen (p, Acc_open_tag acc') ->
        let tag = to_string (compose_acc acc' empty) in
        let doc = compose_acc p doc in
        doc |> open_tag (Format.String_tag tag)
    | Acc_formatting_gen (p, Acc_open_box acc') ->
        let doc = compose_acc p doc in
        let box = to_string (compose_acc acc' empty) in
        let (indent, bty) = CamlinternalFormat.open_box_of_string box in
        doc |> open_box (box_type bty) indent
    | Acc_string_literal (p, s)
    | Acc_data_string (p, s)   ->
        doc |> compose_acc p |> string s
    | Acc_char_literal (p, c)
    | Acc_data_char (p, c)     -> doc |> compose_acc p |> char c
    | Acc_delay (p, f)         -> doc |> compose_acc p |> f
    | Acc_flush p              -> doc |> compose_acc p |> flush
    | Acc_invalid_arg (_p, msg) ->  invalid_arg msg;
    | End_of_acc               -> doc

  let kprintf k (CamlinternalFormatBasics.Format (fmt, _))  =
    CamlinternalFormat.make_printf
      (fun acc doc -> doc |> compose_acc acc |> k )
      End_of_acc fmt

  let printf doc = kprintf Fun.id doc
  let kmsg k  (CamlinternalFormatBasics.Format (fmt, _)) =
    CamlinternalFormat.make_printf
      (fun acc -> k (compose_acc acc empty))
      End_of_acc fmt

  let msg fmt = kmsg Fun.id fmt

end


module Ref = struct
  type ('a,'b) fmt = ('a, doc ref, unit,'b) format4

  open Immutable
  let rec update_acc doc (acc: (doc ref,unit) CamlinternalFormat.acc)  : unit =
    match acc with
    | Acc_formatting_lit (p, f) ->
        update_acc doc p;
        doc := output_formatting_lit f !doc
    | Acc_formatting_gen (p, Acc_open_tag acc') ->
        let tag_doc = ref empty in
        update_acc doc p;
        update_acc tag_doc acc';
        doc := open_tag (Format.String_tag (to_string !tag_doc)) !doc
    | Acc_formatting_gen (p, Acc_open_box acc') ->
        update_acc doc p;
        let tag_doc = ref empty in
        update_acc tag_doc acc';
        let box = to_string !tag_doc in
        let (indent, bty) = CamlinternalFormat.open_box_of_string box in
        doc := open_box (box_type bty) indent !doc
    | Acc_string_literal (p, s)
    | Acc_data_string (p, s)   ->
        update_acc doc p;
        doc := string s !doc
    | Acc_char_literal (p, c)
    | Acc_data_char (p, c)     -> update_acc doc p; doc := char c !doc
    | Acc_delay (p, f)         -> update_acc doc p; f doc
    | Acc_flush p              -> update_acc doc p;  doc :=  flush !doc
    | Acc_invalid_arg (p, msg) -> update_acc doc p; invalid_arg msg
    | End_of_acc               -> ()

  let kprintf k rdoc (CamlinternalFormatBasics.Format (fmt, _))  =
    CamlinternalFormat.make_printf
      (fun acc -> update_acc rdoc acc; k rdoc)
      End_of_acc fmt
  let printf doc fmt = kprintf ignore doc fmt
end


(** Compatibility interface *)

type rdoc = doc ref
type formatter =
  | Format of Format.formatter
  | Doc of rdoc

type 'a printer = formatter -> 'a -> unit


let make_formatter fmt  = Format fmt
let formatter_of_out_channel chan =
  make_formatter (Format.formatter_of_out_channel chan)
let make_doc d = Doc d

let doc = function
  | Doc { contents = rd } -> Some rd
  | Format  _ -> None

let formatter = function
  | Doc _ -> None
  | Format ppf -> Some ppf

(** {1 Primitive functions }*)


let pp_print_string ppf s = match ppf with
  | Format ppf -> Format.pp_print_string ppf s
  | Doc rdoc -> rdoc := Immutable.string s !rdoc

let pp_print_as ppf size s = match ppf with
  | Format ppf -> Format.pp_print_as ppf size s
  | Doc rdoc -> rdoc := !rdoc |> Immutable.with_size size |> Immutable.string s

let pp_print_substring ~pos ~len ppf s = match ppf with
  | Format ppf -> Format.pp_print_substring ~pos ~len ppf s
  | Doc rdoc -> rdoc := Immutable.string (String.sub s pos len) !rdoc

let pp_print_substring_as ~pos ~len ppf size s = match ppf with
  | Format ppf -> Format.pp_print_substring_as ~pos ~len ppf size s
  | Doc rdoc -> rdoc :=
        !rdoc
        |> Immutable.with_size size
        |> Immutable.string (String.sub s pos len)

let pp_print_bytes ppf s = match ppf with
  | Format ppf -> Format.pp_print_bytes ppf s
  | Doc rdoc -> rdoc := Immutable.string (Bytes.to_string s) !rdoc

let pp_print_text ppf s = match ppf with
  | Format ppf -> Format.pp_print_text ppf s
  | Doc rdoc -> rdoc := Immutable.text s !rdoc

let pp_print_char ppf c = match ppf with
  | Format ppf -> Format.pp_print_char ppf c
  | Doc rdoc -> rdoc := Immutable.char c !rdoc

let pp_print_int ppf c = match ppf with
  | Format ppf -> Format.pp_print_int ppf c
  | Doc rdoc -> rdoc := Immutable.int c !rdoc

let pp_print_float ppf f = match ppf with
  | Format ppf -> Format.pp_print_float ppf f
  | Doc rdoc -> rdoc := Immutable.float f !rdoc

let pp_print_bool ppf b = match ppf with
  | Format ppf -> Format.pp_print_bool ppf b
  | Doc rdoc -> rdoc := Immutable.bool b !rdoc

let pp_print_nothing _ _ = ()

let pp_close_box ppf () = match ppf with
  | Format ppf -> Format.pp_close_box ppf ()
  | Doc rdoc -> rdoc := Immutable.close_box !rdoc

let pp_close_stag ppf () = match ppf with
  | Format ppf -> Format.pp_close_stag ppf ()
  | Doc rdoc -> rdoc := Immutable.close_tag !rdoc

let pp_print_break ppf spaces indent = match ppf with
  | Format ppf -> Format.pp_print_break ppf spaces indent
  | Doc rdoc -> rdoc := Immutable.break ~spaces ~indent !rdoc

let pp_print_custom_break ppf ~fits ~breaks = match ppf with
  | Format ppf -> Format.pp_print_custom_break ~fits ~breaks ppf
  | Doc rdoc -> rdoc := Immutable.custom_break ~fits ~breaks !rdoc


let pp_print_space ppf () = pp_print_break ppf 1 0
let pp_print_cut ppf () = pp_print_break ppf 0 0


let pp_print_flush ppf () = match ppf with
  | Format ppf -> Format.pp_print_flush ppf ()
  | Doc rdoc -> rdoc := Immutable.flush !rdoc

let pp_force_newline ppf () = match ppf with
  | Format ppf -> Format.pp_force_newline ppf ()
  | Doc rdoc -> rdoc := Immutable.force_newline !rdoc

let pp_print_newline ppf () = match ppf with
  | Format ppf -> Format.pp_print_newline ppf ()
  | Doc rdoc -> rdoc := Immutable.force_stop !rdoc

let pp_print_if_newline ppf () = match ppf with
  | Format ppf -> Format.pp_print_if_newline ppf ()
  | Doc rdoc -> rdoc := Immutable.if_newline !rdoc

let pp_open_stag ppf stag = match ppf with
  | Format ppf -> Format.pp_open_stag ppf stag
  | Doc rdoc -> rdoc := !rdoc |> Immutable.open_tag stag

let pp_open_box_gen ppf indent bxty =
  let box_type = Immutable.box_type bxty in
  match ppf with
  | Format ppf -> format_open_box_gen ppf box_type indent
  | Doc rdoc -> rdoc := !rdoc |> Immutable.open_box box_type indent

let pp_open_box ppf indent = pp_open_box_gen ppf indent Pp_box


let pp_open_tbox ppf () =
  match ppf with
  | Format ppf -> Format.pp_open_tbox ppf ()
  | Doc rdoc -> rdoc := !rdoc |> Immutable.open_tbox

let pp_close_tbox ppf () =
  match ppf with
  | Format ppf -> Format.pp_close_tbox ppf ()
  | Doc rdoc -> rdoc := !rdoc |> Immutable.close_tbox

let pp_set_tab ppf () =
  match ppf with
  | Format ppf -> Format.pp_set_tab ppf ()
  | Doc rdoc -> rdoc := !rdoc |> Immutable.set_tab

let pp_print_tab ppf () =
  match ppf with
  | Format ppf -> Format.pp_print_tab ppf ()
  | Doc rdoc -> rdoc := !rdoc |> Immutable.tab

let pp_print_tbreak ppf width offset =
  match ppf with
  | Format ppf -> Format.pp_print_tbreak ppf width offset
  | Doc rdoc -> rdoc := !rdoc |> Immutable.tab_break ~width ~offset


let pp_doc ppf doc = match ppf with
  | Format ppf -> format ppf doc
  | Doc rdoc -> rdoc := append !rdoc doc

module Driver = struct
  (* Interpret a formatting entity on a formatter. *)
  let output_formatting_lit ppf
      (fmting_lit:CamlinternalFormatBasics.formatting_lit)
    = match fmting_lit with
    | Close_box                 -> pp_close_box ppf ()
    | Close_tag                 -> pp_close_stag ppf ()
    | Break (_, width, offset)  -> pp_print_break ppf width offset
    | FFlush                    -> pp_print_flush ppf ()
    | Force_newline             -> pp_force_newline ppf ()
    | Flush_newline             -> pp_print_newline ppf ()
    | Magic_size (_, _)         -> ()
    | Escaped_at                -> pp_print_char ppf '@'
    | Escaped_percent           -> pp_print_char ppf '%'
    | Scan_indic c              -> pp_print_char ppf '@'; pp_print_char ppf c



  let compute_tag output tag_acc =
    let buf = Buffer.create 16 in
    let buf_fmt = Format.formatter_of_buffer buf in
    let ppf = Format buf_fmt in
    output ppf tag_acc;
    pp_print_flush ppf ();
    let len = Buffer.length buf in
    if len < 2 then Buffer.contents buf
    else Buffer.sub buf 1 (len - 2)

  (* Recursively output an "accumulator" containing a reversed list of
     printing entities (string, char, flus, ...) in an output_stream. *)
  (* Differ from Printf.output_acc by the interpretation of formatting. *)
  (* Used as a continuation of CamlinternalFormat.make_printf. *)
  let rec output_acc ppf (acc: _ CamlinternalFormat.acc) =
    match acc with
    | Acc_string_literal (Acc_formatting_lit (p, Magic_size (_, size)), s)
    | Acc_data_string (Acc_formatting_lit (p, Magic_size (_, size)), s) ->
        output_acc ppf p;
        pp_print_as ppf size s;
    | Acc_char_literal (Acc_formatting_lit (p, Magic_size (_, size)), c)
    | Acc_data_char (Acc_formatting_lit (p, Magic_size (_, size)), c) ->
        output_acc ppf p;
        pp_print_as ppf size (String.make 1 c);
    | Acc_formatting_lit (p, f) ->
        output_acc ppf p;
        output_formatting_lit ppf f;
    | Acc_formatting_gen (p, Acc_open_tag acc') ->
        output_acc ppf p;
        pp_open_stag ppf (Format.String_tag (compute_tag output_acc acc'))
    | Acc_formatting_gen (p, Acc_open_box acc') ->
        output_acc ppf p;
        let (indent, bty) =
          let box_info = compute_tag output_acc acc' in
          CamlinternalFormat.open_box_of_string box_info
        in
        pp_open_box_gen ppf indent bty
    | Acc_string_literal (p, s)
    | Acc_data_string (p, s)   -> output_acc ppf p; pp_print_string ppf s;
    | Acc_char_literal (p, c)
    | Acc_data_char (p, c)     -> output_acc ppf p; pp_print_char ppf c;
    | Acc_delay (p, f)         -> output_acc ppf p; f ppf;
    | Acc_flush p              -> output_acc ppf p; pp_print_flush ppf ();
    | Acc_invalid_arg (p, msg) -> output_acc ppf p; invalid_arg msg;
    | End_of_acc               -> ()
end

let kfprintf k ppf (CamlinternalFormatBasics.Format (fmt, _))  =
  CamlinternalFormat.make_printf
    (fun acc -> Driver.output_acc ppf acc; k ppf)
    End_of_acc fmt
let fprintf doc fmt = kfprintf ignore doc fmt

let kasprintf k fmt =
  let b = Buffer.create 20 in
  let ppf = make_formatter (Format.formatter_of_buffer b) in
  kfprintf
    (fun ppf -> pp_print_flush ppf ();
      let r = Buffer.contents b in
      Buffer.reset b;
      k r)
    ppf fmt

let asprintf fmt = kasprintf Fun.id fmt

let kdprintf k (CamlinternalFormatBasics.Format (fmt, _)) =
  CamlinternalFormat.make_printf
    (fun acc -> k (fun ppf -> Driver.output_acc ppf acc))
    End_of_acc fmt

let dprintf fmt = kdprintf (fun i -> i) fmt

let doc_printf fmt =
  let doc = ref empty in
  kfprintf (fun _ -> !doc) (make_doc doc) fmt
let kdoc_printf k fmt =
  let doc = ref empty in
  kfprintf (fun _ppf -> k @@ !doc) (make_doc doc) fmt


let doc_printer f x doc =
  let r = ref doc in
  f (Doc r) x;
  !r
let format_printer f ppf x = f (make_formatter ppf) x
let compat = format_printer


let pp_print_iter ?(pp_sep=pp_print_cut) iter elt ppf c = match ppf with
  | Format ppf ->
      Format.pp_print_iter ~pp_sep:(compat pp_sep) iter (compat elt) ppf c
  | Doc rdoc ->
      let sep = doc_printer pp_sep () in
      rdoc:= Immutable.iter ~sep ~iter (doc_printer elt) c !rdoc

let pp_print_list ?(pp_sep=pp_print_cut) elt ppf l = match ppf with
  | Format ppf ->
      Format.pp_print_list ~pp_sep:(compat pp_sep) (compat elt) ppf l
  | Doc rdoc ->
      rdoc :=
        Immutable.list ~sep:(doc_printer pp_sep ()) (doc_printer elt) l !rdoc

let pp_print_array ?pp_sep elt ppf a =
  pp_print_iter ?pp_sep Array.iter elt ppf a
let pp_print_seq ?pp_sep elt ppf s = pp_print_iter ?pp_sep Seq.iter elt ppf s

let pp_print_option  ?(none=fun _ () -> ()) elt ppf o = match ppf with
  | Format ppf ->
      Format.pp_print_option ~none:(compat none) (compat elt) ppf o
  | Doc rdoc ->
      rdoc :=
        Immutable.option
          ~none:(doc_printer none ())
          (doc_printer elt) o !rdoc

let pp_print_result  ~ok ~error ppf r = match ppf with
  | Format ppf ->
      Format.pp_print_result ~ok:(compat ok) ~error:(compat error) ppf r
  | Doc rdoc ->
      rdoc :=
        Immutable.result ~ok:(doc_printer ok) ~error:(doc_printer error)
          r !rdoc

let pp_print_either  ~left ~right ppf e = match ppf with
  | Format ppf ->
      Format.pp_print_either ~left:(compat left) ~right:(compat right) ppf e
  | Doc rdoc ->
      rdoc :=
        Immutable.either
          ~left:(doc_printer left) ~right:(doc_printer right)
          e !rdoc


let comma ppf () = fprintf ppf ",@ "

let pp_two_columns ?(sep = "|") ?max_lines ppf (lines: (string * string) list) =
  let left_column_size =
    List.fold_left (fun acc (s, _) -> Int.max acc (String.length s)) 0 lines in
  let lines_nb = List.length lines in
  let ellipsed_first, ellipsed_last =
    match max_lines with
    | Some max_lines when lines_nb > max_lines ->
        let printed_lines = max_lines - 1 in (* the ellipsis uses one line *)
        let lines_before = printed_lines / 2 + printed_lines mod 2 in
        let lines_after = printed_lines / 2 in
        (lines_before, lines_nb - lines_after - 1)
    | _ -> (-1, -1)
  in
  fprintf ppf "@[<v>";
  List.iteri (fun k (line_l, line_r) ->
      if k = ellipsed_first then fprintf ppf "...@,";
      if ellipsed_first <= k && k <= ellipsed_last then ()
      else fprintf ppf "%*s %s %s@," left_column_size line_l sep line_r
    ) lines;
  fprintf ppf "@]"

let approx pr ppf = match ppf with
  | Format ppf -> pr ppf
  | Doc _ ->
      let s = Format.asprintf "%t" pr in
      pp_print_string ppf s
