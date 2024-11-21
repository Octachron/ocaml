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


module Label_map = Misc.Stdlib.String.Map

module H = Diagnostic_history
module D = Diagnostic
module V = Diagnostic_validation

type ('id,'a) field = ('id,'a) Diagnostic.field
type version = Diagnostic_history.version = { major:int; minor:int }
type 'a update = 'a Diagnostic_history.update

type device =
  {
    initialized: bool ref;
    ppf: Format.formatter ref;
    on_close: unit -> unit;
  }

let make_device ?(on_close=ignore) ppf =
  { initialized=ref false; ppf; on_close}
let err = make_device (ref Format.err_formatter)
let std = make_device (ref Format.std_formatter)
let ppf d = !(d.ppf)

let out_channel_device out =
  let on_close () =
    Out_channel.flush out;
    Out_channel.close out
  in
  let ppf = Format.formatter_of_out_channel out in
  make_device ~on_close (ref ppf)



type printer = {
  record: Format.formatter -> Diagnostic.typed_record -> unit;
  item: Format.formatter -> string * Diagnostic.typed_val -> unit
}

type 'a mode =
  | Direct of device
  | Store of {data:'a Diagnostic.record; out:device option}

type 'a log =
  {
      mutable redirections: device Label_map.t;
      version: Diagnostic_validation.version;
      scheme: 'a Diagnostic.t;
      settings: Misc.Color.setting option;
      mode: 'a mode;
      printer:printer;
  }




let log_scheme log = log.scheme
let log_version log = Diagnostic_validation.exact_version log.version

type 'a t = 'a log




(** {1:log_scheme_versionning  Current version of the log } *)


let make ~structured ~printer settings version scheme out =
  let mode =
    let version = Diagnostic_validation.exact_version version in
    if structured then
      Store {data=Diagnostic.Record.make version []; out= Some out}
    else Direct out
  in
  {
    redirections = Label_map.empty;
    settings;
    version;
    printer;
    mode;
    scheme;
  }

let redirect log field device  =
  log.redirections <-
    Label_map.add (Diagnostic.field_name field) device log.redirections

let generic_detach label_scheme ~set ~lift ~extract log
    (field: _ Diagnostic.field) =
  let out = Label_map.find_opt (D.field_name field) log.redirections in
  let mode = match log.mode with
    | Direct d ->
        let out = Option.value ~default:d out in
        Direct out
    | Store {data=st; out=st_out} ->
        let data =
          match Option.bind (D.Record.get st field) extract with
          | Some data -> data
          | None ->
              let data = D.Record.make None [] in
              set st (V.exact_version log.version) ~field (lift data); data
        in
        let out = match out with
          | Some _ -> out
          | _ -> st_out
        in Store { data; out }
  in
  let child =
    { scheme=label_scheme (D.field_type field);
      mode;
      printer=log.printer;
      version = log.version;
      settings = log.settings;
      redirections = Label_map.empty;
    } in
  child

let some x = Some x
let detach log field =
  generic_detach D.record_scheme
    ~set:D.Record.set ~lift:Fun.id ~extract:some log field
let detach_item log field =
  generic_detach D.record_list_scheme
    ~set:D.Record.cons
    ~lift:Fun.id
    ~extract:(Fun.const None)
    log field

module Fmt = struct
   let init color ppf =
    let color = Misc.Style.enable_color color in
    Misc.Style.set_tag_handling ~color !ppf;
    Format.fprintf !ppf "@[<v>"

  let flush c =
    Format.fprintf !(c.ppf) "%!"

  let separate c = Format.pp_print_newline !(c.ppf) ()

  let close c =
    if not !(c.initialized) then ()
    else (Format.fprintf !(c.ppf) "@,@]%!"; c.initialized := false);
    c.on_close ()
end

(** *)
let set (field: _ D.field) x log =
  let version = log.version in
  match log.mode with
  | Store st -> D.Record.set st.data (V.exact_version version) ~field x
  | Direct d ->
      let status = match D.field_info log.scheme field with
        | Some lmd ->
          let v = V.reference_version version in
          H.stage_at (Some v) lmd.status
        | None -> Diagnostic_history.Lifetime.Deletion
      in
      match status with
      | Deletion | Future -> ()
      | Inception | Publication | Expansion | Deprecation ->
          let r = Label_map.find_opt (D.field_name field) log.redirections in
          let out = Option.value ~default:d r in
          let ppf = !(out.ppf) in
          if not !(d.initialized) then
            (Fmt.init log.settings out.ppf ; d.initialized := true);
          Format.fprintf ppf "@[<v>%a@,@]%!"
            log.printer.item (D.field_name field, D.V(D.field_type field,x))

let cons field x log =
  match log.mode with
  | Direct _-> set field [x] log
  | Store st -> D.Record.cons st.data (V.exact_version log.version) ~field x

let (.%[]<-) log field x = set field x log

let get field log = match log.mode with
  | Direct _ -> None
  | Store st -> D.Record.get st.data field

let dynamic_get field log = match log.mode with
  | Direct _ -> None
  | Store st -> D.Record.dynamic_get st.data field

let f field log fmt = Format.kasprintf (fun s -> log.%[field] <- s) fmt
let itemf field log fmt = Format.kasprintf (fun s -> cons field s log) fmt

let d field log fmt = Format_doc.kdoc_printf (fun s -> log.%[field] <- s) fmt
let itemd field log fmt = Format_doc.kdoc_printf (fun s -> cons field s log) fmt

let flush: type a. a log -> unit = fun log ->
  begin match log.mode with
  | Direct d -> Fmt.flush d
  | Store st ->
      let _ = V.diagnostic ~version:log.version log.scheme st.data in
      Option.iter (fun out ->
          let ppf = !(out.ppf) in
          log.printer.record ppf (R(log.scheme, st.data))
        ) st.out;
        D.Record.reset st.data
  end;
  Label_map.iter (fun _ -> Fmt.flush) log.redirections

let separate log = match log.mode with
  | Direct d -> Fmt.separate d
  | _ -> ()

let close: type a. a log -> unit = fun log ->
  begin match log.mode with
  | Direct d -> Fmt.close d
  | Store { out = Some out } -> out.on_close ()
  | Store _ -> ()
  end;
  Label_map.iter (fun _ -> Fmt.close) log.redirections

let close log = flush log; close log

let replay source dest =
  match source.mode with
  | Direct _ -> ()
  | Store st ->
      Seq.iter
        (fun (D.F(field,x)) -> dest.%[field] <- x )
        (D.Record.all_fields st.data)

(** {1:log_publication }*)

let tmp scheme =
  {
  settings = None;
  redirections = Label_map.empty;
  version=(Downward_compatible {major=0;minor=0});
  scheme;
  printer = { record = (fun _ _ -> ()); item = (fun _ _ -> ()) };
  mode = Store { out=None; data=D.Record.make None [] }
}



let log_if dlog field flag printer x =
  if flag then
    Format.kasprintf (fun s -> dlog.%[field] <- s) "%a" printer x
