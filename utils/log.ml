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
module R = D.Record_introspection
module V = Diagnostic_validation

type ('id,'a) field = ('id,'a) Diagnostic.field
type version = Diagnostic_history.version = { major:int; minor:int }

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

type redirections = {
  mutable map: (device option * redirections) Label_map.t
}
let empty_redirections () = { map = Label_map.empty }

let redirection key r =
  match Label_map.find_opt key r.map with
  | None ->
      let child = empty_redirections () in
      r.map <- Label_map.add key (None,child) r.map;
      None, child
  | Some(d,x) -> d, x

let device_redirection key r =
  match Label_map.find_opt key r.map with
  | None -> None
  | Some(d,_) -> d

let iter_redirection f r =
  Label_map.iter (fun _ (x,_) -> Option.iter f x) r.map

type 'a log =
  {
      redirections: redirections;
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
    if structured then
      Store {data=Diagnostic.empty (); out= Some out}
    else Direct out
  in
  {
    redirections = empty_redirections ();
    settings;
    version;
    printer;
    mode;
    scheme;
  }

let redirect log field device  =
  let r = log.redirections in
  let new_redirection = Some device, empty_redirections () in
  r.map <-Label_map.add (Diagnostic.field_name field) new_redirection r.map

let generic_detach label_scheme ~set ~lift ~extract log
    (field: _ Diagnostic.field) =
  let out, redirections = redirection (D.field_name field) log.redirections in
  let mode = match log.mode with
    | Direct d ->
        let out = Option.value ~default:d out in
        Direct out
    | Store {data=st; out=st_out} ->
        let data =
          match Option.bind (R.get st field) extract with
          | Some data -> data
          | None ->
              let data = D.empty () in
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
      redirections;
    } in
  child

let some x = Some x
let detach log field =
  generic_detach D.record_scheme
    ~set:R.set ~lift:Fun.id ~extract:some log field
let detach_item log field =
  generic_detach D.record_list_scheme
    ~set:R.cons
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
  | Store st -> R.set st.data (V.exact_version version) ~field x
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
          let r = device_redirection (D.field_name field) log.redirections in
          let out = Option.value ~default:d r in
          let ppf = !(out.ppf) in
          if not !(d.initialized) then
            (Fmt.init log.settings out.ppf ; d.initialized := true);
          Format.fprintf ppf "@[<v>%a@,@]%!"
            log.printer.item (D.field_name field, D.V(D.field_type field,x))

let cons field x log =
  match log.mode with
  | Direct _-> set field [x] log
  | Store st -> R.cons st.data (V.exact_version log.version) ~field x

let (.%[]<-) log field x = set field x log

let get field log = match log.mode with
  | Direct _ -> None
  | Store st -> R.get st.data field

let dynamic_get field log = match log.mode with
  | Direct _ -> None
  | Store st -> R.dynamic_get st.data field

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
        R.reset st.data
  end;
  iter_redirection Fmt.flush log.redirections

let separate log = match log.mode with
  | Direct d -> Fmt.separate d
  | _ -> ()

let close: type a. a log -> unit = fun log ->
  match log.mode with
  | Direct d ->
      Fmt.close d; iter_redirection Fmt.close log.redirections
  | Store { out;_ } ->
      let close x = x.on_close () in
      Option.iter close out;
      iter_redirection close log.redirections


let close log = flush log; close log

let replay source dest =
  match source.mode with
  | Direct _ -> ()
  | Store st ->
      Seq.iter
        (fun (D.F(field,x)) -> dest.%[field] <- x )
        (R.all_fields st.data)

(** {1:log_publication }*)

let tmp scheme =
  {
  settings = None;
  redirections = { map = Label_map.empty };
  version=(Downward_compatible {major=0;minor=0});
  scheme;
  printer = { record = (fun _ _ -> ()); item = (fun _ _ -> ()) };
  mode = Store { out=None; data=D.empty () }
}



let log_if dlog field flag printer x =
  if flag then
    Format.kasprintf (fun s -> dlog.%[field] <- s) "%a" printer x
