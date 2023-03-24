module Error_report = Log.New_scheme ()


type location = Warnings.loc = {
  loc_start: Lexing.position;
  loc_end: Lexing.position;
  loc_ghost: bool;
}


type report_kind =
  | Report_error
  | Report_warning of string
  | Report_warning_as_error of string
  | Report_alert of string
  | Report_alert_as_error of string


type _ Log.extension +=
  | Report_kind: report_kind Log.extension
  | Main_location: location Log.extension
  | Sub_location: location Log.extension

let scheme = Error_report.scheme

let report_kind = function
  | Report_error -> "error",  ""
  | Report_warning s -> "warning", s
  | Report_alert s -> "alert", s
  | Report_warning_as_error s -> "warning_as_error", s
  | Report_alert_as_error s -> "alert_as_error", s


let kind = Log.new_key ~path:["kind"] scheme
    Log.(
      Custom { default = Pair (String, String); pull = report_kind; id = Report_kind }
    )


let input_name = ref "_none_"
let input_lexbuf = ref (None : Lexing.lexbuf option)
let input_phrase_buffer = ref (None : Buffer.t option)
let status = ref Terminfo.Uninitialised
let setup_colors () =
  Misc.Color.setup !Clflags.color


(* Best-effort printing of the text describing a location, of the form
   'File "foo.ml", line 3, characters 10-12'.

   Some of the information (filename, line number or characters numbers) in the
   location might be invalid; in which case we do not print it.
*)

type loc_summary = {
  file: string option;
  lines: int option * int option;
  chars: (int * int) option
}

let loc_typ =
  let open Log in
  Triple (Option Int,
          Pair (Option Int, Option Int),
          Option (Pair (Int, Int))
         )


let pull_loc l =
  l.file, l.lines, l.chars

let main_loc =
  let kind = Log.(Custom {
      id = main_Loc;
      pull = (fun l -> l.file, l.lines, l.chars);
      default = loc_typ
    })
  in
  Log.new_key ~path:["loc"] scheme kind


let loc_summary loc =
  let line_valid line = if line > 0 then Some line else None in
  let chars_valid ~startchar ~endchar =
    if startchar <> -1 && endchar <> -1 then Some (startchar, endchar)
    else None
  in
  let file =
    (* According to the comment in location.mli, if [pos_fname] is "", we must
       use [!input_name]. *)
    if loc.loc_start.Lexing.pos_fname = "" then !input_name
    else loc.loc_start.pos_fname
  in
  let file = match file with
    | "_none_" ->
        (* This is a dummy placeholder, but we print it anyway to please editors
           that parse locations in error messages (e.g. Emacs). *)
        Some file
    | "" | "//toplevel//" -> None
    | _ -> Some file
  in

  let chars = chars_valid
      ~startchar:(loc.loc_start.pos_cnum - loc.loc_start.pos_bol)
      ~endchar:(loc.loc_end.pos_cnum - loc.loc_end.pos_bol)
  in
  {
    file;
    lines = line_valid loc.loc_start.pos_lnum,
            line_valid loc.loc_end.pos_lnum;
    chars;
  }



(******************************************************************************)
(* Printing locations, e.g. 'File "foo.ml", line 3, characters 10-12' *)

let rewrite_absolute_path path =
  match Misc.get_build_path_prefix_map () with
  | None -> path
  | Some map -> Build_path_prefix_map.rewrite map path



let absolute_path s = (* This function could go into Filename *)
  let open Filename in
  let s =
    if not (is_relative s) then s
    else (rewrite_absolute_path (concat (Sys.getcwd ()) s))
  in
  (* Now simplify . and .. components *)
  let rec aux s =
    let base = basename s in
    let dir = dirname s in
    if dir = s then dir
    else if base = current_dir_name then aux dir
    else if base = parent_dir_name then dirname (aux dir)
    else concat (aux dir) base
  in
  aux s

let show_filename file =
  if !Clflags.absname then absolute_path file else file

let print_filename ppf file =
  Format.pp_print_string ppf (show_filename file)


(* Best-effort printing of the text describing a location, of the form
   'File "foo.ml", line 3, characters 10-12'.

   Some of the information (filename, line number or characters numbers) in the
   location might be invalid; in which case we do not print it.
 *)
let print_loc ppf loc =
  let loc = loc_summary loc in
  let first = ref true in
  let capitalize s =
    if !first then (first := false; String.capitalize_ascii s)
    else s in
  let comma () =
    if !first then () else Format.fprintf ppf ", " in
  Format.fprintf ppf "@{<loc>";
  Option.iter
    (Format.fprintf ppf "%s \"%a\"" (capitalize "file") print_filename)
    loc.file
  ;
  (* Print "line 1" in the case of a dummy line number. This is to please the
     existing setup of editors that parse locations in error messages (e.g.
     Emacs). *)
  comma ();
  let startline, endline = loc.lines in
  let startline = Option.value ~default:1 startline in
  let endline = Option.value ~default:startline endline in
  begin if startline = endline then
    Format.fprintf ppf "%s %i" (capitalize "line") startline
  else
    Format.fprintf ppf "%s %i-%i" (capitalize "lines") startline endline
  end;

  Option.iter (fun (startchar,endchar) ->
    comma ();
    Format.fprintf ppf "%s %i-%i" (capitalize "characters") startchar endchar
  ) loc.chars;

  Format.fprintf ppf "@}"


let (.%[]<-) = Log.(.%[]<-)
