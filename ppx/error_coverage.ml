open Ast_mapper
open Parsetree
open Longident
open Asttypes

[@@@ocaml.warning "@A-4-48"]

let noloc = Location.mknoloc
let unit = Ast_helper.Exp.ident @@ noloc @@ Lident "()"

let report_file_in =
  match Sys.getenv_opt ("OCAML_SELFDIAGNOSTIC_IN") with
  | Some x -> x
  | None -> "/tmp/ocaml-report"

let report_file_out =
  match Sys.getenv_opt ("OCAML_SELFDIAGNOSTIC_OUT") with
  | None -> "/tmp/ocaml-report"
  | Some x -> x

let print_loc ppf loc =
  let start =  loc.Location.loc_start in
  let stop = loc.Location.loc_end in
  let open Lexing in
  let file = start.pos_fname in
  if start.pos_lnum = stop.pos_lnum then
    Format.fprintf ppf "%s:%d" file start.pos_lnum

let report_loc loc =
  try
    let module S = Set.Make(String) in
    let module M = Map.Make(String) in
    let read_file () =
      let f =
        try open_in_gen [Open_creat;Open_text] 0o700 report_file_in with _ ->
        Format.eprintf "Report: opening file failure@."; exit 2 in
      let rec loop prev map =
        match input_line f with
        | exception End_of_file -> begin
            match prev with
            | None -> map
            | Some (key,s) -> M.add key s map
          end
        | "" -> loop prev map
        | s ->
            let kind, loc = s.[0], String.sub s 1 (String.length s - 1) in
            match kind, prev with
            | '+', None -> loop (Some(loc,S.empty)) map
            | '+', Some(ploc,pset) ->
                loop (Some(loc,S.empty)) (M.add ploc pset map)
            | ' ', Some(ploc,pset) ->
                loop (Some(ploc,S.add loc pset)) map
            | _ -> loop prev map in
      let r = loop None M.empty in
      close_in f;
      r in
    let map = read_file () in
    let loc = Format.asprintf "%a" print_loc loc in
    let map = match M.find_opt loc map with
      | None -> M.add loc S.empty map
      | Some _ -> map in
    let f = try open_out report_file_out with
      | exn ->
          Format.eprintf "Error while opening %s as out: %s@."
            report_file_out (Printexc.to_string exn);
          exit 2
    in
    let ppf = Format.formatter_of_out_channel f in
    M.iter (fun key set ->
        Format.fprintf ppf "+%s\n" key;
        S.iter (Format.fprintf ppf " %s\n") set
      ) map;
    Format.fprintf ppf "@.";
    close_out f
  with
  | exn -> Format.eprintf "Report error: %s@."
             (Printexc.to_string exn)
         ; exit 2

let insider ~key ~loc =
  Ast_helper.default_loc := key;
  let key = Format.asprintf "%a" print_loc key in
  let src =
    Format.asprintf
  {|
    let report_file_in =
      match Sys.getenv_opt "OCAML_SELFDIAGNOSTIC_IN" with
      | None -> %S
      | Some x -> x in
    let report_file_out =
      match Sys.getenv_opt "OCAML_SELFDIAGNOSTIC_OUT" with
      | None -> %S
      | Some x -> x in
    let read_file () =
      let f =
        try open_in_gen [Open_creat;Open_rdonly] 1 report_file_in with _ ->
        Format.eprintf "Report: opening file failure@@."; exit 2 in
      let rec loop prev map =
        match input_line f with
        | exception End_of_file -> begin
            match prev with
            | None -> map
            | Some (key,s) -> Misc.Stdlib.String.Map.add key s map
          end
        | "" -> loop prev map
        | s ->
            let kind, loc = s.[0], String.sub s 1 (String.length s - 1) in
            match kind, prev with
            | '+', None -> loop (Some(loc,Misc.Stdlib.String.Set.empty)) map
            | '+', Some(ploc,pset) ->
                loop
                  (Some(loc,Misc.Stdlib.String.Set.empty))
                  (Misc.Stdlib.String.Map.add ploc pset map)
            | ' ', Some(ploc,pset) ->
                loop (Some(ploc, Misc.Stdlib.String.Set.add loc pset)) map
            | _ -> loop prev map in
      let r = loop None Misc.Stdlib.String.Map.empty in
      close_in f;
      r in
    let map = read_file () in
    let file, line, _char = Location.get_pos_info (%s).Location.loc_start in
    let file = match List.rev (String.split_on_char '.' file) with
      | "corrected" :: q -> String.concat "." (List.rev q)
      | _ -> file in
    let file = if file = "" then "?" else file in
    let loc = Format.asprintf "%%s:%%d" file line in
    let key = {x|%s|x} in
    let map = match Misc.Stdlib.String.Map.find_opt key map with
      | None ->
         Misc.Stdlib.String.Map.add key
           (Misc.Stdlib.String.Set.singleton loc) map
      | Some set ->
         Misc.Stdlib.String.Map.add key
           (Misc.Stdlib.String.Set.add loc set) map in
    let f = open_out report_file_out in
    let ppf = Format.formatter_of_out_channel f in
    Misc.Stdlib.String.Map.iter (fun key set ->
        Format.fprintf ppf "+%%s\n" key;
        Misc.Stdlib.String.Set.iter (Format.fprintf ppf " %%s\n") set
      ) map;
    Format.fprintf ppf "@@.";
    close_out f
  |} report_file_in report_file_out loc key in
  try Parse.expression (Lexing.from_string src)
  with exn ->
    let printer ppf = match Location.error_of_exn exn with
    | Some `Ok e -> Location.print_report ppf e
    | _ -> () in
    Format.eprintf "@[<v>Error when parsing:@ %t@]@." printer;
    exit 2

let seq l = Ast_helper.Exp.sequence l

let super = Ast_mapper.default_mapper


let is_prefix x y =
  let plen, mlen = String.(length x, length y) in
  if plen > mlen then false
  else x = String.sub y 0 plen

let error_prefix = is_prefix "Error"

let rec finally_error = function
  | Lident x -> error_prefix x
  | Ldot(_,x) -> error_prefix x
  | Lapply(_,x) -> finally_error x

let expr mapper e =
  match e.pexp_desc with
  | Pexp_apply (
      { pexp_desc = Pexp_ident {txt=Lident "raise"; loc } ; _ },
      [Nolabel, arg]
    ) ->
    begin match arg.pexp_desc with
      | Pexp_construct ({txt;_},Some args) when finally_error txt ->
          report_loc loc;
          begin
            match args.pexp_desc  with
            | Pexp_tuple [eloc;_;_] ->
                let caller = Format.asprintf "%a" Pprintast.expression eloc in
                seq (insider ~key:loc ~loc:caller) e
            | _ -> seq
                     (insider ~key:loc ~loc:"Location.(in_file !input_name)")
                     e
          end
      | _ -> e
    end
  | _ -> super.expr mapper e

let () =
  Ast_mapper.register "error checker" (fun _ ->
      if List.mem !Location.input_name
          ["parsing/lexer.ml"; "parsing/location.ml"]
      then super
      else { super with expr } )
