(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Jeremie Dimino, Jane Street Europe                   *)
(*                                                                        *)
(*   Copyright 2016 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Execute a list of phrases from a .ml file and compare the result to the
   expected output, written inside [%%expect ...] nodes. At the end, create
   a .corrected file containing the corrected expectations. The test is
   successful if there is no differences between the two files.

   An [%%expect] node always contains both the expected outcome with and
   without -principal. When the two differ the expectation is written as
   follows:

   {[
     [%%expect {|
     output without -principal
     |}, Principal{|
     output with -principal
     |}]
   ]}
*)

[@@@ocaml.warning "-40"]

open StdLabels

(* representation of: {tag|str|tag} *)
type string_constant =
  { str : string
  ; tag : string
  }

module String_map = Misc.Stdlib.String.Map

type expectation =
  { extid_loc   : Location.t [@warning "-69"]
  (* Location of "expect" in "[%%expect ...]" *)
  ; payload_loc : Location.t (* Location of the whole payload *)
  ; main: string_constant
  ; variants : string_constant String_map.t
  }

let empty_expectation = {
  extid_loc = Location.none;
  payload_loc = Location.none;
  main = { str = ""; tag = "" };
  variants = String_map.empty
}

(* A list of phrases with the expected toplevel output *)
type chunk =
  { phrases     : Parsetree.toplevel_phrase list
  ; expectation : expectation
  }

type correction =
  { corrected_expectations : expectation list
  ; trailing_output        : expectation option
  }


let invalid_payload loc =
  Location.raise_errorf ~loc "invalid [%%%%expect payload]"

let string_constant ~loc (e : Parsetree.expression) =
  match e.pexp_desc with
  | Pexp_constant {pconst_desc = Pconst_string (str, _, Some tag); _} ->
      { str; tag }
  | _ -> invalid_payload loc

let match_qualified_string ~loc (_,x) = match x.Parsetree.pexp_desc with
  |  Pexp_construct ({ txt = Lident name; _ }, Some b) ->
      name, string_constant ~loc b
  | _ -> invalid_payload loc

let match_expect_extension (ext : Parsetree.extension) =
  match ext with
  | ({Asttypes.txt="expect"|"ocaml.expect"; loc }, payload) ->
    let expectation =
      match payload with
      | PStr [{ pstr_desc = Pstr_eval (e, []) }] ->
        let main, variants =
          match e.pexp_desc with
          | Pexp_tuple ((None, a) :: l) ->
              let bindings = List.map ~f:(match_qualified_string ~loc) l in
              string_constant ~loc a,
              String_map.of_list bindings
          | _ -> string_constant ~loc e, String_map.empty
        in
        { extid_loc = loc
        ; payload_loc = e.pexp_loc
        ; main
        ; variants
        }
      | PStr [] ->
        let s = { tag = ""; str = "" } in
        { extid_loc = loc
        ; payload_loc  = { loc with loc_start = loc.loc_end }
        ; main      = s
        ; variants = String_map.empty
        }
      | _ -> invalid_payload loc
    in
    Some expectation
  | _ ->
    None

(* Split a list of phrases from a .ml file *)
let split_chunks phrases =
  let rec loop (phrases : Parsetree.toplevel_phrase list) code_acc acc =
    match phrases with
    | [] ->
      if code_acc = [] then
        (List.rev acc, None)
      else
        (List.rev acc, Some (List.rev code_acc))
    | phrase :: phrases ->
      match phrase with
      | Ptop_def [] -> loop phrases code_acc acc
      | Ptop_def [{pstr_desc = Pstr_extension(ext, [])}] -> begin
          match match_expect_extension ext with
          | None -> loop phrases (phrase :: code_acc) acc
          | Some expectation ->
            let chunk =
              { phrases     = List.rev code_acc
              ; expectation
              }
            in
            loop phrases [] (chunk :: acc)
        end
      | _ -> loop phrases (phrase :: code_acc) acc
  in
  loop phrases [] []

module Compiler_messages = struct
  let capture ppf ~f =
    Misc.protect_refs
      [ R (Location.formatter_for_warnings, ppf) ]
      f
end

let collect_formatters buf pps ~f =
  let ppb = Format.formatter_of_buffer buf in
  let out_functions = Format.pp_get_formatter_out_functions ppb () in

  List.iter ~f:(fun pp -> Format.pp_print_flush pp ()) pps;
  let save =
    List.map ~f:(fun pp -> Format.pp_get_formatter_out_functions pp ()) pps
  in
  let restore () =
    List.iter2
      ~f:(fun pp out_functions ->
         Format.pp_print_flush pp ();
         Format.pp_set_formatter_out_functions pp out_functions)
      pps save
  in
  List.iter
    ~f:(fun pp -> Format.pp_set_formatter_out_functions pp out_functions)
    pps;
  match f () with
  | x             -> restore (); x
  | exception exn -> restore (); raise exn

(* Invariant: ppf = Format.formatter_of_buffer buf *)
let capture_everything buf ppf ~f =
  collect_formatters buf [Format.std_formatter; Format.err_formatter]
                     ~f:(fun () -> Compiler_messages.capture ppf ~f)

type mode = Main | Variant of string

module Exec = struct

  type ref_env = R: 'a ref * 'a -> ref_env
  let apply_flags (R (r,x)) = r:= x

  type state = {
    name: mode;
    flags: ref_env list;
    ppf: Format.formatter;
    buf: Buffer.t;
    env: Env.t;
  }[@@warning "-69"]

  let _debug_env state =
    Format.eprintf "@[env {@,";
    Env.fold_values (fun s _p _vd () -> Format.eprintf "%s,@ " s)
      None state.env ();
    Format.eprintf "@]@."

  let check_warnings x = Warnings.check_fatal (); x

  let typecheck ppf state sstr =
    List.iter ~f:apply_flags state.flags;
    let snap = Btype.snapshot () in
    match check_warnings (Topcommon.typecheck_phrase ppf state.env sstr) with
    | str, sg, env ->
        Ok (str, sg), { state with env }
    | exception exn ->
        let bt = Printexc.get_raw_backtrace () in
        begin try Location.report_exception ppf exn with
        |  _ ->
            Format.fprintf ppf "Uncaught exception: %s\n%s\n"
              (Printexc.to_string exn)
              (Printexc.raw_backtrace_to_string bt)
        end;
        Btype.backtrack snap;
        Error (), state

  let _select arr =
    Iarray.find_map (function (Ok x, _) -> Some x | _ -> None) arr,
    Iarray.map snd arr


  let value_bindings : Obj.t String_map.t ref = ref String_map.empty

  let dump_if ppf r printer x = if !r then Format.fprintf ppf "%a@." printer x

  (** Imported from Byte.Topeval *)
  let load_lambda ppf lam =
    dump_if ppf Clflags.dump_rawlambda Printlambda.lambda lam;
    let slam = Simplif.simplify_lambda lam in
    dump_if ppf Clflags.dump_lambda Printlambda.lambda slam;
    let instrs, can_free = Bytegen.compile_phrase slam in
    dump_if ppf Clflags.dump_instr Printinstr.instrlist instrs;
    let (code, reloc, events) = Emitcode.to_memory instrs in
    let initial_symtable = Symtable.current_state() in
    Symtable.patch_object code reloc;
    Symtable.check_global_initialized reloc;
    Symtable.update_global_table();
    let initial_bindings = !value_bindings in
    let bytecode, closure = Meta.reify_bytecode code [| events |] None in
    match closure () with
    | retval ->
        if can_free then Meta.release_bytecode bytecode;
        Ok retval
    | exception x ->
        Topcommon.record_backtrace ();
        if can_free then Meta.release_bytecode bytecode;
        value_bindings := initial_bindings; (* PR#6211 *)
        Symtable.restore_state initial_symtable;
        Error x


  let exec_typed ppf str =
    let lam = Translmod.transl_toplevel_definition str in
    load_lambda ppf lam

  let next_state ~oldenv state = function
    | Ok _ -> Ok state
    | Error _ -> Error { state with env = oldenv }

  let print_outcome ppf ~oldenv ~newenv str sg r =
    let out_phr = match r with
      | Ok v -> Topeval.res_outcome ~rewritten:false ~oldenv ~newenv str sg v
      | Error exn -> Topeval.exn_outcome oldenv exn
    in
    begin match out_phr with
    | Ophr_signature [] -> ()
    | _ ->
        Location.separate_new_message ppf;
        !Oprint.out_phrase ppf out_phr;
    end;
    if Printexc.backtrace_status ()
    then begin
      match !Topcommon.backtrace with
      | None -> ()
      | Some b ->
          Location.separate_new_message ppf;
          Format.pp_print_string ppf b;
          Format.pp_print_flush ppf ();
          Topcommon.backtrace := None;
    end

let execute_phrase ppf phr state =
  match phr with
  | Parsetree.Ptop_dir {pdir_name = {Location.txt = dir_name}; pdir_arg } ->
      Toploop.toplevel_env := state.env;
      let ok = Topcommon.try_run_directive ppf dir_name pdir_arg in
      let state = { state with env = !Toploop.toplevel_env } in
      if ok then Ok state else Error state
  | Parsetree.Ptop_def sstr ->
      let oldenv = state.env in
      let ty, state = typecheck ppf state sstr in
      match ty with
      | Result.Error () -> Error state
      | Result.Ok (str, sg) ->
          let r = exec_typed ppf str in
          print_outcome ppf ~oldenv ~newenv:state.env str sg r;
          next_state ~oldenv state r

let execute_phrase ppf phr state =
  try execute_phrase ppf phr state
  with exn ->
    Warnings.reset_fatal ();
    raise exn

end


let exec_phrase ppf phrase =
  Location.reset ();
  if !Clflags.dump_parsetree then Printast. top_phrase ppf phrase;
  if !Clflags.dump_source    then Pprintast.top_phrase ppf phrase;
  Exec.execute_phrase ppf phrase

let exec_or_skip_phrase ppf state phrase =
  match phrase, state with
  | Parsetree.Ptop_def [], _ -> state
  | _, Error (state,i) -> Error (state,i+1)
  | _, Ok state ->
      match exec_phrase ppf phrase state with
      | Ok _ as x -> x
      | Error _  -> Error (state,0)
      | exception exn ->
          let bt = Printexc.get_raw_backtrace () in
          begin try Location.report_exception ppf exn
          with _ ->
            Format.fprintf ppf "Uncaught exception: %s\n%s\n"
              (Printexc.to_string exn)
              (Printexc.raw_backtrace_to_string bt)
          end;
          Error (state,0)


let parse_contents ~fname contents =
  let lexbuf = Lexing.from_string contents in
  Location.init lexbuf fname;
  Location.input_name := fname;
  Location.input_lexbuf := Some lexbuf;
  Parse.use_file lexbuf

let get_mode mode e = match mode with
  | Main -> e.main
  | Variant v ->
      match String_map.find_opt v e.variants with
      | None -> e.main
      | Some x -> x

let update_mode mode e x = match mode with
  | Main -> { e with main = x }
  | Variant v ->
      let variants = String_map.add v x e.variants in
      { e with variants }


type expectation_ref = {
  expect: expectation;
  altered: bool;
}

let eval_expectation mode reference ~output =
  let s = get_mode mode reference.expect in
  if s.str = output then
    reference
  else
    let s = { s with str = output } in
    {
      expect = update_mode mode reference.expect s;
      altered = true
    }

let shift_lines delta phrases =
  let position (pos : Lexing.position) =
    { pos with pos_lnum = pos.pos_lnum + delta }
  in
  let location _this (loc : Location.t) =
    { loc with
      loc_start = position loc.loc_start
    ; loc_end   = position loc.loc_end
    }
  in
  let mapper = { Ast_mapper.default_mapper with location } in
  List.map phrases ~f:(function
    | Parsetree.Ptop_dir _ as p -> p
    | Parsetree.Ptop_def st ->
      Parsetree.Ptop_def (mapper.structure mapper st))

let rec min_line_number : Parsetree.toplevel_phrase list -> int option =
function
  | [] -> None
  | (Ptop_dir _  | Ptop_def []) :: l -> min_line_number l
  | Ptop_def (st :: _) :: _ -> Some st.pstr_loc.loc_start.pos_lnum




let visible_inline_code () =
  let open Misc.Style in
  let default = get_styles () in
  let inline_code = { ansi = []; text_open = {|"|}; text_close={|"|} } in
  set_styles { default with inline_code }

let exec_phrases phrases ({ Exec.buf; ppf; _ } as state) =
  let phrases =
    match min_line_number phrases with
    | None -> phrases
    | Some lnum -> shift_lines (1 - lnum) phrases
  in
  (* For formatting purposes *)
  Buffer.add_char buf '\n';
  let skipped_phrases =
    List.fold_left phrases ~init:(Ok state) ~f:(exec_or_skip_phrase ppf)
  in
  Format.pp_print_flush ppf ();
  let len = Buffer.length buf in
  if len > 0 && Buffer.nth buf (len - 1) <> '\n' then
    (* For formatting purposes *)
    Buffer.add_char buf '\n';
  let state = match skipped_phrases with
    | Ok state | Error (state,0) -> state
    | Error (state,i) ->
        Format.fprintf ppf
          "Unexecuted phrases: %i phrases did not execute due to an error\n" i;
        state
  in
  Format.pp_print_flush ppf ();
  let s = Buffer.contents buf in
  Buffer.clear buf;
  state, Misc.delete_eol_spaces s

let update_correction phrases (states, correction) (state:Exec.state) =
  let state, output =
    capture_everything ~f:(fun () -> exec_phrases phrases state)
      state.buf state.ppf
  in
  state :: states , eval_expectation state.name correction ~output

let eval_correction chunk states corrections =
  let r = { expect = chunk.expectation; altered = false } in
  let states, correction =
    List.fold_left ~f:(update_correction chunk.phrases) ~init:([],r) states
  in
  List.rev states,
  if correction.altered then corrections else correction.expect :: corrections

let main_mode =
  Main,
  Exec.[ R (Clflags.principal, false); R (Clflags.recursive_types, false) ]
let principal_mode =
  Variant "Principal",
  Exec.[ R (Clflags.principal, true); R (Clflags.recursive_types, false) ]
let rectype_modes =
  Variant "Rectypes",
  Exec.[ R(Clflags.principal, false); R (Clflags.recursive_types, true) ]
let all_modes = [main_mode; principal_mode]


let init_state (mode, flags) =
  let buf = Buffer.create 1024 in
  let ppf = Format.formatter_of_buffer buf in
  visible_inline_code ();
  Misc.Style.set_tag_handling ppf;
  {
    Exec.name = mode;
    env = !Toploop.toplevel_env;
    flags;
    buf;
    ppf
  }


let eval_expect_file modes _fname ~file_contents =
  Warnings.reset_fatal ();
  let chunks, trailing_code =
    parse_contents ~fname:"" file_contents |> split_chunks
  in
  let init = List.map ~f:init_state modes, [] in
  let states, corrected_expectations =
    List.fold_left
      ~f:(fun (s,c) chunk -> eval_correction chunk s c)
      ~init
      chunks
  in
  let corrected_expectations = List.rev corrected_expectations in
  let trailing_output = match trailing_code with
    | None -> None
    | Some phrases ->
        let chunk = { phrases; expectation = empty_expectation } in
        match eval_correction chunk states [] with
        | _, a :: _ -> Some a
        | _, [] -> None
  in
  { corrected_expectations; trailing_output }

let output_slice oc s a b =
  output_string oc (String.sub s ~pos:a ~len:(b - a))

let output_correction oc c =
  let output_body oc { str; tag } =
    Printf.fprintf oc "{%s|%s|%s}" tag str tag
  in
  output_body oc c.main;
  String_map.iter (fun name v ->
      if c.main.str <> v.str then begin
        output_string oc ", ";
        output_string oc name;
        output_body oc v
      end
    ) c.variants

let output_corrected oc ~file_contents correction =
  let ofs =
    List.fold_left correction.corrected_expectations ~init:0
      ~f:(fun ofs c ->
        output_slice oc file_contents ofs c.payload_loc.loc_start.pos_cnum;
        output_correction oc c;
        c.payload_loc.loc_end.pos_cnum)
  in
  output_slice oc file_contents ofs (String.length file_contents);
  match correction.trailing_output with
  | None -> ()
  | Some c  -> Printf.fprintf oc "\n[%%%%expect{|%a|}]\n" output_correction c

let write_corrected ~file ~file_contents correction =
  let oc = open_out file in
  output_corrected oc ~file_contents correction;
  close_out oc

let process_expect_file fname =
  let corrected_fname = fname ^ ".corrected" in
  let file_contents =
    let ic = open_in_bin fname in
    match really_input_string ic (in_channel_length ic) with
    | s           -> close_in ic; Misc.normalise_eol s
    | exception e -> close_in ic; raise e
  in
  let correction = eval_expect_file all_modes fname ~file_contents in
  write_corrected ~file:corrected_fname ~file_contents correction

let repo_root = ref None
let keep_original_error_size = ref false

let main fname =
  if not !keep_original_error_size then
    Clflags.error_size := 0;
  Toploop.override_sys_argv
    (Array.sub Sys.argv ~pos:!Arg.current
       ~len:(Array.length Sys.argv - !Arg.current));
  (* Ignore OCAMLRUNPARAM=b to be reproducible *)
  Printexc.record_backtrace false;
  if not !Clflags.no_std_include then begin
    match !repo_root with
    | None -> ()
    | Some dir ->
        (* If we pass [-repo-root], use the stdlib from inside the
           compiler, not the installed one. We use
           [Compenv.last_include_dirs] to make sure that the stdlib
           directory is the last one. *)
        Clflags.no_std_include := true;
        Compenv.last_include_dirs := [Filename.concat dir "stdlib"]
  end;
  Compmisc.init_path ~auto_include:Load_path.no_auto_include ();
  Toploop.initialize_toplevel_env ();
  (* We are in interactive mode and should record directive error on stdout *)
  Sys.interactive := true;
  process_expect_file fname;
  exit 0

module Options = Main_args.Make_bytetop_options (struct
  include Main_args.Default.Topmain
  let _stdin () = (* disabled *) ()
  let _args = Arg.read_arg
  let _args0 = Arg.read_arg0
  let anonymous s = main s
end);;

let args =
  Arg.align
    ( [ "-repo-root", Arg.String (fun s -> repo_root := Some s),
        "<dir> root of the OCaml repository. This causes the tool to use \
         the stdlib from the current source tree rather than the installed one."
      ; "-keep-original-error-size", Arg.Set keep_original_error_size,
        " truncate long error messages as the compiler would"
      ] @ Options.list
    )

let usage = "Usage: expect <options> [script-file [arguments]]\n\
             options are:"

let () =
(* Early disabling of colors in any output *)
  let () =
    Clflags.color := Some Misc.Color.Never;
    Misc.Style.(setup @@ Some Never)
  in
  try
    Arg.parse args main usage;
    Printf.eprintf "expect: no input file\n";
    exit 2
  with exn ->
    Location.report_exception Format.err_formatter exn;
    exit 2
