(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Pierre Weis && Damien Doligez, INRIA Rocquencourt          *)
(*                                                                        *)
(*   Copyright 1998 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* When you change this, you need to update the documentation:
   - man/ocamlc.m
   - man/ocamlopt.m
   - manual/manual/cmds/comp.etex
   - manual/manual/cmds/native.etex
*)

type loc = {
  loc_start: Lexing.position;
  loc_end: Lexing.position;
  loc_ghost: bool;
}

type t =
  | Comment_start                           (*  1 *)
  | Comment_not_end                         (*  2 *)
  | Deprecated of string * loc * loc        (*  3 *)
  | Fragile_match of string                 (*  4 *)
  | Partial_application                     (*  5 *)
  | Labels_omitted of string list           (*  6 *)
  | Method_override of string list          (*  7 *)
  | Partial_match of string                 (*  8 *)
  | Non_closed_record_pattern of string     (*  9 *)
  | Statement_type                          (* 10 *)
  | Unused_match                            (* 11 *)
  | Unused_pat                              (* 12 *)
  | Instance_variable_override of string list (* 13 *)
  | Illegal_backslash                       (* 14 *)
  | Implicit_public_methods of string list  (* 15 *)
  | Unerasable_optional_argument            (* 16 *)
  | Undeclared_virtual_method of string     (* 17 *)
  | Not_principal of I18n.s                 (* 18 *)
  | Without_principality of I18n.s          (* 19 *)
  | Unused_argument                         (* 20 *)
  | Nonreturning_statement                  (* 21 *)
  | Preprocessor of string                  (* 22 *)
  | Useless_record_with                     (* 23 *)
  | Bad_module_name of string               (* 24 *)
  | All_clauses_guarded                     (* 8, used to be 25 *)
  | Unused_var of string                    (* 26 *)
  | Unused_var_strict of string             (* 27 *)
  | Wildcard_arg_to_constant_constr         (* 28 *)
  | Eol_in_string                           (* 29 *)
  | Duplicate_definitions of string * string * string * string (*30 *)
  | Multiple_definition of string * string * string (* 31 *)
  | Unused_value_declaration of string      (* 32 *)
  | Unused_open of string                   (* 33 *)
  | Unused_type_declaration of string       (* 34 *)
  | Unused_for_index of string              (* 35 *)
  | Unused_ancestor of string               (* 36 *)
  | Unused_constructor of string * bool * bool  (* 37 *)
  | Unused_extension of string * bool * bool * bool (* 38 *)
  | Unused_rec_flag                         (* 39 *)
  | Name_out_of_scope of string * string list * bool (* 40 *)
  | Ambiguous_name of string list * string list *  bool    (* 41 *)
  | Disambiguated_name of string            (* 42 *)
  | Nonoptional_label of string             (* 43 *)
  | Open_shadow_identifier of I18n.s * string (* 44 *)
  | Open_shadow_label_constructor of I18n.s  * string (* 45 *)
  | Bad_env_variable of string * string     (* 46 *)
  | Attribute_payload of string * string    (* 47 *)
  | Eliminated_optional_arguments of string list (* 48 *)
  | No_cmi_file of string * string option   (* 49 *)
  | Bad_docstring of bool                   (* 50 *)
  | Expect_tailcall                         (* 51 *)
  | Fragile_literal_pattern                 (* 52 *)
  | Misplaced_attribute of string           (* 53 *)
  | Duplicated_attribute of string          (* 54 *)
  | Inlining_impossible of I18n.s           (* 55 *)
  | Unreachable_case                        (* 56 *)
  | Ambiguous_pattern of string list        (* 57 *)
  | No_cmx_file of string                   (* 58 *)
  | Assignment_to_non_mutable_value         (* 59 *)
  | Unused_module of string                 (* 60 *)
  | Unboxable_type_in_prim_decl of string   (* 61 *)
  | Constraint_on_gadt                      (* 62 *)
;;

(* If you remove a warning, leave a hole in the numbering.  NEVER change
   the numbers of existing warnings.
   If you add a new warning, add it at the end with a new number;
   do NOT reuse one of the holes.
*)

let number = function
  | Comment_start -> 1
  | Comment_not_end -> 2
  | Deprecated _ -> 3
  | Fragile_match _ -> 4
  | Partial_application -> 5
  | Labels_omitted _ -> 6
  | Method_override _ -> 7
  | Partial_match _ -> 8
  | Non_closed_record_pattern _ -> 9
  | Statement_type -> 10
  | Unused_match -> 11
  | Unused_pat -> 12
  | Instance_variable_override _ -> 13
  | Illegal_backslash -> 14
  | Implicit_public_methods _ -> 15
  | Unerasable_optional_argument -> 16
  | Undeclared_virtual_method _ -> 17
  | Not_principal _ -> 18
  | Without_principality _ -> 19
  | Unused_argument -> 20
  | Nonreturning_statement -> 21
  | Preprocessor _ -> 22
  | Useless_record_with -> 23
  | Bad_module_name _ -> 24
  | All_clauses_guarded -> 8 (* used to be 25 *)
  | Unused_var _ -> 26
  | Unused_var_strict _ -> 27
  | Wildcard_arg_to_constant_constr -> 28
  | Eol_in_string -> 29
  | Duplicate_definitions _ -> 30
  | Multiple_definition _ -> 31
  | Unused_value_declaration _ -> 32
  | Unused_open _ -> 33
  | Unused_type_declaration _ -> 34
  | Unused_for_index _ -> 35
  | Unused_ancestor _ -> 36
  | Unused_constructor _ -> 37
  | Unused_extension _ -> 38
  | Unused_rec_flag -> 39
  | Name_out_of_scope _ -> 40
  | Ambiguous_name _ -> 41
  | Disambiguated_name _ -> 42
  | Nonoptional_label _ -> 43
  | Open_shadow_identifier _ -> 44
  | Open_shadow_label_constructor _ -> 45
  | Bad_env_variable _ -> 46
  | Attribute_payload _ -> 47
  | Eliminated_optional_arguments _ -> 48
  | No_cmi_file _ -> 49
  | Bad_docstring _ -> 50
  | Expect_tailcall -> 51
  | Fragile_literal_pattern -> 52
  | Misplaced_attribute _ -> 53
  | Duplicated_attribute _ -> 54
  | Inlining_impossible _ -> 55
  | Unreachable_case -> 56
  | Ambiguous_pattern _ -> 57
  | No_cmx_file _ -> 58
  | Assignment_to_non_mutable_value -> 59
  | Unused_module _ -> 60
  | Unboxable_type_in_prim_decl _ -> 61
  | Constraint_on_gadt -> 62
;;

let last_warning_number = 62
;;

(* Must be the max number returned by the [number] function. *)

let letter = function
  | 'a' ->
     let rec loop i = if i = 0 then [] else i :: loop (i - 1) in
     loop last_warning_number
  | 'b' -> []
  | 'c' -> [1; 2]
  | 'd' -> [3]
  | 'e' -> [4]
  | 'f' -> [5]
  | 'g' -> []
  | 'h' -> []
  | 'i' -> []
  | 'j' -> []
  | 'k' -> [32; 33; 34; 35; 36; 37; 38; 39]
  | 'l' -> [6]
  | 'm' -> [7]
  | 'n' -> []
  | 'o' -> []
  | 'p' -> [8]
  | 'q' -> []
  | 'r' -> [9]
  | 's' -> [10]
  | 't' -> []
  | 'u' -> [11; 12]
  | 'v' -> [13]
  | 'w' -> []
  | 'x' -> [14; 15; 16; 17; 18; 19; 20; 21; 22; 23; 24; 30]
  | 'y' -> [26]
  | 'z' -> [27]
  | _ -> assert false
;;

type state =
  {
    active: bool array;
    error: bool array;
  }

let current =
  ref
    {
      active = Array.make (last_warning_number + 1) true;
      error = Array.make (last_warning_number + 1) false;
    }

let backup () = !current

let restore x = current := x

let is_active x = (!current).active.(number x);;
let is_error x = (!current).error.(number x);;

let parse_opt error active flags s =
  let set i = flags.(i) <- true in
  let clear i = flags.(i) <- false in
  let set_all i = active.(i) <- true; error.(i) <- true in
  let error () = raise (Arg.Bad "Ill-formed list of warnings") in
  let rec get_num n i =
    if i >= String.length s then i, n
    else match s.[i] with
    | '0'..'9' -> get_num (10 * n + Char.code s.[i] - Char.code '0') (i + 1)
    | _ -> i, n
  in
  let get_range i =
    let i, n1 = get_num 0 i in
    if i + 2 < String.length s && s.[i] = '.' && s.[i + 1] = '.' then
      let i, n2 = get_num 0 (i + 2) in
      if n2 < n1 then error ();
      i, n1, n2
    else
      i, n1, n1
  in
  let rec loop i =
    if i >= String.length s then () else
    match s.[i] with
    | 'A' .. 'Z' ->
       List.iter set (letter (Char.lowercase_ascii s.[i]));
       loop (i+1)
    | 'a' .. 'z' ->
       List.iter clear (letter s.[i]);
       loop (i+1)
    | '+' -> loop_letter_num set (i+1)
    | '-' -> loop_letter_num clear (i+1)
    | '@' -> loop_letter_num set_all (i+1)
    | _ -> error ()
  and loop_letter_num myset i =
    if i >= String.length s then error () else
    match s.[i] with
    | '0' .. '9' ->
        let i, n1, n2 = get_range i in
        for n = n1 to min n2 last_warning_number do myset n done;
        loop i
    | 'A' .. 'Z' ->
       List.iter myset (letter (Char.lowercase_ascii s.[i]));
       loop (i+1)
    | 'a' .. 'z' ->
       List.iter myset (letter s.[i]);
       loop (i+1)
    | _ -> error ()
  in
  loop 0
;;

let parse_options errflag s =
  let error = Array.copy (!current).error in
  let active = Array.copy (!current).active in
  parse_opt error active (if errflag then error else active) s;
  current := {error; active}

(* If you change these, don't forget to change them in man/ocamlc.m *)
let defaults_w = "+a-4-6-7-9-27-29-32..39-41..42-44-45-48-50-60";;
let defaults_warn_error = "-a+31";;

let () = parse_options false defaults_w;;
let () = parse_options true defaults_warn_error;;

open I18n open I18n_core
let message = function
  | Comment_start -> s_"this is the start of a comment."
  | Comment_not_end -> s_"this is not the end of a comment."
  | Deprecated (s, _, _) ->
      (* Reduce \r\n to \n:
           - Prevents any \r characters being printed on Unix when processing
             Windows sources
           - Prevents \r\r\n being generated on Windows, which affects the
             testsuite
       *)
       sprintf (f_"deprecated: %s") (Misc.normalise_eol s)
  | Fragile_match "" ->
      s_ "this pattern-matching is fragile."
  | Fragile_match s ->
      sprintf
        (f_ "this pattern-matching is fragile.\n\
             It will remain exhaustive when constructors are added to type %s.")
        s
  | Partial_application ->
      s_"this function application is partial,\n\
       maybe some arguments are missing."
  | Labels_omitted [] -> assert false
  | Labels_omitted l ->
      let n = List.length l in
      sprintf
        (fn_
           "label %s was omitted in the application of this function."
           "labels %s were omitted in the application of this function."
           n
        )
       (String.concat ", " l)
| Method_override [lab] ->
      sprintf (f_"the method %s is overridden.") lab
  | Method_override (cname :: slist) ->
        sprintf (f_"the following methods are overridden by the class %s")
        (String.concat " " (cname  :: ":\n " :: slist))
  | Method_override [] -> assert false
  | Partial_match "" -> s_"this pattern-matching is not exhaustive."
  | Partial_match s ->
      sprintf (f_"this pattern-matching is not exhaustive.\n\
       Here is an example of a case that is not matched:\n%s") s
  | Non_closed_record_pattern s ->
      sprintf
        (f_"the following labels are not bound in this record pattern:\n%s\n\
           Either bind these labels explicitly or add '; _' to the pattern.")
        s
  | Statement_type ->
      s_"this expression should have type unit."
  | Unused_match -> s_"this match case is unused."
  | Unused_pat   -> s_"this sub-pattern is unused."
  | Instance_variable_override [lab] ->
      sprintf
        (f_ "the instance variable %s is overridden.\n\
             The behaviour changed in ocaml 3.10 (previous behaviour was hiding.)"
        ) lab
  | Instance_variable_override (cname :: slist) ->
      sprintf
        (f_ "the following instance variables are overridden by the class %s\n\
             The behaviour changed in ocaml 3.10 (previous behaviour was hiding.)"
        )
        (String.concat " " (cname  :: ":\n " :: slist))
  | Instance_variable_override [] -> assert false
  | Illegal_backslash -> s_"illegal backslash escape in string."
  | Implicit_public_methods l ->
      sprintf
      (f_"the following private methods were made public implicitly:\n %s.")
      (String.concat " " l)
  | Unerasable_optional_argument -> s_"this optional argument cannot be erased."
  | Undeclared_virtual_method m ->
      sprintf (f_"the virtual method %s is not declared.") m
  | Not_principal s -> asprintf (f_"%a is not principal.") i18n s
  | Without_principality s -> asprintf (f_"%a without principality.") i18n s
  | Unused_argument -> s_"this argument will not be used by the function."
  | Nonreturning_statement ->
      s_"this statement never returns (or has an unsound type.)"
  | Preprocessor s -> raw s
  | Useless_record_with ->
      s_"all the fields are explicitly listed in this record:\n\
       the 'with' clause is useless."
  | Bad_module_name (modname) ->
      sprintf
        (f_"bad source file name: \"%s\" is not a valid module name.") modname
  | All_clauses_guarded ->
      s_"this pattern-matching is not exhaustive.\n\
       All clauses in this pattern-matching are guarded."
  | Unused_var v | Unused_var_strict v -> sprintf (f_"unused variable %s.") v
  | Wildcard_arg_to_constant_constr ->
     s_"wildcard pattern given as argument to a constant constructor"
  | Eol_in_string ->
     s_"unescaped end-of-line in a string constant (non-portable code)"
  | Duplicate_definitions (kind, cname, tc1, tc2) ->
      asprintf (f_"the %a %s is defined in both types %s and %s.")
        i18n (s_ kind) cname tc1 tc2
  | Multiple_definition(modname, file1, file2) ->
      sprintf
        (f_"files %s and %s both define a module named %s")
        file1 file2 modname
  | Unused_value_declaration v -> sprintf (f_"unused value %s.") v
  | Unused_open s -> sprintf (f_"unused open %s.") s
  | Unused_type_declaration s -> sprintf (f_"unused type %s.") s
  | Unused_for_index s -> sprintf (f_"unused for-loop index %s.") s
  | Unused_ancestor s -> sprintf (f_"unused ancestor variable %s.") s
  | Unused_constructor (s, false, false) ->
      sprintf (f_"unused constructor %s.") s
  | Unused_constructor (s, true, _) ->
      sprintf (f_"constructor %s is never used to build values.\n\
                  (However, this constructor appears in patterns.)")
        s
  | Unused_constructor (s, false, true) ->
      sprintf (f_"constructor %s is never used to build values.\n\
                  Its type is exported as a private type.")
        s
  | Unused_extension (s, is_exception, cu_pattern, cu_privatize) ->
      let kind =
        if is_exception then s_"exception" else s_"extension constructor" in
     begin match cu_pattern, cu_privatize with
     | false, false -> asprintf (f_"unused %a %s") i18n kind s
     | true, _ ->
         asprintf (f_"%a %s is never used to build values.\n\
                  (However, this constructor appears in patterns.)")
           i18n kind s
     | false, true ->
         asprintf
           (f_"%a %s is never used to build values.\n\
               It is exported or rebound as a private extension.")
           i18n kind s
     end
  | Unused_rec_flag ->
      s_"unused rec flag."
  | Name_out_of_scope (ty, [nm], false) ->
      sprintf
      (f_"%s was selected from type %s.\n\
       It is not visible in the current scope, and will not \n\
       be selected if the type becomes unknown.") nm ty
  | Name_out_of_scope (_, _, false) -> assert false
  | Name_out_of_scope (ty, slist, true) ->
      sprintf
      (f_"this record of type %s contains fields that are \n\
          not visible in the current scope: %s.\n\
          They will not be selected if the type becomes unknown."
      ) ty (String.concat " " slist)
  | Ambiguous_name ([s], tl, false) ->
      sprintf
        (f_"%s belongs to several types: %s\n\
            The first one was selected. Please disambiguate if this is wrong.")
        s (String.concat " " tl)
  | Ambiguous_name (_, _, false) -> assert false
  | Ambiguous_name (_slist, tl, true) ->
      sprintf
        (f_"these field labels belong to several types: %s\n\
            The first one was selected. Please disambiguate if this is wrong."
        )
        (String.concat " " tl)
  | Disambiguated_name s ->
      sprintf
        (f_"this use of %s relies on type-directed disambiguation,\n\
            it will not compile with OCaml 4.00 or earlier.")
        s
  | Nonoptional_label s ->
      sprintf (f_"the label %s is not optional.") s
  | Open_shadow_identifier (kind, s) ->
      asprintf
        (f_"this open statement shadows the %a identifier %s \
            (which is later used)")
        i18n kind s
  | Open_shadow_label_constructor (kind, s) ->
      asprintf
        (f_"this open statement shadows the %a %s (which is later used)")
        i18n kind s
  | Bad_env_variable (var, s) ->
      sprintf (f_"illegal environment variable %s : %s") var s
  | Attribute_payload (a, s) ->
      sprintf (f_"illegal payload for attribute '%s'.\n%s") a s
  | Eliminated_optional_arguments sl ->
      sprintf (f_"implicit elimination of optional argument%s %s")
        (if List.length sl = 1 then "" else "s")
        (String.concat ", " sl)
  | No_cmi_file(name, None) ->
      sprintf (f_"no cmi file was found in path for module %s") name
  | No_cmi_file(name, Some msg) ->
      sprintf
        (f_"no valid cmi file was found in path for module %s. %s")
        name msg
  | Bad_docstring unattached ->
      if unattached then s_"unattached documentation comment (ignored)"
      else s_"ambiguous documentation comment"
  | Expect_tailcall ->
      s_"expected tailcall"
  | Fragile_literal_pattern ->
      sprintf
        (f_"Code should not depend on the actual values of\n\
         this constructor's arguments. They are only for information\n\
         and may change in future versions. (See manual section 8.5)")
  | Unreachable_case ->
      s_"this match case is unreachable.\n\
       Consider replacing it with a refutation case '<pat> -> .'"
  | Misplaced_attribute attr_name ->
      sprintf (f_"the %S attribute cannot appear in this context") attr_name
  | Duplicated_attribute attr_name ->
      sprintf (f_"the %S attribute is used more than once on this \
               expression")
        attr_name
  | Inlining_impossible reason ->
      asprintf (f_"Cannot inline: %a") i18n reason
  | Ambiguous_pattern vars ->
      let msg =
        let vars = List.sort String.compare vars in
        match vars with
        | [] -> assert false
        | l ->
            sprintf (fn_ "variable %s" "variables %s" (List.length l) )
              (String.concat "," vars) in
      asprintf
        (f_"Ambiguous or-pattern variables under guard;\n\
         %a may match different arguments. (See manual section 8.5)")
        i18n msg
  | No_cmx_file name ->
      sprintf
        (f_"no cmx file was found in path for module %s, \
         and its interface was not compiled with -opaque") name
  | Assignment_to_non_mutable_value ->
      s_"A potential assignment to a non-mutable value was detected \n\
        in this source file.  Such assignments may generate incorrect code \n\
        when using Flambda."
  | Unused_module s -> sprintf (f_"unused module %s.") s
  | Unboxable_type_in_prim_decl t ->
      sprintf
        (f_"This primitive declaration uses type %s, which is unannotated and\n\
         unboxable. The representation of such types may change in future\n\
         versions. You should annotate the declaration of %s with [@@boxed]\n\
         or [@@unboxed].") t t
  | Constraint_on_gadt ->
      s_"Type constraints do not apply to GADT cases of variant types."
;;

let sub_locs = function
  | Deprecated (_, def, use) ->
      [
        def, s_"Definition";
        use, s_"Expected signature";
      ]
  | _ -> []

let nerrors = ref 0;;

type reporting_information =
  { number : int
  ; message : I18n.s
  ; is_error : bool
  ; sub_locs : (loc * I18n.s) list;
  }

let report w =
  match is_active w with
  | false -> `Inactive
  | true ->
     if is_error w then incr nerrors;
     `Active { number = number w; message = message w; is_error = is_error w;
               sub_locs = sub_locs w;
             }
;;

exception Errors;;

let reset_fatal () =
  nerrors := 0

let check_fatal () =
  if !nerrors > 0 then begin
    nerrors := 0;
    raise Errors;
  end;
;;

let descriptions () =
  [
    1, s_"Suspicious-looking start-of-comment mark.";
    2, s_"Suspicious-looking end-of-comment mark.";
    3, s_"Deprecated feature.";
    4, s_"Fragile pattern matching: matching that will remain complete even\n\
   \    if additional constructors are added to one of the variant types\n\
   \    matched.";
    5, s_"Partially applied function: expression whose result has function\n\
   \    type and is ignored.";
    6, s_"Label omitted in function application.";
    7, s_"Method overridden.";
    8, s_"Partial match: missing cases in pattern-matching.";
    9, s_"Missing fields in a record pattern.";
   10, s_"Expression on the left-hand side of a sequence that doesn't have \
      ts_ype\n\
   \   s_ \"unit\" (and that is not a function, see warning number 5).";
   11, s_"Redundant case in a pattern matching (unused match case).";
   12, s_"Redundant sub-pattern in a pattern-matching.";
   13, s_"Instance variable overridden.";
   14, s_"Illegal backslash escape in a string constant.";
   15, s_"Private method made public implicitly.";
   16, s_"Unerasable optional argument.";
   17, s_"Undeclared virtual method.";
   18, s_"Non-principal type.";
   19, s_"Type without principality.";
   20, s_"Unused function argument.";
   21, s_"Non-returning statement.";
   22, s_"Preprocessor warning.";
   23, s_"Useless record \"with\" clause.";
   24, s_"Bad module name: the source file name is not a valid OCaml module \
          name.";
   (* 2s_5, "Pattern-matching with all clauses guarded.  Exhaustiveness cannot \
      be\n\
   \    checked.";  (* Now part of warning 8 *) *)
   26, s_"Suspicious unused variable: unused variable that is bound\n\
   \    with \"let\" or \"as\", and doesn't start with an underscore (\"_\")\n\
   \    character.";
   27, s_"Innocuous unused variable: unused variable that is not bound with\n\
   \    \"let\" nor \"as\", and doesn't start with an underscore (\"_\")\n\
   \    character.";
   28, s_"Wildcard pattern given as argument to a constant constructor.";
   29, s_"Unescaped end-of-line in a string constant (non-portable code).";
   30, s_"Two labels or constructors of the same name are defined in two\n\
   \    mutually recursive types.";
   31, s_"A module is linked twice in the same executable.";
   32, s_"Unused value declaration.";
   33, s_"Unused open statement.";
   34, s_"Unused type declaration.";
   35, s_"Unused for-loop index.";
   36, s_"Unused ancestor variable.";
   37, s_"Unused constructor.";
   38, s_"Unused extension constructor.";
   39, s_"Unused rec flag.";
   40, s_"Constructor or label name used out of scope.";
   41, s_"Ambiguous constructor or label name.";
   42, s_"Disambiguated constructor or label name (compatibility warning).";
   43, s_"Nonoptional label applied as optional.";
   44, s_"Open statement shadows an already defined identifier.";
   45, s_"Open statement shadows an already defined label or constructor.";
   46, s_"Error in environment variable.";
   47, s_"Illegal attribute payload.";
   48, s_"Implicit elimination of optional arguments.";
   49, s_"Absent cmi file when looking up module alias.";
   50, s_"Unexpected documentation comment.";
   51, s_"Warning on non-tail calls if @tailcall present.";
   52, s_"Fragile constant pattern.";
   53, s_"Attribute cannot appear in this context";
   54, s_"Attribute used more than once on an expression";
   55, s_"Inlining impossible";
   56, s_"Unreachable case in a pattern-matching (based on type information).";
   57, s_"Ambiguous or-pattern variables under guard";
   58, s_"Missing cmx file";
   59, s_"Assignment to non-mutable value";
   60, s_"Unused module declaration";
   61, s_"Unboxable type in primitive declaration";
   62, s_"Type constraint on GADT type declaration"
  ]
;;

let help_warnings () =
  List.iter (fun (i, s) -> Format.printf "%3i %a\n" i i18n s) (descriptions ());
  print_endline "  A all warnings";
  for i = Char.code 'b' to Char.code 'z' do
    let c = Char.chr i in
    match letter c with
    | [] -> ()
    | [n] ->
        printf (f_"  %c Alias for warning %i.\n") (Char.uppercase_ascii c) n
    | l ->
        printf (f_"  %c warnings %s.\n")
          (Char.uppercase_ascii c)
          (String.concat ", " (List.map string_of_int l))
  done;
  exit 0
;;
