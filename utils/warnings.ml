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


type name_kind = [ `Constructor | `Label ]
(** Variants of duplicated definitions*)

type identifier_kind =
  [  `Value
  | `Type
  | `Module
  | `Module_type
  | `Class
  | `Class_type
  ]
(** Variants for shadowed identifiers *)

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
  | Duplicate_definitions of name_kind * string * string * string (*30 *)
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
  | Open_shadow_identifier of identifier_kind * string (* 44 *)
  | Open_shadow_label_constructor of name_kind  * string (* 45 *)
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

let disabled = ref false

let without_warnings f =
  Misc.protect_refs [Misc.R(disabled, true)] f

let backup () = !current

let restore x = current := x

let is_active x = not !disabled && (!current).active.(number x);;
let is_error x = not !disabled && (!current).error.(number x);;

let mk_lazy f =
  let state = backup () in
  lazy
    (
      let prev = backup () in
      restore state;
      try
        let r = f () in
        restore prev;
        r
      with exn ->
        restore prev;
        raise exn
    )

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
let defaults_w = "+a-4-6-7-9-27-29-32..42-44-45-48-50-60";;
let defaults_warn_error = "-a+31";;

let () = parse_options false defaults_w;;
let () = parse_options true defaults_warn_error;;

let message = function
  | Comment_start -> I18n.s "this is the start of a comment."
  | Comment_not_end -> I18n.s "this is not the end of a comment."
  | Deprecated (s, _, _) ->
      (* Reduce \r\n to \n:
           - Prevents any \r characters being printed on Unix when processing
             Windows sources
           - Prevents \r\r\n being generated on Windows, which affects the
             testsuite
       *)
       I18n.sprintf "deprecated: %s" (Misc.normalise_eol s)
  | Fragile_match "" ->
      I18n.s "this pattern-matching is fragile."
  | Fragile_match s ->
      I18n.sprintf
        "this pattern-matching is fragile.\n\
         It will remain exhaustive when constructors are added to type %s."
        s
  | Partial_application ->
      I18n.s "this function application is partial,\n\
       maybe some arguments are missing."
  | Labels_omitted [] -> assert false
  | Labels_omitted l ->
      let n = List.length l in
      I18n.snprintf n
        "label %s was omitted in the application of this function."
        "labels %s were omitted in the application of this function."
        (String.concat ", " l)
| Method_override [lab] ->
      I18n.sprintf "the method %s is overridden." lab
  | Method_override (cname :: slist) ->
        I18n.sprintf "the following methods are overridden by the class %s"
        (String.concat " " (cname  :: ":\n " :: slist))
  | Method_override [] -> assert false
  | Partial_match "" -> I18n.s "this pattern-matching is not exhaustive."
  | Partial_match s ->
      I18n.sprintf "this pattern-matching is not exhaustive.\n\
       Here is an example of a case that is not matched:\n%s" s
  | Non_closed_record_pattern s ->
      I18n.sprintf
        "the following labels are not bound in this record pattern:\n%s\n\
         Either bind these labels explicitly or add '; _' to the pattern."
        s
  | Statement_type ->
      I18n.s "this expression should have type unit."
  | Unused_match -> I18n.s "this match case is unused."
  | Unused_pat   -> I18n.s"this sub-pattern is unused."
  | Instance_variable_override [lab] ->
      I18n.sprintf
        "the instance variable %s is overridden.\n\
         The behaviour changed in ocaml 3.10 (previous behaviour was hiding.)"
        lab
  | Instance_variable_override (cname :: slist) ->
      I18n.sprintf
        "the following instance variables are overridden by the class %s\n\
         The behaviour changed in ocaml 3.10 (previous behaviour was hiding.)"
        (String.concat " " (cname  :: ":\n " :: slist))
  | Instance_variable_override [] -> assert false
  | Illegal_backslash -> I18n.sprintf "illegal backslash escape in string."
  | Implicit_public_methods l ->
      I18n.sprintf
        "the following private methods were made public implicitly:\n %s."
        (String.concat " " l)
  | Unerasable_optional_argument ->
      I18n.s "this optional argument cannot be erased."
  | Undeclared_virtual_method m ->
      I18n.sprintf "the virtual method %s is not declared." m
  | Not_principal s -> I18n.sprintf "%a is not principal." I18n.pp s
  | Without_principality s -> I18n.sprintf "%a without principality." I18n.pp s
  | Unused_argument ->
      I18n.s"this argument will not be used by the function."
  | Nonreturning_statement ->
      I18n.s"this statement never returns (or has an unsound type.)"
  | Preprocessor s -> I18n.raw s
  | Useless_record_with ->
      I18n.s"all the fields are explicitly listed in this record:\n\
       the 'with' clause is useless."
  | Bad_module_name (modname) ->
      I18n.sprintf
       "bad source file name: \"%s\" is not a valid module name." modname
  | All_clauses_guarded ->
      I18n.s"this pattern-matching is not exhaustive.\n\
       All clauses in this pattern-matching are guarded."
  | Unused_var v | Unused_var_strict v -> I18n.sprintf "unused variable %s." v
  | Wildcard_arg_to_constant_constr ->
     I18n.s "wildcard pattern given as argument to a constant constructor"
  | Eol_in_string ->
     I18n.s"unescaped end-of-line in a string constant (non-portable code)"
  | Duplicate_definitions (kind, cname, tc1, tc2) ->
      begin match kind with
      | `Constructor ->
          I18n.sprintf "the constructor %s is defined in both types %s and %s."
      | `Label ->
          I18n.sprintf "the label %s is defined in both types %s and %s."
      end
        cname tc1 tc2
  | Multiple_definition (modname, file1, file2) ->
      I18n.sprintf
        "files %s and %s both define a module named %s"
        file1 file2 modname
  | Unused_value_declaration v -> I18n.sprintf "unused value %s." v
  | Unused_open s -> I18n.sprintf "unused open %s." s
  | Unused_type_declaration s -> I18n.sprintf "unused type %s." s
  | Unused_for_index s -> I18n.sprintf "unused for-loop index %s." s
  | Unused_ancestor s -> I18n.sprintf "unused ancestor variable %s." s
  | Unused_constructor (s, false, false) ->
      I18n.sprintf "unused constructor %s." s
  | Unused_constructor (s, true, _) ->
      I18n.sprintf "constructor %s is never used to build values.\n\
                    (However, this constructor appears in patterns.)"
        s
  | Unused_constructor (s, false, true) ->
      I18n.sprintf "constructor %s is never used to build values.\n\
                    Its type is exported as a private type."
        s
  | Unused_extension (s, is_exception, cu_pattern, cu_privatize) ->
     begin match cu_pattern, cu_privatize with
     | false, false ->
         if is_exception then
           I18n.sprintf "unused exception %s" s
         else
           I18n.sprintf "unused extension constructor %s" s
     | _ ->
         let never_used =
           if is_exception then
             I18n.sprintf "exception %s is never used to build values." s
           else
             I18n.sprintf
               "extension constructor %s is never used to build values." s in
         begin match cu_pattern with
         | true ->
             I18n.sprintf
               "%a\n(However, this constructor appears in patterns.)"
               I18n.pp never_used
         | false ->
             I18n.sprintf
               "%a\n\
                It is exported or rebound as a private extension."
               I18n.pp never_used
         end
     end
  | Unused_rec_flag ->
      I18n.s "unused rec flag."
  | Name_out_of_scope (ty, [nm], false) ->
      I18n.sprintf
        "%s was selected from type %s.\n\
         It is not visible in the current scope, and will not \n\
         be selected if the type becomes unknown." nm ty
  | Name_out_of_scope (_, _, false) -> assert false
  | Name_out_of_scope (ty, slist, true) ->
      I18n.sprintf
        "this record of type %s contains fields that are \n\
         not visible in the current scope: %s.\n\
         They will not be selected if the type becomes unknown."
        ty (String.concat " " slist)
  | Ambiguous_name ([s], tl, false) ->
      I18n.sprintf
        "%s belongs to several types: %s\n\
         The first one was selected. Please disambiguate if this is wrong."
        s (String.concat " " tl)
  | Ambiguous_name (_, _, false) -> assert false
  | Ambiguous_name (_slist, tl, true) ->
      I18n.sprintf
        "these field labels belong to several types: %s\n\
         The first one was selected. Please disambiguate if this is wrong."
        (String.concat " " tl)
  | Disambiguated_name s ->
      I18n.sprintf
        "this use of %s relies on type-directed disambiguation,\n\
         it will not compile with OCaml 4.00 or earlier."
        s
  | Nonoptional_label s ->
      I18n.sprintf "the label %s is not optional." s
  | Open_shadow_identifier (kind, s) ->
      begin match kind with
      | `Value ->
          I18n.sprintf
            "this open statement shadows the value identifier %s \
             (which is later used)"
            s
      | `Type ->
          I18n.sprintf
          "this open statement shadows the type identifier %s \
           (which is later used)"
          s
      | `Module ->
          I18n.sprintf
          "this open statement shadows the module identifier %s \
           (which is later used)"
          s
      | `Module_type ->
          I18n.sprintf
          "this open statement shadows the module type identifier %s \
           (which is later used)"
          s
      | `Class ->
          I18n.sprintf
          "this open statement shadows the class identifier %s \
           (which is later used)"
          s
      | `Class_type ->
          I18n.sprintf
            "this open statement shadows the type identifier %s \
             (which is later used)"
            s
      end
  | Open_shadow_label_constructor (kind, s) ->
      begin match kind with
      | `Label ->
          I18n.sprintf
            "this open statement shadows the label %s (which is later used)"
      | `Constructor ->
          I18n.sprintf
            "this open statement shadows the constructor %s (which is later used)"
      end
        s
  | Bad_env_variable (var, s) ->
      I18n.sprintf "illegal environment variable %s : %s" var s
  | Attribute_payload (a, s) ->
      I18n.sprintf "illegal payload for attribute '%s'.\n%s" a s
  | Eliminated_optional_arguments sl ->
      I18n.snprintf
        (List.length sl)
        "implicit elimination of optional argument %s"
        "implicit elimination of optional arguments %s"
        (String.concat ", " sl)
  | No_cmi_file(name, None) ->
      I18n.sprintf "no cmi file was found in path for module %s" name
  | No_cmi_file(name, Some msg) ->
      I18n.sprintf
        "no valid cmi file was found in path for module %s. %s"
        name msg
  | Bad_docstring unattached ->
      if unattached then I18n.s"unattached documentation comment (ignored)"
      else I18n.s"ambiguous documentation comment"
  | Expect_tailcall ->
      I18n.s"expected tailcall"
  | Fragile_literal_pattern ->
      I18n.sprintf
        "Code should not depend on the actual values of\n\
         this constructor's arguments. They are only for information\n\
         and may change in future versions. (See manual section 8.5)"
  | Unreachable_case ->
      I18n.s
        "this match case is unreachable.\n\
         Consider replacing it with a refutation case '<pat> -> .'"
  | Misplaced_attribute attr_name ->
      I18n.sprintf "the %S attribute cannot appear in this context" attr_name
  | Duplicated_attribute attr_name ->
      I18n.sprintf "the %S attribute is used more than once on this \
                    expression"
        attr_name
  | Inlining_impossible reason ->
      I18n.sprintf "Cannot inline: %a" I18n.pp reason
  | Ambiguous_pattern vars ->
      let varlist =
        match List.sort String.compare vars with
        | [] -> assert false
        | l -> String.concat "," l in
      I18n.snprintf (List.length vars)
        "Ambiguous or-pattern variables under guard;\n\
         variable %s may match different arguments. (See manual section 9.5)"
        ("Ambiguous or-pattern variables under guard;\n\
         variables %s may match different arguments. (See manual section 9.5)"
         [@ocaml.doc {|%s is a list of comma separated variable name in
                       variables %s|} ])
        varlist
  | No_cmx_file name ->
      I18n.sprintf
        "no cmx file was found in path for module %s, \
         and its interface was not compiled with -opaque" name
  | Assignment_to_non_mutable_value ->
      I18n.sprintf
        "A potential assignment to a non-mutable value was detected \n\
         in this source file.  Such assignments may generate incorrect code \n\
         when using Flambda."
  | Unused_module s -> I18n.sprintf "unused module %s." s
  | Unboxable_type_in_prim_decl t ->
      I18n.sprintf
        "This primitive declaration uses type %s, which is unannotated and\n\
         unboxable. The representation of such types may change in future\n\
         versions. You should annotate the declaration of %s with [@@boxed]\n\
         or [@@unboxed]." t t
  | Constraint_on_gadt ->
      I18n.s "Type constraints do not apply to GADT cases of variant types."
;;

let sub_locs = function
  | Deprecated (_, def, use) ->
      [
        def, I18n.s"Definition";
        use, I18n.s"Expected signature";
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
    1, I18n.s"Suspicious-looking start-of-comment mark.";
    2, I18n.s"Suspicious-looking end-of-comment mark.";
    3, I18n.s"Deprecated feature.";
    4, I18n.s"Fragile pattern matching: matching that will remain complete even\n\
   \    if additional constructors are added to one of the variant types\n\
   \    matched.";
    5, I18n.s"Partially applied function: expression whose result has function\n\
   \    type and is ignored.";
    6, I18n.s"Label omitted in function application.";
    7, I18n.s"Method overridden.";
    8, I18n.s"Partial match: missing cases in pattern-matching.";
    9, I18n.s"Missing fields in a record pattern.";
   10, I18n.s"Expression on the left-hand side of a sequence that doesn't have \
   \    type\n\
   \    \"unit\" (and that is not a function, see warning number 5).";
   11, I18n.s"Redundant case in a pattern matching (unused match case).";
   12, I18n.s"Redundant sub-pattern in a pattern-matching.";
   13, I18n.s"Instance variable overridden.";
   14, I18n.s"Illegal backslash escape in a string constant.";
   15, I18n.s"Private method made public implicitly.";
   16, I18n.s"Unerasable optional argument.";
   17, I18n.s"Undeclared virtual method.";
   18, I18n.s"Non-principal type.";
   19, I18n.s"Type without principality.";
   20, I18n.s"Unused function argument.";
   21, I18n.s"Non-returning statement.";
   22, I18n.s"Preprocessor warning.";
   23, I18n.s"Useless record \"with\" clause.";
   24, I18n.s"Bad module name: the source file name is not a valid OCaml module \
   \    name.";
   25, I18n.s"Deprecated: now part of warning 8.";
   26, I18n.s"Suspicious unused variable: unused variable that is bound\n\
   \    with \"let\" or \"as\", and doesn't start with an underscore (\"_\")\n\
   \    character.";
   27, I18n.s"Innocuous unused variable: unused variable that is not bound with\n\
   \    \"let\" nor \"as\", and doesn't start with an underscore (\"_\")\n\
   \    character.";
   28, I18n.s"Wildcard pattern given as argument to a constant constructor.";
   29, I18n.s"Unescaped end-of-line in a string constant (non-portable code).";
   30, I18n.s"Two labels or constructors of the same name are defined in two\n\
   \    mutually recursive types.";
   31, I18n.s"A module is linked twice in the same executable.";
   32, I18n.s"Unused value declaration.";
   33, I18n.s"Unused open statement.";
   34, I18n.s"Unused type declaration.";
   35, I18n.s"Unused for-loop index.";
   36, I18n.s"Unused ancestor variable.";
   37, I18n.s"Unused constructor.";
   38, I18n.s"Unused extension constructor.";
   39, I18n.s"Unused rec flag.";
   40, I18n.s"Constructor or label name used out of scope.";
   41, I18n.s"Ambiguous constructor or label name.";
   42, I18n.s"Disambiguated constructor or label name (compatibility warning).";
   43, I18n.s"Nonoptional label applied as optional.";
   44, I18n.s"Open statement shadows an already defined identifier.";
   45, I18n.s"Open statement shadows an already defined label or constructor.";
   46, I18n.s"Error in environment variable.";
   47, I18n.s"Illegal attribute payload.";
   48, I18n.s"Implicit elimination of optional arguments.";
   49, I18n.s"Absent cmi file when looking up module alias.";
   50, I18n.s"Unexpected documentation comment.";
   51, I18n.s"Warning on non-tail calls if @tailcall present.";
   52, I18n.s"Fragile constant pattern.";
   53, I18n.s"Attribute cannot appear in this context";
   54, I18n.s"Attribute used more than once on an expression";
   55, I18n.s"Inlining impossible";
   56, I18n.s"Unreachable case in a pattern-matching (based on type information).";
   57, I18n.s"Ambiguous or-pattern variables under guard";
   58, I18n.s"Missing cmx file";
   59, I18n.s"Assignment to non-mutable value";
   60, I18n.s"Unused module declaration";
   61, I18n.s"Unboxable type in primitive declaration";
   62, I18n.s"Type constraint on GADT type declaration"
  ]
;;

let help_warnings () =
  List.iter (fun (i, s) -> Format.printf "%3i %a\n" i I18n.pp s) (descriptions ());
  I18n.printf
    ("  A all warnings@."[@ocaml.doc {| A is the letter A not an article |}]);
  for i = Char.code 'b' to Char.code 'z' do
    let c = Char.chr i in
    match letter c with
    | [] -> ()
    | [n] ->
        I18n.printf "  %c Alias for warning %i.\n" (Char.uppercase_ascii c) n
    | l ->
        I18n.printf "  %c warnings %s.\n"
          (Char.uppercase_ascii c)
          (String.concat ", " (List.map string_of_int l))
  done;
  exit 0
;;
