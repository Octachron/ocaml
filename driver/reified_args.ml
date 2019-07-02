(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Florian Angeletti, projet Gallium, INRIA Paris             *)
(*                                                                        *)
(*   Copyright 2019 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)


let set r = Arg.Unit (fun () -> r := true)
let set_some r = Arg.String (fun s -> r := Some s)
let set_some_int r = Arg.Int (fun n -> r := Some n)
let append r = Arg.string (fun s -> r:= s :: !r)
let unset r = Arg.Unit (fun () -> r := false)
let action (_name, action, _description) = action


let a =
  "-a", set Clflags.make_archive, " Build a library"
;;

let absname =
  "-absname", set Clflags.absname,
  " Show absolute filenames in error messages"
;;

let annot =
  "-annot", set Clflags.annotations, " Save information in <filename>.annot"
;;

let binannot =
  "-bin-annot", set Clflags.binary_annotations,
  " Save typedtree in <filename>.cmt"
;;

let c =
  "-c", set Clflags.compile_only, " Compile only (do not link)"
;;

let cc =
  "-cc", set_some Clflags.c_compiler,
  "<command>  Use <command> as the C compiler and linker"
;;

let cclib =
  let process s = Compenv.(defer @@ ProcessObject (Misc.rev_split_words s)) in
  "-cclib", Arg.String process, "<opt>  Pass option <opt> to the C linker"
;;

let ccopt =
  "-ccopt", append Clflags.first_ccopts,
  "<opt>  Pass option <opt> to the C compiler and linker"
;;

let clambda_checks =
  "-clambda-checks", set Clflags.clambda_check,
  " Instrument clambda code with closure and field access checks \
   (for debugging the compiler)"
;;

let compact =
  "-compact", unset Clflags.optimize_for_speed,
  " Optimize code size rather than speed"
;;

let compat_32 =
  "-compat-32", set Clflags.bytecode_compatible_32,
  " Check that generated bytecode can run on 32-bit platforms"
;;

let config =
  "-config", Arg.Unit Misc.show_config_and_exit,
  " Print configuration values and exit"
;;

let config_var =
  "-config-var", Arg.String Misc.show_config_variable_and_exit,
  " Print the value of a configuration variable, a newline, and exit\n\
\    (print nothing and exit with error value if the variable does not exist)"
;;

let custom =
  "-custom", set Clflags.custom_runtime, " Link in custom mode"
;;

let dllib =
  let f s = Compenv.(defer @@ ProcessDLLs (Misc.rev_split_words s)) in
  "-dllib", Arg.String f, "<lib>  Use the dynamically-loaded library <lib>"
;;

let dllpath =
  let append_last s = Clflags.dllpath := Clflags.dllpath @ [s] in
  "-dllpath", Arg.String append_last,
  "<dir>  Add <dir> to the run-time search path for shared libraries"
;;

let stop_after =
  let f pass =
    let module P = Compiler_pass in
    begin match P.of_string pass with
    | None -> () (* this should not occur as we use Arg.Symbol *)
    | Some pass ->
        Clflags.stop_after := Some pass;
        begin match pass with
        | P.Parsing | P.Typing ->
            compile_only := true
        end;
    end in
  "-stop-after", Arg.Symbol (Clflags.Compiler_pass.pass_names, f),
  " Stop after the given compilation pass."
;;

let dtypes =
  "-dtypes", action annot, " (deprecated) same as -annot"
;;

let for_pack_byt =
  "-for-pack", set_some Clflags.for_pack,
  "<ident>  Generate code that can later be `packed' with\n\
  \     ocamlc -pack -o <ident>.cmo"
;;

let for_pack_opt =
  "-for-pack", set_some Clflags.for_pack,
  "<ident>  Generate code that can later be `packed' with\n\
  \     ocamlopt -pack -o <ident>.cmx"
;;

let g_byt =
  "-g", set Clflags.debug, " Save debugging information"
;;

let g_opt =
  "-g",  set Clflags.debug,
  " Record debugging information for exception backtrace"
;;

let i =
  let f () =
    print_types := true;
    compile_only := true;
    stop_after := Some Compiler_pass.Typing;
    () in
  "-i", Arg.Unit f, " Print inferred interface"
;;

let fI =
  "-I", append Clflags.include_dirs,
  "<dir>  Add <dir> to the list of include directories"
;;

let impl =
  "-impl", Arg.String Compenv.impl, "<file>  Compile <file> as a .ml file"
;;

let init =
  "-init", set_some Clflags.init_file,
  "<file>  Load <file> instead of default init file"
;;

let inline =
  let f = Float_arg_helper.parse spec
      "Syntax: -inline <n> | <round>=<n>[,...]"  Clflags.inline_threshold in
  "-inline", Arg.String f,
    Printf.sprintf "<n>|<round>=<n>[,...]  Aggressiveness of inlining \
        (default %.02f, higher numbers mean more aggressive)"
      Clflags.default_inline_threshold
;;

let inline_toplevel =
  let f =
    Int_arg_helper.parse spec
      "Syntax: -inline-toplevel <n> | <round>=<n>[,...]"
      inline_toplevel_threshold
  in
  "-inline-toplevel", Arg.String f,
    Printf.sprintf "<n>|<round>=<n>[,...]  Aggressiveness of inlining at \
      toplevel (higher numbers mean more aggressive)"
;;

let inlining_report =
  "-inlining-report", set Clflags.inline_report,
  " Emit `.<round>.inlining' file(s) (one per round) showing \
   the inliner's decisions"
;;

let dump_pass =
  "-dump-pass", Arg.String (fun pass -> Compenv.set_dumped_pass pass true),
  Format.asprintf
    " @[<4>Record transformations performed by these passes:@ @[%a@]@]"
    (Format.pp_print_list
       ~pp_sep:Format.pp_print_space
       Format.pp_print_string)
    !Clflags.all_passes
;;

let o2 =
  let action () =
    let open Clflags in
    default_simplify_rounds := 2;
    use_inlining_arguments_set o2_arguments;
    use_inlining_arguments_set ~round:0 o1_arguments in
  "-O2", Arg.Unit action, " Apply increased optimization for speed"
;;

let o3 =
  let action () =
    let open Clflags in
    default_simplify_rounds := 3;
    use_inlining_arguments_set o3_arguments;
    use_inlining_arguments_set ~round:1 o2_arguments;
    use_inlining_arguments_set ~round:0 o1_arguments in
  "-O3", Arg.Unit action, " Apply aggressive optimization for speed (may \
    significantly increase code size and compilation time)"
;;

let rounds =
  "-rounds", set_some_int Clflags.simplify_rounds,
    Printf.sprintf "<n>  Repeat tree optimization and inlining phases this \
        many times (default %d).  Rounds are numbered starting from zero."
      !Clflags.default_simplify_rounds
;;

let inline_max_unroll =
  let action = Int_arg_helper.parse spec
      "Syntax: -inline-max-unroll <n> | <round>=<n>[,...]"
      inline_max_unroll in
  "-inline-max-unroll", Arg.String action,
    Printf.sprintf "<n>|<round>=<n>[,...]  Unroll recursive functions at most \
      this many times (default %d)"
      Clflags.default_inline_max_unroll
;;

let classic_inlining =
  "-Oclassic", set Clflags.classic_inlining,
  " Make inlining decisions at function definition \
   time rather than at the call site (replicates previous behaviour of the \
   compiler)"
;;

let inline_cost arg descr default r =
  let name ppf =   Printf.fprintf ppf "-inline-%s-cost" arg in
  let action =
    Int_arg_helper.parse spec
      "Syntax: %t <n> | <round>=<n>[,...]" name default
      r in
  Printf.sprintf "%t" name,
  Arg.int action,
  Printf.sprintf "<n>|<round>=<n>[,...]  The cost of not removing %s during \
      inlining (default %d, higher numbers more costly)"
    descr
    default
;;

let inline_call_cost =
  inline_cost "call" "a call" Clflags.default_inline_call_cost
    Clflags.inline_call_cost
let inline_alloc_cost =
  inline_cost "alloc" "an allocation" Clflags.default_inline_alloc_cost
    Clflags.default_inline_alloc_cost
let inline_prim_cost =
  inline_cost "prim" "a primitive" Clflags.default_inline_prim_cost
    Clflags.inline_prim_cost
let inline_branch_cost =
  inline_cost "branch" "a conditional" Clflags.default_inline_branch_cost
    Clflags.inline_branch_cost

let inline_indirect_cost =
  inline_cost "indirect" "an indirect call"
    Clflags.default_indirect_indirect_cost
    Clflags.inline_prim_cost

let inline_lifting_benefit =
  "-inline-lifting-benefit",
  Arg.String f,
  Printf.sprintf "<n>|<round>=<n>[,...]  The benefit of lifting definitions \
    to toplevel during inlining (default %d, higher numbers more beneficial)"
    Clflags.default_inline_lifting_benefit
;;

let inline_branch_factor =
  "-inline-branch-factor", Arg.String f,
    Printf.sprintf "<n>|<round>=<n>[,...]  Estimate the probability of a \
        branch being cold as 1/(1+n) (used for inlining) (default %.2f)"
    Clflags.default_inline_branch_factor
;;

let intf =
  "-intf", Arg.String f, "<file>  Compile <file> as a .mli file"
;;

let intf_suffix =
  "-intf-suffix", Arg.String f,
  "<string>  Suffix for interface files (default: .mli)"
;;

let intf_suffix_2 =
  "-intf_suffix", Arg.String f, "<string>  (deprecated) same as -intf-suffix"
;;

let insn_sched =
  "-insn-sched", Arg.Unit f,
  Printf.sprintf " Run the instruction scheduling pass%s"
    (if Clflags.insn_sched_default then " (default)" else "")
;;

let no_insn_sched =
  "-no-insn-sched", Arg.Unit f,
  Printf.sprintf " Do not run the instruction scheduling pass%s"
    (if not Clflags.insn_sched_default then " (default)" else "")
;;

let keep_docs =
  "-keep-docs", Arg.Unit f, " Keep documentation strings in .cmi files"
;;

let no_keep_docs =
  "-no-keep-docs", Arg.Unit f,
  " Do not keep documentation strings in .cmi files (default)"
;;

let keep_locs =
  "-keep-locs", Arg.Unit f, " Keep locations in .cmi files (default)"
;;

let no_keep_locs =
  "-no-keep-locs", Arg.Unit f, " Do not keep locations in .cmi files"
;;

let labels =
  "-labels", Arg.Unit f, " Use commuting label mode"
;;

let linkall =
  "-linkall", Arg.Unit f, " Link all modules, even unused ones"
;;

let linscan =
  "-linscan", Arg.Unit f, " Use the linear scan register allocator"
;;

let make_runtime =
  "-make-runtime", Arg.Unit f,
  " Build a runtime system with given C objects and libraries"
;;

let make_runtime_2 =
  "-make_runtime", Arg.Unit f, " (deprecated) same as -make-runtime"
;;

let inline_max_depth =
  "-inline-max-depth", Arg.String f,
    Printf.sprintf "<n>|<round>=<n>[,...]  Maximum depth of search for \
      inlining opportunities inside inlined functions (default %d)"
      Clflags.default_inline_max_depth
;;

let modern =
  "-modern", Arg.Unit f, " (deprecated) same as -labels"
;;

let alias_deps =
  "-alias-deps", Arg.Unit f,
  " Do record dependencies for module aliases"
;;

let no_alias_deps =
  "-no-alias-deps", Arg.Unit f,
  " Do not record dependencies for module aliases"
;;

let app_funct =
  "-app-funct", Arg.Unit f, " Activate applicative functors"
;;

let no_app_funct =
  "-no-app-funct", Arg.Unit f, " Deactivate applicative functors"
;;

let no_check_prims =
  "-no-check-prims", Arg.Unit f, " Do not check runtime for primitives"
;;

let no_float_const_prop =
  "-no-float-const-prop", Arg.Unit f,
  " Deactivate constant propagation for floating-point operations"
;;

let noassert =
  "-noassert", Arg.Unit f, " Do not compile assertion checks"
;;

let noautolink_byt =
  "-noautolink", Arg.Unit f,
  " Do not automatically link C libraries specified in .cma files"
;;

let noautolink_opt =
  "-noautolink", Arg.Unit f,
  " Do not automatically link C libraries specified in .cmxa files"
;;

let nodynlink =
  "-nodynlink", Arg.Unit f,
  " Enable optimizations for code that will not be dynlinked"
;;

let noinit =
  "-noinit", Arg.Unit f,
  " Do not load any init file"

let nolabels =
  "-nolabels", Arg.Unit f, " Ignore non-optional labels in types"
;;

let noprompt =
  "-noprompt", Arg.Unit f, " Suppress all prompts"
;;

let nopromptcont =
  "-nopromptcont", Arg.Unit f,
  " Suppress prompts for continuation lines of multi-line inputs"
;;

let nostdlib =
  "-nostdlib", Arg.Unit f,
  " Do not add default directory to the list of include directories"
;;

let no_unbox_free_vars_of_closures =
  "-no-unbox-free-vars-of-closures", Arg.Unit f,
  " Do not unbox variables that will appear inside function closures"
;;

let no_unbox_specialised_args =
  "-no-unbox-specialised-args", Arg.Unit f,
  " Do not unbox arguments to which functions have been specialised"
;;

let o =
  "-o", Arg.String f, "<file>  Set output file name to <file>"
;;

let open =
  "-open", Arg.String f, "<module>  Opens the module <module> before typing"

let output_obj =
  "-output-obj", Arg.Unit f, " Output an object file instead of an executable"
;;

let output_complete_obj =
  "-output-complete-obj", Arg.Unit f,
  " Output an object file, including runtime, instead of an executable"
;;

let p =
  "-p", Arg.Unit f, " (no longer supported)"
;;

let pack_byt =
  "-pack", Arg.Unit f, " Package the given .cmo files into one .cmo"
;;

let pack_opt =
  "-pack", Arg.Unit f, " Package the given .cmx files into one .cmx"
;;

let pp =
  "-pp", Arg.String f, "<command>  Pipe sources through preprocessor <command>"
;;

let ppx =
  "-ppx", Arg.String f,
  "<command>  Pipe abstract syntax trees through preprocessor <command>"
;;

let plugin =
  "-plugin", Arg.String f,
  "<plugin>  (no longer supported)"
;;

let principal =
  "-principal", Arg.Unit f, " Check principality of type inference"
;;

let no_principal =
  "-no-principal", Arg.Unit f,
  " Do not check principality of type inference (default)"
;;

let rectypes =
  "-rectypes", Arg.Unit f, " Allow arbitrary recursive types"
;;

let no_rectypes =
  "-no-rectypes", Arg.Unit f,
  " Do not allow arbitrary recursive types (default)"
;;

let remove_unused_arguments =
  "-remove-unused-arguments", Arg.Unit f,
  " Remove unused function arguments"
;;

let runtime_variant =
  "-runtime-variant", Arg.String f,
  "<str>  Use the <str> variant of the run-time system"
;;

let with_runtime =
  "-with-runtime", Arg.Unit f,
  "Include the runtime system in the generated program (default)"
;;

let without_runtime =
  "-without-runtime", Arg.Unit f,
  "Do not include the runtime system in the generated program."
;;

let S =
  "-S", Arg.Unit f, " Keep intermediate assembly file"
;;

let safe_string =
  "-safe-string", Arg.Unit f,
  if Config.safe_string then " (was set when configuring the compiler)"
  else if Config.default_safe_string then " Make strings immutable (default)"
  else " Make strings immutable"
;;

let shared =
  "-shared", Arg.Unit f, " Produce a dynlinkable plugin"
;;

let short_paths =
  "-short-paths", Arg.Unit f, " Shorten paths in types"
;;

let stdin =
  "-stdin", Arg.Unit f, " Read script from standard input"
;;

let no_strict_sequence =
  "-no-strict-sequence", Arg.Unit f,
  " Left-hand part of a sequence need not have type unit (default)"
;;

let strict_sequence =
  "-strict-sequence", Arg.Unit f,
  " Left-hand part of a sequence must have type unit"
;;

let thread =
  "-thread", Arg.Unit f,
  " (deprecated) same as -I +threads"
;;

let dtimings =
  "-dtimings", Arg.Unit f, " Print timings information for each pass";
;;

let dprofile =
  "-dprofile", Arg.Unit f, Profile.options_doc
;;

let unbox_closures =
  "-unbox-closures", Arg.Unit f,
  " Pass free variables via specialised arguments rather than closures"
;;

let unbox_closures_factor =
  "-unbox-closures-factor", Arg.Int f,
  Printf.sprintf "<n > 0>  Scale the size threshold above which \
      unbox-closures will slow down indirect calls rather than duplicating a \
      function (default %d)"
    Clflags.default_unbox_closures_factor
;;

let unboxed_types =
  "-unboxed-types", Arg.Unit f,
  " unannotated unboxable types will be unboxed"
;;

let no_unboxed_types =
  "-no-unboxed-types", Arg.Unit f,
  " unannotated unboxable types will not be unboxed (default)"
;;

let unsafe =
  "-unsafe", Arg.Unit f,
  " Do not compile bounds checking on array and string access"
;;

let unsafe_string =
  if Config.safe_string then
    let err () =
      raise (Arg.Bad "OCaml has been configured with -force-safe-string: \
                      -unsafe-string is not available")
    in
    "-unsafe-string", Arg.Unit err, " (option not available)"
  else if Config.default_safe_string then
    "-unsafe-string", Arg.Unit f, " Make strings mutable"
  else
    "-unsafe-string", Arg.Unit f, " Make strings mutable (default)"
;;

let use_runtime =
  "-use-runtime", Arg.String f,
  "<file>  Generate bytecode for the given runtime system"
;;

let use_runtime_2 =
  "-use_runtime", Arg.String f,
  "<file>  (deprecated) same as -use-runtime"
;;

let v =
  "-v", Arg.Unit f,
  " Print compiler version and location of standard library and exit"
;;

let verbose =
  "-verbose", Arg.Unit f, " Print calls to external commands"
;;

let version =
  "-version", Arg.Unit f, " Print version and exit"
;;

let _version =
  "--version", Arg.Unit f, " Print version and exit"
;;

let no_version =
  "-no-version", Arg.Unit f, " Do not print version at startup"
;;

let vmthread =
  "-vmthread", Arg.Unit f,
  "  (no longer supported)"
;;

let vnum =
  "-vnum", Arg.Unit f, " Print version number and exit"
;;

let mk_w =
  "-w", Arg.String f,
  Printf.sprintf
  "<list>  Enable or disable warnings according to <list>:\n\
  \        +<spec>   enable warnings in <spec>\n\
  \        -<spec>   disable warnings in <spec>\n\
  \        @<spec>   enable warnings in <spec> and treat them as errors\n\
  \     <spec> can be:\n\
  \        <num>             a single warning number\n\
  \        <num1>..<num2>    a range of consecutive warning numbers\n\
  \        <letter>          a predefined set\n\
  \     default setting is %S" Warnings.defaults_w
;;

let warn_error =
  "-warn-error", Arg.String f,
  Printf.sprintf
  "<list>  Enable or disable error status for warnings according\n\
  \     to <list>.  See option -w for the syntax of <list>.\n\
  \     Default setting is %S" Warnings.defaults_warn_error
;;

let warn_help =
  "-warn-help", Arg.Unit f, " Show description of warning numbers"
;;

let color =
  "-color", Arg.Symbol (["auto"; "always"; "never"], f),
  Printf.sprintf
  "  Enable or disable colors in compiler messages\n\
  \    The following settings are supported:\n\
  \      auto    use heuristics to enable colors only if supported\n\
  \      always  enable colors\n\
  \      never   disable colors\n\
  \    The default setting is 'auto', and the current heuristic\n\
  \    checks that the TERM environment variable exists and is\n\
  \    not empty or \"dumb\", and that isatty(stderr) holds.\n\
  \  If the option is not specified, these setting can alternatively\n\
  \  be set through the OCAML_COLOR environment variable."
;;

let error_style =
  "-error-style", Arg.Symbol (["contextual"; "short"], f),
  Printf.sprintf
    "  Control the way error messages and warnings are printed\n\
    \    The following settings are supported:\n\
    \      short       only print the error and its location\n\
    \      contextual  like \"short\", but also display the source code\n\
    \                  snippet corresponding to the location of the error\n\
    \    The default setting is 'contextual'.\n\
    \  If the option is not specified, these setting can alternatively\n\
    \  be set through the OCAML_ERROR_STYLE environment variable."
;;

let where =
  "-where", Arg.Unit f, " Print location of standard library and exit"
;;

let nopervasives =
  "-nopervasives", Arg.Unit f, " (undocumented)"
;;

let match_context_rows =
  "-match-context-rows", Arg.Int f,
  let[@manual.ref "s:comp-options"] chapter, section = 9, 2 in
  Printf.sprintf
  "<n>  (advanced, see manual section %d.%d.)" chapter section
;;

let use_prims =
  "-use-prims", Arg.String f, "<file>  (undocumented)"
;;

let dump_into_file =
  "-dump-into-file", Arg.Unit f, " dump output like -dlambda into <target>.dump"
;;

let dparsetree =
  "-dparsetree", Arg.Unit f, " (undocumented)"
;;

let dtypedtree =
  "-dtypedtree", Arg.Unit f, " (undocumented)"
;;

let drawlambda =
  "-drawlambda", Arg.Unit f, " (undocumented)"
;;

let dno_unique_ids =
  "-dno-unique-ids", Arg.Unit f, " (undocumented)"
;;

let dunique_ids =
  "-dunique-ids", Arg.Unit f, " (undocumented)"
;;

let dsource =
  "-dsource", Arg.Unit f, " (undocumented)"
;;

let dlambda =
  "-dlambda", Arg.Unit f, " (undocumented)"
;;

let drawclambda =
  "-drawclambda", Arg.Unit f, " (undocumented)"
;;

let dclambda =
  "-dclambda", Arg.Unit f, " (undocumented)"
;;

let dflambda =
  "-dflambda", Arg.Unit f, " Print Flambda terms"
;;

let drawflambda =
  "-drawflambda", Arg.Unit f, " Print Flambda terms after closure conversion"
;;

let dflambda_invariants =
  "-dflambda-invariants", Arg.Unit f, " Check Flambda invariants \
      around each pass"
;;

let dflambda_no_invariants =
  "-dflambda-no-invariants", Arg.Unit f, " Do not Check Flambda invariants \
      around each pass"
;;

let dflambda_let =
  "-dflambda-let", Arg.Int f, "<stamp>  Print when the given Flambda [Let] \
      is created"
;;

let dflambda_verbose =
  "-dflambda-verbose", Arg.Unit f, " Print Flambda terms including around \
      each pass"
;;

let dinstr =
  "-dinstr", Arg.Unit f, " (undocumented)"
;;

let dcamlprimc =
  "-dcamlprimc", Arg.Unit f, " (undocumented)"
;;

let dcmm =
  "-dcmm", Arg.Unit f, " (undocumented)"
;;

let dsel =
  "-dsel", Arg.Unit f, " (undocumented)"
;;

let dcombine =
  "-dcombine", Arg.Unit f, " (undocumented)"
;;

let dcse =
  "-dcse", Arg.Unit f, " (undocumented)"
;;

let dlive =
  "-dlive", Arg.Unit f, " (undocumented)"
;;

let davail =
  "-davail", Arg.Unit f, " Print register availability info when printing \
    liveness"
;;

let drunavail =
  "-drunavail", Arg.Unit f, " Run register availability pass (for testing \
    only; needs -g)"
;;

let dspill =
  "-dspill", Arg.Unit f, " (undocumented)"
;;

let dsplit =
  "-dsplit", Arg.Unit f, " (undocumented)"
;;

let dinterf =
  "-dinterf", Arg.Unit f, " (undocumented)"
;;

let dprefer =
  "-dprefer", Arg.Unit f, " (undocumented)"
;;

let dalloc =
  "-dalloc", Arg.Unit f, " (undocumented)"
;;

let dreload =
  "-dreload", Arg.Unit f, " (undocumented)"
;;

let dscheduling =
  "-dscheduling", Arg.Unit f, " (undocumented)"
;;

let dlinear =
  "-dlinear", Arg.Unit f, " (undocumented)"
;;

let dinterval =
  "-dinterval", Arg.Unit f, " (undocumented)"
;;

let dstartup =
  "-dstartup", Arg.Unit f, " (undocumented)"
;;

let opaque =
  "-opaque", Arg.Unit f,
  " Does not generate cross-module optimization information\n\
  \     (reduces necessary recompilation on module change)"
;;

let strict_formats =
  "-strict-formats", Arg.Unit f,
  " Reject invalid formats accepted by legacy implementations\n\
  \     (Warning: Invalid formats may behave differently from\n\
  \      previous OCaml versions, and will become always-rejected\n\
  \      in future OCaml versions. You should always use this flag\n\
  \      to detect invalid formats so you can fix them.)"

let no_strict_formats =
  "-no-strict-formats", Arg.Unit f,
  " Accept invalid formats accepted by legacy implementations (default)\n\
  \     (Warning: Invalid formats may behave differently from\n\
  \      previous OCaml versions, and will become always-rejected\n\
  \      in future OCaml versions. You should never use this flag\n\
  \      and instead fix invalid formats.)"
;;

let args =
  "-args", Arg.Expand f,
  "<file> Read additional newline-terminated command line arguments\n\
  \      from <file>"
;;

let args0 =
  "-args0", Arg.Expand f,
  "<file> Read additional null character terminated command line arguments\n\
          from <file>"
;;

let afl_instrument =
  "-afl-instrument", Arg.Unit f, "Enable instrumentation for afl-fuzz"
;;

let afl_inst_ratio =
  "-afl-inst-ratio", Arg.Int f,
  "Configure percentage of branches instrumented\n\
  \     (advanced, see afl-fuzz docs for AFL_INST_RATIO)"
;;

let __ =
  "-", Arg.String f,
  "<file>  Treat <file> as a file name (even if it starts with `-')"
;;
