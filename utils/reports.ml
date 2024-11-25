(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Florian Angeletti, projet Cambium, Inria Paris             *)
(*                                                                        *)
(*   Copyright 2024 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Diagnostic
module V = Diagnostic_history.Make()
let v1 = V.v1

module type Record = Record with type vl := V.id
module type Sum = Sum with type vl := V.id

type doc = Format_doc.t
module Structured_text = struct
  module Doc = Format_doc.Doc
  module Box_type = struct
    include New_sum(V)
        (struct
          let name = "box_type"
          let update = v1
        end
        )()
    let h = new_constr0 v1 "H"
    let v = new_constr0 v1 "V"
    let hv = new_constr0 v1 "HV"
    let hov = new_constr0 v1 "HoV"
    let b = new_constr0 v1 "B"
    let () = seal v1
    type _ extension += Box_type: Doc.box_type extension
    let typ =
      let pull version = function
        | Doc.H -> app version h ()
        | Doc.V -> app version v ()
        | Doc.HoV -> app version hov ()
        | Doc.HV -> app version hv ()
        | Doc.B -> app version b ()
      in
      Custom { id = Box_type; pull; default = raw_type}
  end

  module Format_tag = struct
    include New_sum(V)
        (struct
          let name = "format_tag"
          let update = v1
        end
        )()

    let unknown = new_constr v1 "<Unknown>" String
    let string_tag = new_constr v1 "String_tag" String

    type _ extension += Format_tag: Format.stag extension
    type format_tag_serializer =
      Diagnostic_history.version option -> Format.stag -> raw_type
    let map: (Obj.Extension_constructor.t, format_tag_serializer) Hashtbl.t =
      Hashtbl.create 5
    let register_tag ext conv = Hashtbl.replace map ext conv
    let typ =
      let pull v = function
        | Format.String_tag s -> app v string_tag s
        | x ->
            let ext = Obj.Extension_constructor.of_val x in
            match Hashtbl.find map ext with
            | exception Not_found ->
                app v unknown (Obj.Extension_constructor.name ext)
            | f -> f v x
      in
      Custom { id = Format_tag; pull; default = raw_type}

    let register_tag0 v ext =
      let name = Obj.Extension_constructor.name ext in
      let name = match String.rindex name '.' with
        | exception Not_found -> name
        | dot -> String.sub name (dot+1) (String.length name - dot -1)
      in
      let constr = new_constr0 v name in
      register_tag ext (fun v _ -> app v constr ())

   let () =
      Array.iter (register_tag0 v1)
        Misc.Style.[|
          [%extension_constructor Error];
          [%extension_constructor Warning];
          [%extension_constructor Loc];
          [%extension_constructor Inline_code];
          [%extension_constructor Hint];
          [%extension_constructor Deletion];
          [%extension_constructor Insertion];
          [%extension_constructor Modification];
          [%extension_constructor Preservation];
        |];
      seal v1

  end


  include New_sum(V)
    (struct
      let name = "structured_text"
      let update = v1
    end)
    ()

  let text = new_constr v1 "Text" String
  let tab_break = new_constr v1 "Tab_break" (Pair(Int,Int))
  let set_tab = new_constr0 v1 "Set_tab"
  let simple_break = new_constr v1 "Simple_break" (Pair(Int,Int))
  let break =
    let alt = Triple(String,Int,String) in
    new_constr v1 "Break" (Pair(alt,alt))
  let flush = new_constr v1 "Flush" Bool
  let newline = new_constr0 v1 "Newline"
  let if_newline = new_constr0 v1 "If_newline"



  let deprecated = new_constr0 v1 "<deprecated>"

  type _ extension += Doc: Doc.t extension
  let with_size = new_constr v1 "With_size" (Pair(Int,raw_type))
  let box = new_constr v1 "Box" (Triple(Box_type.typ,Int,List raw_type))
  let tag = new_constr v1 "Tag" (Pair(Format_tag.typ,List raw_type))
  let tbox = new_constr v1 "Tbox" (List raw_type)

  let typ =
    let rec tree_pull v =
      let open Doc.Tree in
      function
      | Core (Text x) -> app v text x
      | With_size {size;subtree} -> app v with_size (size, tree_pull v subtree)
      | Box r -> app v box (r.kind, r.indent, trees v r.subtrees)
      | Tagged t -> app v tag (t.tag, trees v t.subtrees)
      | Tbox s -> app v tbox (trees v s)
      | Core (Tab_break t) -> app v tab_break (t.width,t.offset)
      | Core Set_tab -> app v set_tab ()
      | Core (Simple_break r) -> app v simple_break (r.spaces, r.indent)
      | Core (Break r) -> app v break (r.fits, r.breaks)
      | Core (Flush r) -> app v flush r.newline
      | Core Newline -> app v newline ()
      | Core If_newline -> app v if_newline ()
      | Core (Deprecated _) -> app v deprecated ()
    and trees v = List.map (tree_pull v) in
    let default = List raw_type in
    let pull v d = trees v (Doc.Tree.parse d) in
    Custom {id = Doc; default; pull }
  let () = seal v1



  let register_tag = Format_tag.register_tag
  let register_tag0 = Format_tag.register_tag0

 end



module Debug = struct
  let v1 = V.v1
  include New_record(V)
      (struct
        let name = "debug"
        let update = v1
      end)
      ()

  let slist = List String

  let parsetree = new_field_opt v1 "parsetree" String
  let source = new_field_opt v1 "source" String
  let typedtree = new_field_opt v1 "typedtree" String
  let shape = new_field_opt v1 "shape" String
  let instr = new_field_opt v1 "instr" String
  let lambda = new_field_opt v1 "lambda" String
  let raw_lambda = new_field_opt v1 "raw_lambda" String
  let flambda = new_field_opt v1 "flambda" slist
  let raw_flambda = new_field_opt v1 "raw_flambda" slist
  let clambda = new_field_opt v1 "clambda" slist
  let raw_clambda = new_field_opt v1 "raw_clambda" slist
  let cmm = new_field_opt v1 "cmm" slist
  let remove_free_vars_equal_to_args =
    new_field_opt v1 "remove-free-vars-equal-to-args" slist
  let unbox_free_vars_of_closures =
    new_field_opt v1 "unbox-free-vars-of-closures" slist
  let unbox_closures = new_field_opt v1 "unbox-closures" slist
  let unbox_specialised_args = new_field_opt v1 "unbox-specialised-args" slist
  let mach = new_field_opt v1 "mach" slist
  let linear = new_field_opt v1 "linear" slist
  let cmm_invariant = new_field_opt v1 "cmm_invariant" String
end

module Error =
  New_record(V)
    (struct
      let name = "error_report"
      let update = v1
    end)
    ()

module Compiler = struct
  include New_record(V)
      (struct
        let name = "compiler"
        let update = v1
      end)
      ()
  let debug = new_field_opt v1  "debug" (Record Debug.scheme)
end

let doc = Structured_text.typ
let ldoc = List Structured_text.typ
module Toplevel = struct
  include New_record(V)
      (struct
        let name = "toplevel"
        let update = v1
      end)
      ()
  let output = new_field_opt v1 "output" doc
  let backtrace = new_field_opt v1 "backtrace" doc
  let compiler_log = new_field_opt v1 "compiler_log" Compiler.raw_type
  let errors = new_field_opt v1 "errors" ldoc
  let trace = new_field_opt v1 "trace" ldoc
  let () = seal v1
end



module Config_versions = Diagnostic_history.Make()
module Config = struct
  let v1 = Config_versions.v1
  include Diagnostic.New_record(Config_versions)(struct
    let name = "config"
    let update = v1
    end)()
  open Diagnostic
  let set_config = Dynarray.create 32
  let new_field name typ config =
    let f = new_field v1 name typ in
    Dynarray.add_last set_config (fun log -> log.%[f] <- config);
    f
  open Config

  let version = new_field "version" String version
  let standard_library_default =
    new_field "standard_library_default" String standard_library_default
  let standard_library = new_field v1 "standard_library" String standard_library
  let ccomp_type = new_field v1 "ccomp_type" String ccomp_typ
  let c_compiler = new_field v1 "c_compiler" String c_compiler
  let ocamlc_cflags = new_field v1 "ocamlc_cflags" String ocamlc_cglags
  let ocamlc_cppflags = new_field v1 "ocamlc_cppflags" String ocamlc_cppflags
  let ocamlopt_cflags = new_field v1 "ocamlopt_cflags" String ocamlc_cflags
  let ocamlopt_cppflags =
    new_field v1 "ocamlopt_cppflags" String ocamlc_cppflags
(* bytecomp_c_compiler and native_c_compiler have been supported for a
   long time and are retained for backwards compatibility.
   For programs that don't need compatibility with older OCaml releases
   the recommended approach is to use the constituent variables
   c_compiler, ocamlc_cflags, ocamlc_cppflags etc., directly.
*)
  let old_c_compiler =
    String.concat " " [c_compiler; ocamlc_clfags; ocamlc_cppflags]
  let bytecomp_c_compiler = new_field"bytecomp_c_compiler" String old_c_compiler
  let native_c_compiler = new_field "native_c_compiler" String old_c_compiler
  let bytecomp_c_libraries =
    new_field "bytecomp_c_libraries" String bytecomp_c_libraries
  let native_c_libraries =
    new_field "native_c_libraries" String native_c_libraries
  let native_ldflags = new_field "native_ldflags" String native_ldflags
  let native_pack_linker =
    new_field "native_pack_linker" String native_pack_linker
  let native_compiler = new_field "native_compiler" Bool native_compiler
  let architecture = new_field "architecture" String architecture
  let model = new_field "model" String model
  let int_size = new_field "int_size" Int int_size
  let word_size = new_field "word_size" Int word_size
  let system = new_field "system" String system
  let asm = new_field "asm" String asm
  let asm_cfi_supported = new_field "asm_cfi_supported" Bool asm_cfi_supported
  let with_frame_pointers =
    new_field "with_frame_pointers" Bool with_frame_pointer
  let ext_exe = new_field "ext_exe" String ext_exe
  let ext_obj = new_field "ext_obj" String ext_obj
  let ext_asm = new_field "ext_asm" String ext_asm
  let ext_lib = new_field "ext_lib" String ext_lib
  let ext_dll = new_field "ext_dll" String ext_dll
  let os_type = new_field "os_type" String os_type
  let default_executable_name =
    new_field "default_executable_name" String default_executable_name
  let systhread_supported =
    new_field "systhread_supported" Bool systhread_supported
  let host = new_field "host" String host
  let target = new_field "target" String target
  let flambda = new_field "flambda" Bool flambda
  let safe_string = new_field "safe_string" Bool safe_string
  let default_safe_string =
    new_field "default_safe_string" Bool default_safe_string
  let flat_float_array = new_field "flat_float_array" Bool flat_float_array
  let function_sections = new_field "function_sections" Bool function_sections
  let afl_instrument = new_field "afl_instrument" Bool afl_instrument
  let tsan = new_field "tsan" Bool tsan
  let windows_unicode = new_field "windows_unicode" Bool windows_unicode
  let supports_shared_libraries =
    new_field "supports_shared_libraries" Bool supports_shared_libraries
  let native_dynlink = new_field "native_dynlink" Bool native_dynlink
  let naked_pointers = new_field "naked_pointers" Bool naked_pointers

  let exec_magic_number = new_field "exec_magic_number" String exec_magic_number
  let cmi_magic_number = new_field "cmi_magic_number" String cmi_magic_number
  let cmo_magic_number = new_field "cmo_magic_number" String cmo_magic_number
  let cma_magic_number = new_field "cma_magic_number" String cma_magic_number
  let cmx_magic_number = new_field "cmx_magic_number" String cmx_magic_number
  let cmxa_magic_number = new_field "cmxa_magic_number" String cmxa_magic_number
  let ast_impl_magic_number =
    new_field "ast_impl_magic_number" String ast_impl_magic_number
  let ast_intf_magic_number =
    new_field "ast_intf_magic_number" String ast_intf_magic_number
  let cmxs_magic_number = new_field "cmxs_magic_number" String cmxs_magic_number
  let cmt_magic_number = new_field "cmt_magic_number" String cmt_magic_number
  let linear_magic_number =
    new_field "linear_magic_number" String linear_magic_number

let log_variables log =
  Dynarray.iter (fun f -> f log) set_config

let print_config log =
  log_variables log;
  Log.flush log

let config_var x =
  let log = Log.tmp scheme in
  let () = log_variables log in
  match Log.dynamic_get x log with
  | None -> None
  | Some (Diagnostic.V (ty,v)) ->
      let s = match ty with
        | Diagnostic.String -> (v:string)
        | Diagnostic.Int -> Int.to_string v
        | Diagnostic.Bool -> string_of_bool v
        | Diagnostic.Unit -> "()"
        | _ -> assert false
      in
      Some s

let show_config_variable_and_exit x =
  match config_var x with
  | Some v ->
      (* we intentionally don't print a newline to avoid Windows \r
         issues: bash only strips the trailing \n when using a command
         substitution $(ocamlc -config-var foo), so a trailing \r would
         remain if printing a newline under Windows and scripts would
         have to use $(ocamlc -config-var foo | tr -d '\r')
         for portability. Ugh. *)
      print_string v;
      exit 0
  | None ->
      exit 2


end
