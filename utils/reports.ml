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

open Log
module V = New_root()
let v1 = V.v1

module type Record = Log.Record with type vl := V.id
module type Sum = Log.Sum with type vl := V.id

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
    type format_tag_serializer = Version.t -> Format.stag -> raw_type
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
  let with_size = new_constr v1 "With_size" Int
  let open_box = new_constr v1 "Open_box" (Pair(Box_type.typ,Int))
  let close_box = new_constr0 v1 "Close_box"
  let open_tag = new_constr v1 "Open_tag" Format_tag.typ
  let close_tag = new_constr0 v1 "Close_tag"
  let open_tbox = new_constr0 v1 "Open_tbox"
  let close_tbox = new_constr0 v1 "Close_tbox"
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
  let () = seal v1

  type _ extension += Doc: Doc.t extension
  let typ =
    let elt_pull v = function
      | Doc.Text x -> app v text x
      | Doc.With_size x -> app v with_size x
      | Doc.Open_box r -> app v open_box (r.kind, r.indent)
      | Doc.Close_box -> app v close_box ()
      | Doc.Open_tag t -> app v open_tag t
      | Doc.Close_tag -> app v close_tag ()
      | Doc.Open_tbox -> app v open_tbox ()
      | Doc.Close_tbox -> app v close_tbox ()
      | Doc.Tab_break t -> app v tab_break (t.width,t.offset)
      | Doc.Set_tab -> app v set_tab ()
      | Doc.Simple_break r -> app v simple_break (r.spaces, r.indent)
      | Doc.Break r -> app v break (r.fits, r.breaks)
      | Doc.Flush r -> app v flush r.newline
      | Doc.Newline -> app v newline ()
      | Doc.If_newline -> app v if_newline ()
      | Doc.Deprecated _ -> app v deprecated ()
    in
    let default = List raw_type in
    let pull v d =
      List.rev @@
      Format_doc.Doc.fold (fun l x -> elt_pull v x :: l ) [] d in
    Custom {id = Doc; default; pull }

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
  let profile = new_field_opt v1 "profile" String
  let () = seal v1
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



module Config_versions = Log.New_root()
module Config = struct
  let v1 = Config_versions.v1
  include Log.New_record(Config_versions)(struct
    let name = "config"
    let update = v1
    end)()
  open Log
  let version = new_field v1 "version" String
  let standard_library_default =  new_field v1 "standard_library_default" String
  let standard_library = new_field v1 "standard_library" String
  let ccomp_type = new_field v1 "ccomp_type" String
  let c_compiler = new_field v1 "c_compiler" String
  let ocamlc_cflags = new_field v1 "ocamlc_cflags" String
  let ocamlc_cppflags = new_field v1 "ocamlc_cppflags" String
  let ocamlopt_cflags = new_field v1 "ocamlopt_cflags" String
  let ocamlopt_cppflags = new_field v1 "ocamlopt_cppflags" String
  let bytecomp_c_compiler = new_field v1 "bytecomp_c_compiler" String
  let native_c_compiler = new_field v1 "native_c_compiler" String
  let bytecomp_c_libraries = new_field v1 "bytecomp_c_libraries" String
  let native_c_libraries = new_field v1 "native_c_libraries" String
  let native_ldflags = new_field v1 "native_ldflags" String
  let native_pack_linker = new_field v1 "native_pack_linker" String
  let native_compiler = new_field v1 "native_compiler" Bool
  let architecture = new_field v1 "architecture" String
  let model = new_field v1 "model" String
  let int_size = new_field v1 "int_size" Int
  let word_size = new_field v1 "word_size" Int
  let system = new_field v1 "system" String
  let asm = new_field v1 "asm" String
  let asm_cfi_supported = new_field v1 "asm_cfi_supported" Bool
  let with_frame_pointers = new_field v1 "with_frame_pointers" Bool
  let ext_exe = new_field v1 "ext_exe" String
  let ext_obj = new_field v1 "ext_obj" String
  let ext_asm = new_field v1 "ext_asm" String
  let ext_lib = new_field v1 "ext_lib" String
  let ext_dll = new_field v1 "ext_dll" String
  let os_type = new_field v1 "os_type" String
  let default_executable_name = new_field v1 "default_executable_name" String
  let systhread_supported = new_field v1 "systhread_supported" Bool
  let host = new_field v1 "host" String
  let target = new_field v1 "target" String
  let flambda = new_field v1 "flambda" Bool
  let safe_string = new_field v1 "safe_string" Bool
  let default_safe_string = new_field v1 "default_safe_string" Bool
  let flat_float_array = new_field v1 "flat_float_array" Bool
  let function_sections = new_field v1 "function_sections" Bool
  let afl_instrument = new_field v1 "afl_instrument" Bool
  let tsan = new_field v1 "tsan" Bool
  let windows_unicode = new_field v1 "windows_unicode" Bool
  let supports_shared_libraries = new_field v1 "supports_shared_libraries" Bool
  let native_dynlink = new_field v1 "native_dynlink" Bool
  let naked_pointers = new_field v1 "naked_pointers" Bool

  let exec_magic_number = new_field v1 "exec_magic_number" String
  let cmi_magic_number = new_field v1 "cmi_magic_number" String
  let cmo_magic_number = new_field v1 "cmo_magic_number" String
  let cma_magic_number = new_field v1 "cma_magic_number" String
  let cmx_magic_number = new_field v1 "cmx_magic_number" String
  let cmxa_magic_number = new_field v1 "cmxa_magic_number" String
  let ast_impl_magic_number = new_field v1 "ast_impl_magic_number" String
  let ast_intf_magic_number = new_field v1 "ast_intf_magic_number" String
  let cmxs_magic_number = new_field v1 "cmxs_magic_number" String
  let cmt_magic_number = new_field v1 "cmt_magic_number" String
  let linear_magic_number = new_field v1 "linear_magic_number" String


let log_variables log =
  let open Log in
  log.%[version] <- Config.version;
  log.%[standard_library_default] <- Config.standard_library_default;
  log.%[standard_library] <- Config.standard_library;
  log.%[ccomp_type] <- Config.ccomp_type;
  log.%[c_compiler] <- Config.c_compiler;
  log.%[ocamlc_cflags] <- Config.ocamlc_cflags;
  log.%[ocamlc_cppflags] <- Config.ocamlc_cppflags;
  log.%[ocamlopt_cflags] <- Config.ocamlc_cflags;
  log.%[ocamlopt_cppflags] <- Config.ocamlc_cppflags;
(* bytecomp_c_compiler and native_c_compiler have been supported for a
   long time and are retained for backwards compatibility.
   For programs that don't need compatibility with older OCaml releases
   the recommended approach is to use the constituent variables
   c_compiler, ocamlc_cflags, ocamlc_cppflags etc., directly.
*)
  log.%[bytecomp_c_compiler] <-
   Config.(c_compiler ^ " " ^ ocamlc_cflags ^ " " ^ ocamlc_cppflags);
  log.%[native_c_compiler] <-
    Config.(c_compiler ^ " " ^ ocamlc_cflags ^ " " ^ ocamlc_cppflags);
  log.%[bytecomp_c_libraries] <- Config.bytecomp_c_libraries;
  log.%[native_c_libraries] <- Config.native_c_libraries;
  log.%[native_ldflags] <- Config.native_ldflags;
  log.%[native_pack_linker] <- Config.native_pack_linker;
  log.%[native_compiler] <- Config.native_compiler;
  log.%[architecture] <- Config.architecture;
  log.%[model] <- Config.model;
  log.%[int_size] <- Sys.int_size;
  log.%[word_size] <- Sys.word_size;
  log.%[system] <- Config.system;
  log.%[asm] <- Config.asm;
  log.%[asm_cfi_supported] <- Config.asm_cfi_supported;
  log.%[with_frame_pointers] <- Config.with_frame_pointers;
  log.%[ext_exe] <- Config.ext_exe;
  log.%[ext_obj] <- Config.ext_obj;
  log.%[ext_asm] <- Config.ext_asm;
  log.%[ext_lib] <- Config.ext_lib;
  log.%[ext_dll] <- Config.ext_dll;
  log.%[os_type] <- Sys.os_type;
  log.%[default_executable_name] <- Config.default_executable_name;
  log.%[systhread_supported] <- Config.systhread_supported;
  log.%[host] <- Config.host;
  log.%[target] <- Config.target;
  log.%[flambda] <- Config.flambda;
  log.%[safe_string] <- Config.safe_string;
  log.%[default_safe_string] <- Config.default_safe_string;
  log.%[flat_float_array] <- Config.flat_float_array;
  log.%[function_sections] <- Config.function_sections;
  log.%[afl_instrument] <- Config.afl_instrument;
  log.%[tsan] <- Config.tsan;
  log.%[windows_unicode] <- Config.windows_unicode;
  log.%[supports_shared_libraries] <- Config.supports_shared_libraries;
  log.%[native_dynlink] <- Config.native_dynlink;
  log.%[naked_pointers] <- Config.naked_pointers;

  log.%[exec_magic_number] <- Config.exec_magic_number;
  log.%[cmi_magic_number] <- Config.cmi_magic_number;
  log.%[cmo_magic_number] <- Config.cmo_magic_number;
  log.%[cma_magic_number] <- Config.cma_magic_number;
  log.%[cmx_magic_number] <- Config.cmx_magic_number;
  log.%[cmxa_magic_number] <- Config.cmxa_magic_number;
  log.%[ast_impl_magic_number] <- Config.ast_impl_magic_number;
  log.%[ast_intf_magic_number] <- Config.ast_intf_magic_number;
  log.%[cmxs_magic_number] <- Config.cmxs_magic_number;
  log.%[cmt_magic_number] <- Config.cmt_magic_number;
  log.%[linear_magic_number] <- Config.linear_magic_number

let print_config log =
  log_variables log;
  Log.flush log

let config_var x =
  let log = Log.tmp scheme in
  let () = log_variables log in
  match Log.dynamic_get x log with
  | None -> None
  | Some (Log.V (ty,v)) ->
      let s = match ty with
        | Log.String -> (v:string)
        | Log.Int -> Int.to_string v
        | Log.Bool -> string_of_bool v
        | Log.Unit -> "()"
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
