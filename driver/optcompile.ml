(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2002 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** The batch compiler *)

open Compile_common

let tool_name = "ocamlopt"

let with_info =
  Compile_common.with_info ~native:true ~tool_name

let interface ~log ~source_file ~output_prefix =
  with_info ~source_file ~output_prefix ~log ~dump_ext:"cmi" @@ fun info ->
  Compile_common.interface info

let (|>>) (x, y) f = (x, f y)

let log_if i key flag printer x =
  Log.log_if i.debug_log key !flag printer x; x
module D = Reports.Debug

(** Native compilation backend for .ml files. *)

let flambda i backend Typedtree.{structure; coercion; _} =
  if !Clflags.classic_inlining then begin
    Clflags.default_simplify_rounds := 1;
    Clflags.use_inlining_arguments_set Clflags.classic_arguments;
    Clflags.unbox_free_vars_of_closures := false;
    Clflags.unbox_specialised_args := false
  end;

  (structure, coercion)
  |> Profile.(record transl)
      (Translmod.transl_implementation_flambda (Unit_info.modname i.target))
  |> Profile.(record generate)
    (fun {Lambda.module_ident; main_module_block_size;
          required_globals; code } ->
      let () =
        let (module_ident, main_module_block_size), code =
          ((module_ident, main_module_block_size), code)
          |>> log_if i D.raw_lambda Clflags.dump_rawlambda Printlambda.lambda
          |>> Simplif.simplify_lambda
          |>> log_if i D.lambda Clflags.dump_lambda Printlambda.lambda
        in

        if Clflags.(should_stop_after Compiler_pass.Lambda) then () else (
          let program : Lambda.program =
            { Lambda.
              module_ident;
              main_module_block_size;
              required_globals;
              code;
            }
          in
          Asmgen.compile_implementation
            ~backend
            ~prefixname:(Unit_info.prefix i.target)
            ~middle_end:Flambda_middle_end.lambda_to_clambda
            ~log:i.debug_log
            program)
      in
      Compilenv.save_unit_info Unit_info.(Artifact.filename @@ cmx i.target))


let clambda i backend Typedtree.{structure; coercion; _} =
  Clflags.use_inlining_arguments_set Clflags.classic_arguments;
  (structure, coercion)
  |> Profile.(record transl)
    (Translmod.transl_store_implementation (Unit_info.modname i.target))
  |> log_if i D.raw_lambda Clflags.dump_rawlambda Printlambda.program
  |> Profile.(record generate)
    (fun program ->
       let code = Simplif.simplify_lambda program.Lambda.code in
       { program with Lambda.code }
       |> log_if i D.lambda Clflags.dump_lambda Printlambda.program
       |>(fun lambda ->
           if Clflags.(should_stop_after Compiler_pass.Lambda) then () else
             Asmgen.compile_implementation
               ~backend
               ~prefixname:(Unit_info.prefix i.target)
               ~middle_end:Closure_middle_end.lambda_to_clambda
               ~log:i.debug_log
               lambda;
           Compilenv.save_unit_info
             Unit_info.(Artifact.filename @@ cmx i.target)))


(* Emit assembly directly from Linear IR *)
let emit i =
  Compilenv.reset ?packname:!Clflags.for_package (Unit_info.modname i.target);
  Asmgen.compile_implementation_linear i.target

let implementation ~backend ~log ~start_from ~source_file ~output_prefix =
  let backend info typed =
    Compilenv.reset ?packname:!Clflags.for_package
      (Unit_info.modname info.target);
    if Config.flambda
    then flambda info backend typed
    else clambda info backend typed
  in
  with_info ~source_file ~output_prefix ~log ~dump_ext:"cmx" @@ fun info ->
  match (start_from:Clflags.Compiler_pass.t) with
  | Parsing -> Compile_common.implementation info ~backend
  | Emit -> emit info
  | _ -> Misc.fatal_errorf "Cannot start from %s"
           (Clflags.Compiler_pass.to_string start_from)
