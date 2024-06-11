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
      let pull = function
        | Doc.H -> h
        | Doc.V -> v
        | Doc.HoV -> hov
        | Doc.HV -> hv
        | Doc.B -> b
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
    let map: (Obj.Extension_constructor.t, Format.stag -> raw_type) Hashtbl.t =
      Hashtbl.create 5
    let register_tag ext conv = Hashtbl.replace map ext conv
    let typ =
      let pull = function
        | Format.String_tag s -> string_tag s
        | x ->
            let ext = Obj.Extension_constructor.of_val x in
            match Hashtbl.find map ext with
            | exception Not_found ->
                unknown (Obj.Extension_constructor.name ext)
            | f -> f x
      in
      Custom { id = Format_tag; pull; default = raw_type}

    let register_tag0 v ext =
      let name = Obj.Extension_constructor.name ext in
      let name = match String.rindex name '.' with
        | exception Not_found -> name
        | dot -> String.sub name (dot+1) (String.length name - dot -1)
      in
      let constr = new_constr0 v name in
      register_tag ext (fun _ -> constr)

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
    let elt_pull = function
      | Doc.Text x -> text x
      | Doc.With_size x -> with_size x
      | Doc.Open_box r -> open_box (r.kind, r.indent)
      | Doc.Close_box -> close_box
      | Doc.Open_tag t -> open_tag t
      | Doc.Close_tag -> close_tag
      | Doc.Open_tbox -> open_tbox
      | Doc.Close_tbox -> close_tbox
      | Doc.Tab_break t -> tab_break (t.width,t.offset)
      | Doc.Set_tab -> set_tab
      | Doc.Simple_break r -> simple_break (r.spaces, r.indent)
      | Doc.Break r -> break (r.fits, r.breaks)
      | Doc.Flush r -> flush r.newline
      | Doc.Newline -> newline
      | Doc.If_newline -> if_newline
      | Doc.Deprecated _ -> deprecated
    in
    let default = List raw_type in
    let pull d =
      List.rev @@
      Format_doc.Doc.fold (fun l x -> elt_pull x :: l ) [] d in
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
  let output = new_field v1 "output" doc
  let backtrace = new_field_opt v1 "backtrace" doc
  let compiler_log = new_field_opt v1 "compiler_log" Compiler.raw_type
  let errors = new_field_opt v1 "errors" ldoc
  let trace = new_field_opt v1 "trace" ldoc
  let () = seal v1
end
