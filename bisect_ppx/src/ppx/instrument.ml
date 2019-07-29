open Ast_helper
open Ast_mapper
open Asttypes
open Parsetree
module Common = Common
module Pat = Pat
module Exp = Exp
module Str = Str
module Cf = Cf
let conv_int ?loc  ?attrs  x =
  Exp.constant ?loc ?attrs (Pconst_integer ((string_of_int x), None))
let conv_str ?loc  ?attrs  s =
  Exp.constant ?loc ?attrs (Pconst_string (s, None))
let conv_lid ?(loc= !default_loc)  s = Location.mkloc (Longident.parse s) loc
module Generated_code :
  sig
    type points
    val init : unit -> points
    val instrument_expr :
      points ->
        ?override_loc:Location.t ->
          Parsetree.expression -> Parsetree.expression
    val instrument_case : points -> Parsetree.case -> Parsetree.case
    val instrument_class_field_kind :
      points -> Parsetree.class_field_kind -> Parsetree.class_field_kind
    val runtime_initialization :
      points -> string -> Parsetree.structure_item list
  end =
  struct
    type points = Common.point_definition list ref
    let init () = ref []
    let instrument_expr points ?override_loc  e =
      let rec outline () =
        let point_loc = choose_location_of_point ~override_loc e in
        if expression_should_not_be_instrumented ~point_loc
        then e
        else
          (let point_index = get_index_of_point_at_location ~point_loc in
           {
             Parsetree.pexp_desc =
               (Parsetree.Pexp_sequence
                  ({
                     Parsetree.pexp_desc =
                       (Parsetree.Pexp_apply
                          ({
                             Parsetree.pexp_desc =
                               (Parsetree.Pexp_ident
                                  {
                                    Asttypes.txt =
                                      (Longident.Lident
                                         "___bisect_visit___");
                                    Asttypes.loc =
                                      point_loc
                                  });
                             Parsetree.pexp_loc = point_loc;
                             Parsetree.pexp_loc_stack = [];
                             Parsetree.pexp_attributes = []
                           },
                            [(Asttypes.Nolabel,
                               point_index)]));
                     Parsetree.pexp_loc = point_loc;
                     Parsetree.pexp_loc_stack = [];
                     Parsetree.pexp_attributes = []
                   }, e));
             Parsetree.pexp_loc = point_loc;
             Parsetree.pexp_loc_stack = [];
             Parsetree.pexp_attributes = []
           })
      and choose_location_of_point ~override_loc  e =
        match override_loc with
        | Some override_loc -> override_loc
        | _ -> let open Parsetree in e.pexp_loc
      and expression_should_not_be_instrumented ~point_loc:loc  =
        if let open Location in loc.loc_ghost
        then true
        else
          (let (file, line) =
             let start = let open Location in loc.loc_start in
             let open Lexing in ((start.pos_fname), (start.pos_lnum)) in
           (Comments.get file) |> (Comments.line_is_ignored line))
      and get_index_of_point_at_location ~point_loc:loc  =
        let point_offset =
          let open Location in let open Lexing in (loc.loc_start).pos_cnum in
        let point =
          try
            List.find
              (fun point -> (let open Common in point.offset) = point_offset)
              (!points)
          with
          | Not_found ->
              let new_index = List.length (!points) in
              let new_point =
                let open Common in
                  { offset = point_offset; identifier = new_index } in
              (points := (new_point :: (!points)); new_point) in
        let open Common in conv_int point.identifier in
      outline ()
    let instrument_case points case =
      let module Helper_types =
        struct
          type location_trace = Location.t list
          type rotated_case = (location_trace * Parsetree.pattern)
        end in
        let open Helper_types in
          let rec outline () =
            if is_assert_false_or_refutation case
            then case
            else
              (let entire_pattern = let open Parsetree in case.pc_lhs in
               let loc = let open Parsetree in entire_pattern.ppat_loc in
               let (non_exception_pattern,
                    reassemble_exception_pattern_if_present)
                 = go_into_exception_pattern_if_present ~entire_pattern in
               let rotated_cases : rotated_case list =
                 rotate_or_patterns_to_top loc ~non_exception_pattern in
               match rotated_cases with
               | [] -> empty_case_list case
               | (location_trace, _)::[] ->
                   no_or_patterns case location_trace
               | _::_::_ ->
                   let new_case_pattern_with_alias =
                     (add_bisect_matched_value_alias loc
                        ~non_exception_pattern)
                       |> reassemble_exception_pattern_if_present in
                   let new_case_expr_with_nested_match =
                     generate_nested_match loc rotated_cases in
                   Exp.case new_case_pattern_with_alias
                     ?guard:(instrument_when_clause case)
                     new_case_expr_with_nested_match)
          and is_assert_false_or_refutation case =
            match case.pc_rhs with
            | {
                Parsetree.pexp_desc = Parsetree.Pexp_assert
                  {
                    Parsetree.pexp_desc =
                      Parsetree.Pexp_construct
                      ({
                         Asttypes.txt =
                           Longident.Lident
                           "false";
                         Asttypes.loc = _ },
                       None);
                    Parsetree.pexp_loc = _;
                    Parsetree.pexp_loc_stack = _;
                    Parsetree.pexp_attributes = _ };
                Parsetree.pexp_loc = _;
                Parsetree.pexp_loc_stack = _;
                Parsetree.pexp_attributes = _ } -> true
            | { pexp_desc = Pexp_unreachable;_} -> true
            | _ -> false
          and go_into_exception_pattern_if_present ~entire_pattern  =
            (match entire_pattern with
             | {
                 Parsetree.ppat_desc =
                   Parsetree.Ppat_exception nested_pattern;
                 Parsetree.ppat_loc = _;
                 Parsetree.ppat_loc_stack = _;
                 Parsetree.ppat_attributes = _ } ->
                 (nested_pattern,
                   ((fun p ->
                       let open Parsetree in
                         { entire_pattern with ppat_desc = (Ppat_exception p)
                         })))
             | _ -> (entire_pattern, ((fun p -> p))) : (Parsetree.pattern *
                                                         (Parsetree.pattern
                                                            ->
                                                            Parsetree.pattern)))
          and empty_case_list case =
            let open Parsetree in
              {
                case with
                pc_rhs = (instrument_expr points case.pc_rhs);
                pc_guard = (instrument_when_clause case)
              }
          and no_or_patterns case location_trace =
            let open Parsetree in
              {
                case with
                pc_rhs =
                  (instrumentation_for_location_trace case.pc_rhs
                     location_trace);
                pc_guard = (instrument_when_clause case)
              }
          and instrument_when_clause case =
            match let open Parsetree in case.pc_guard with
            | None -> None
            | Some guard -> Some (instrument_expr points guard)
          and instrumentation_for_location_trace e location_trace =
            (location_trace |>
               (List.sort_uniq
                  (fun l ->
                     fun l' ->
                       (l.Location.loc_start).Lexing.pos_cnum -
                         (l'.Location.loc_start).Lexing.pos_cnum)))
              |>
              (List.fold_left
                 (fun e -> fun l -> instrument_expr points ~override_loc:l e)
                 e)
          and add_bisect_matched_value_alias loc ~non_exception_pattern  =
            {
              Parsetree.ppat_desc =
                (Parsetree.Ppat_alias
                   (non_exception_pattern,
                     {
                       Asttypes.txt =
                         "___bisect_matched_value___";
                       Asttypes.loc = loc
                     }));
              Parsetree.ppat_loc = loc;
              Parsetree.ppat_loc_stack = [];
              Parsetree.ppat_attributes = []
            }
          and generate_nested_match loc rotated_cases =
            (((rotated_cases |>
                 (List.map
                    (fun (location_trace, rotated_pattern) ->
                       Exp.case rotated_pattern
                         (instrumentation_for_location_trace
                            {
                              Parsetree.pexp_desc =
                                (Parsetree.Pexp_construct
                                   ({
                                      Asttypes.txt
                                        =
                                        (Longident.Lident
                                           "()");
                                      Asttypes.loc
                                        = loc
                                    }, None));
                              Parsetree.pexp_loc = loc;
                              Parsetree.pexp_loc_stack = [];
                              Parsetree.pexp_attributes = []
                            } location_trace))))
                |>
                (fun nested_match_cases ->
                   ((nested_match_cases @
                       [Exp.case
                          {
                            Parsetree.ppat_desc =
                              Parsetree.Ppat_any;
                            Parsetree.ppat_loc = loc;
                            Parsetree.ppat_loc_stack = [];
                            Parsetree.ppat_attributes = []
                          }
                          {
                            Parsetree.pexp_desc =
                              (Parsetree.Pexp_construct
                                 ({
                                    Asttypes.txt =
                                      (Longident.Lident
                                         "()");
                                    Asttypes.loc =
                                      loc
                                  }, None));
                            Parsetree.pexp_loc = loc;
                            Parsetree.pexp_loc_stack = [];
                            Parsetree.pexp_attributes = []
                          }])
                      |>
                      (Exp.match_ ~loc
                         {
                           Parsetree.pexp_desc =
                             (Parsetree.Pexp_ident
                                {
                                  Asttypes.txt =
                                    (Longident.Lident
                                       "___bisect_matched_value___");
                                  Asttypes.loc =
                                    loc
                                });
                           Parsetree.pexp_loc = loc;
                           Parsetree.pexp_loc_stack = [];
                           Parsetree.pexp_attributes = []
                         }))
                     |>
                     (fun nested_match ->
                        (Exp.attr nested_match
                           {attr_name = Location.mkloc "ocaml.warning" loc;
                             attr_payload =
                                (Parsetree.PStr
                                   [{
                                      Parsetree.pstr_desc =
                                        (Parsetree.Pstr_eval
                                           ({
                                              Parsetree.pexp_desc =
                                                (Parsetree.Pexp_constant
                                                   (Parsetree.Pconst_string
                                                      ("-4-8-9-11-26-27-28",
                                                        None)));
                                              Parsetree.pexp_loc =
                                                loc;
                                              Parsetree.pexp_loc_stack
                                                = [];
                                              Parsetree.pexp_attributes
                                                = []
                                            }, []));
                                      Parsetree.pstr_loc = loc
                                    }]);
                                    attr_loc = loc})
                          |>
                          (fun nested_match_with_attribute ->
                             {
                               Parsetree.pexp_desc =
                                 (Parsetree.Pexp_sequence
                                    (nested_match_with_attribute,
                                      (case.pc_rhs)));
                               Parsetree.pexp_loc = loc;
                               Parsetree.pexp_loc_stack = [];
                               Parsetree.pexp_attributes = []
                             }))))
            [@metaloc loc])
          and rotate_or_patterns_to_top loc ~non_exception_pattern  =
            (let rec recurse ~enclosing_loc  p =
               (let loc = let open Parsetree in p.ppat_loc in
                let attrs = let open Parsetree in p.ppat_attributes in
                match p.ppat_desc with
                | Ppat_any|Ppat_var _|Ppat_constant _|Ppat_interval _
                  |Ppat_construct (_, None)|Ppat_variant (_, None)|Ppat_type
                  _|Ppat_unpack _|Ppat_extension _ -> [([enclosing_loc], p)]
                | Ppat_alias (p', x) ->
                    (recurse ~enclosing_loc p') |>
                      (List.map
                         (fun (location_trace, p'') ->
                            (location_trace, (Pat.alias ~loc ~attrs p'' x))))
                | Ppat_construct (c, Some p') ->
                    (recurse ~enclosing_loc p') |>
                      (List.map
                         (fun (location_trace, p'') ->
                            (location_trace,
                              (Pat.construct ~loc ~attrs c (Some p'')))))
                | Ppat_variant (c, Some p') ->
                    (recurse ~enclosing_loc p') |>
                      (List.map
                         (fun (location_trace, p'') ->
                            (location_trace,
                              (Pat.variant ~loc ~attrs c (Some p'')))))
                | Ppat_constraint (p', t) ->
                    (recurse ~enclosing_loc p') |>
                      (List.map
                         (fun (location_trace, p'') ->
                            (location_trace,
                              (Pat.constraint_ ~loc ~attrs p'' t))))
                | Ppat_lazy p' ->
                    (recurse ~enclosing_loc p') |>
                      (List.map
                         (fun (location_trace, p'') ->
                            (location_trace, (Pat.lazy_ ~loc ~attrs p''))))
                | Ppat_open (c, p') ->
                    (recurse ~enclosing_loc p') |>
                      (List.map
                         (fun (location_trace, p'') ->
                            (location_trace, (Pat.open_ ~loc ~attrs c p''))))
                | Ppat_tuple ps ->
                    ((ps |> (List.map (recurse ~enclosing_loc))) |>
                       all_combinations)
                      |>
                      (List.map
                         (fun (location_trace, ps') ->
                            (location_trace, (Pat.tuple ~loc ~attrs ps'))))
                | Ppat_record (entries, closed) ->
                    let (labels, ps) = List.split entries in
                    ((ps |> (List.map (recurse ~enclosing_loc))) |>
                       all_combinations)
                      |>
                      (List.map
                         (fun (location_trace, ps') ->
                            (location_trace,
                              (Pat.record ~loc ~attrs
                                 (List.combine labels ps') closed))))
                | Ppat_array ps ->
                    ((ps |> (List.map (recurse ~enclosing_loc))) |>
                       all_combinations)
                      |>
                      (List.map
                         (fun (location_trace, ps') ->
                            (location_trace, (Pat.array ~loc ~attrs ps'))))
                | Ppat_or (p_1, p_2) ->
                    let ps_1 = recurse ~enclosing_loc:(p_1.ppat_loc) p_1 in
                    let ps_2 = recurse ~enclosing_loc:(p_2.ppat_loc) p_2 in
                    ps_1 @ ps_2
                | Ppat_exception _ -> [] : rotated_case list)
             and all_combinations
               : rotated_case list list ->
                   (location_trace * Parsetree.pattern list) list
               =
               function
               | [] -> []
               | cases::more ->
                   let multiply product cases =
                     (product |>
                        (List.map
                           (fun (location_trace_1, ps) ->
                              cases |>
                                (List.map
                                   (fun (location_trace_2, p) ->
                                      ((location_trace_1 @ location_trace_2),
                                        (ps @ [p])))))))
                       |> List.flatten in
                   let initial =
                     cases |>
                       (List.map
                          (fun (location_trace, p) -> (location_trace, [p]))) in
                   List.fold_left multiply initial more in
             recurse ~enclosing_loc:loc non_exception_pattern : rotated_case
                                                                  list) in
          outline ()
    let instrument_class_field_kind points =
      function
      | Parsetree.Cfk_virtual _ as cf -> cf
      | Parsetree.Cfk_concrete (o, e) ->
          Cf.concrete o (instrument_expr points e)
    let runtime_initialization points file =
      let loc = Location.in_file file in
      let mangled_module_name =
        let buffer = Buffer.create ((String.length file) * 2) in
        file |>
          (String.iter
             (function
              | 'A'..'Z'|'a'..'z'|'0'..'9'|'_' as c ->
                  Buffer.add_char buffer c
              | _ -> Buffer.add_string buffer "___"));
        "Bisect_visit___" ^ (Buffer.contents buffer) in
      let point_count = conv_int ~loc (List.length (!points)) in
      let points_data = conv_str ~loc (Common.write_points (!points)) in
      let file = conv_str ~loc file in
      let generated_module =
        let bisect_visit_function =
          {
            Parsetree.pstr_desc =
              (Parsetree.Pstr_value
                 (Asttypes.Nonrecursive,
                   [{
                      Parsetree.pvb_pat =
                        {
                          Parsetree.ppat_desc =
                            (Parsetree.Ppat_var
                               {
                                 Asttypes.txt =
                                   "___bisect_visit___";
                                 Asttypes.loc =
                                   loc
                               });
                          Parsetree.ppat_loc = loc;
                          Parsetree.ppat_loc_stack = [];
                          Parsetree.ppat_attributes = []
                        };
                      Parsetree.pvb_expr =
                        {
                          Parsetree.pexp_desc =
                            (Parsetree.Pexp_let
                               (Asttypes.Nonrecursive,
                                 [{
                                    Parsetree.pvb_pat =
                                      {
                                        Parsetree.ppat_desc =
                                          (Parsetree.Ppat_var
                                             {
                                               Asttypes.txt
                                                 = "point_definitions";
                                               Asttypes.loc
                                                 = loc
                                             });
                                        Parsetree.ppat_loc = loc;
                                        Parsetree.ppat_loc_stack = [];
                                        Parsetree.ppat_attributes =
                                          []
                                      };
                                    Parsetree.pvb_expr = points_data;
                                    Parsetree.pvb_attributes = [];
                                    Parsetree.pvb_loc = loc
                                  }],
                                 {
                                   Parsetree.pexp_desc =
                                     (Parsetree.Pexp_let
                                        (Asttypes.Nonrecursive,
                                          [{
                                             Parsetree.pvb_pat =
                                               {
                                                 Parsetree.ppat_desc
                                                   =
                                                   (Parsetree.Ppat_variant
                                                      ("Staged",
                                                        (Some
                                                           {
                                                             Parsetree.ppat_desc
                                                               =
                                                               (Parsetree.Ppat_var
                                                                  {
                                                                    Asttypes.txt
                                                                    = "cb";
                                                                    Asttypes.loc
                                                                    = loc
                                                                  });
                                                             Parsetree.ppat_loc
                                                               = loc;
                                                             Parsetree.ppat_loc_stack
                                                               = [];
                                                             Parsetree.ppat_attributes
                                                               = []
                                                           })));
                                                 Parsetree.ppat_loc =
                                                   loc;
                                                 Parsetree.ppat_loc_stack
                                                   = [];
                                                 Parsetree.ppat_attributes
                                                   = []
                                               };
                                             Parsetree.pvb_expr =
                                               {
                                                 Parsetree.pexp_desc
                                                   =
                                                   (Parsetree.Pexp_apply
                                                      ({
                                                         Parsetree.pexp_desc
                                                           =
                                                           (Parsetree.Pexp_ident
                                                              {
                                                                Asttypes.txt
                                                                  =
                                                                  (Longident.Ldot
                                                                    ((Longident.Ldot
                                                                    ((Longident.Lident
                                                                    "Bisect"),
                                                                    "Runtime")),
                                                                    "register_file"));
                                                                Asttypes.loc
                                                                  = loc
                                                              });
                                                         Parsetree.pexp_loc
                                                           = loc;
                                                         Parsetree.pexp_loc_stack
                                                           = [];
                                                         Parsetree.pexp_attributes
                                                           = []
                                                       },
                                                        [(Asttypes.Nolabel,
                                                           file);
                                                        ((Asttypes.Labelled
                                                            "point_count"),
                                                          point_count);
                                                        ((Asttypes.Labelled
                                                            "point_definitions"),
                                                          {
                                                            Parsetree.pexp_desc
                                                              =
                                                              (Parsetree.Pexp_ident
                                                                 {
                                                                   Asttypes.txt
                                                                    =
                                                                    (Longident.Lident
                                                                    "point_definitions");
                                                                   Asttypes.loc
                                                                    = loc
                                                                 });
                                                            Parsetree.pexp_loc
                                                              = loc;
                                                            Parsetree.pexp_loc_stack
                                                              = [];
                                                            Parsetree.pexp_attributes
                                                              = []
                                                          })]));
                                                 Parsetree.pexp_loc =
                                                   loc;
                                                 Parsetree.pexp_loc_stack
                                                   = [];
                                                 Parsetree.pexp_attributes
                                                   = []
                                               };
                                             Parsetree.pvb_attributes
                                               = [];
                                             Parsetree.pvb_loc = loc
                                           }],
                                          {
                                            Parsetree.pexp_desc =
                                              (Parsetree.Pexp_ident
                                                 {
                                                   Asttypes.txt
                                                     =
                                                     (Longident.Lident
                                                        "cb");
                                                   Asttypes.loc
                                                     = loc
                                                 });
                                            Parsetree.pexp_loc = loc;
                                            Parsetree.pexp_loc_stack
                                              = [];
                                            Parsetree.pexp_attributes
                                              = []
                                          }));
                                   Parsetree.pexp_loc = loc;
                                   Parsetree.pexp_loc_stack = [];
                                   Parsetree.pexp_attributes = []
                                 }));
                          Parsetree.pexp_loc = loc;
                          Parsetree.pexp_loc_stack = [];
                          Parsetree.pexp_attributes = []
                        };
                      Parsetree.pvb_attributes = [];
                      Parsetree.pvb_loc = loc
                    }]));
            Parsetree.pstr_loc = loc
          } in
        let open Ast_helper in
          (Str.module_ ~loc) @@
            (Mb.mk ~loc (Location.mkloc mangled_module_name loc)
               (Mod.structure ~loc [bisect_visit_function])) in
      let module_open =
        let open Ast_helper in
          (Str.open_ ~loc) @@
            (Opn.mk ~loc (Mod.ident ~loc (conv_lid mangled_module_name))) in
      [generated_module; module_open]
  end
let super = Ast_mapper.default_mapper


module Make() = struct
let points = Generated_code.init ()
let instrument_expr = Generated_code.instrument_expr @@ points
let instrument_class_field_kind = Generated_code.instrument_class_field_kind @@ points
let instrument_case = Generated_code.instrument_case @@ points

let class_expr mapper ce =
  let loc = ce.pcl_loc in
  let ce = super.class_expr mapper ce in
  match ce.pcl_desc with
  | Pcl_apply (ce, args) ->
      let args =
        List.map (fun (label, e) -> (label, (instrument_expr e))) args in
      Ast_helper.Cl.apply ~loc ~attrs:(ce.pcl_attributes) ce args
  | _ -> ce
let class_field mapper cf =
  let loc = cf.pcf_loc in
  let attrs = cf.pcf_attributes in
  let cf = super.class_field mapper cf in
  match cf.pcf_desc with
  | Pcf_val (name, mutable_, cf) ->
      Cf.val_ ~loc ~attrs name mutable_ (instrument_class_field_kind cf)
  | Pcf_method (name, private_, cf) ->
      Cf.method_ ~loc ~attrs name private_ (instrument_class_field_kind cf)
  | Pcf_initializer e -> Cf.initializer_ ~loc ~attrs (instrument_expr e)
  | _ -> cf
let expr mapper e =
  let loc = e.pexp_loc in
  let attrs = e.pexp_attributes in
  let e' = super.expr mapper e in
  match e'.pexp_desc with
  | Pexp_let (rec_flag, bindings, e) ->
      let bindings =
        List.map
          (fun binding ->
             let open Parsetree in
               { binding with pvb_expr = (instrument_expr binding.pvb_expr) })
          bindings in
      Exp.let_ ~loc ~attrs rec_flag bindings (instrument_expr e)
  | Pexp_poly (e, type_) -> Exp.poly ~loc ~attrs (instrument_expr e) type_
  | Pexp_fun (label, default_value, p, e) ->
      let default_value =
        match default_value with
        | None -> None
        | Some default_value -> Some (instrument_expr default_value) in
      Exp.fun_ ~loc ~attrs label default_value p (instrument_expr e)
  | Pexp_apply (e_function, (label_1, e1)::(label_2, e2)::[]) ->
      (match e_function with
       | {
           Parsetree.pexp_desc = Parsetree.Pexp_ident
             {
               Asttypes.txt =
                 Longident.Lident "&&";
               Asttypes.loc = _ };
           Parsetree.pexp_loc = _;
           Parsetree.pexp_loc_stack = _;
           Parsetree.pexp_attributes = _ }
         |{
            Parsetree.pexp_desc = Parsetree.Pexp_ident
              {
                Asttypes.txt =
                  Longident.Lident "&";
                Asttypes.loc = _ };
            Parsetree.pexp_loc = _;
            Parsetree.pexp_loc_stack = _;
            Parsetree.pexp_attributes = _ }
         |{
            Parsetree.pexp_desc = Parsetree.Pexp_ident
              {
                Asttypes.txt =
                  Longident.Lident "||";
                Asttypes.loc = _ };
            Parsetree.pexp_loc = _;
            Parsetree.pexp_loc_stack = _;
            Parsetree.pexp_attributes = _ }
         |{
            Parsetree.pexp_desc = Parsetree.Pexp_ident
              {
                Asttypes.txt =
                  Longident.Lident "or";
                Asttypes.loc = _ };
            Parsetree.pexp_loc = _;
            Parsetree.pexp_loc_stack = _;
            Parsetree.pexp_attributes = _ }
           ->
           Exp.apply ~loc ~attrs e_function
             [(label_1, (instrument_expr e1));
             (label_2, (instrument_expr e2))]
       | {
           Parsetree.pexp_desc = Parsetree.Pexp_ident
             {
               Asttypes.txt =
                 Longident.Lident "|>";
               Asttypes.loc = _ };
           Parsetree.pexp_loc = _;
           Parsetree.pexp_loc_stack = _;
           Parsetree.pexp_attributes = _ } ->
           Exp.apply ~loc ~attrs e_function
             [(label_1, e1); (label_2, (instrument_expr e2))]
       | _ -> e')
  | Pexp_match (e, cases) ->
      (List.map instrument_case cases) |> (Exp.match_ ~loc ~attrs e)
  | Pexp_function cases ->
      (List.map instrument_case cases) |> (Exp.function_ ~loc ~attrs)
  | Pexp_try (e, cases) ->
      (List.map instrument_case cases) |> (Exp.try_ ~loc ~attrs e)
  | Pexp_ifthenelse (condition, then_, else_) ->
      Exp.ifthenelse ~loc ~attrs condition (instrument_expr then_)
        (match else_ with | Some e -> Some (instrument_expr e) | None -> None)
  | Pexp_sequence (e1, e2) ->
      Exp.sequence ~loc ~attrs e1 (instrument_expr e2)
  | Pexp_while (condition, body) ->
      Exp.while_ ~loc ~attrs condition (instrument_expr body)
  | Pexp_for (variable, initial, bound, direction, body) ->
      Exp.for_ ~loc ~attrs variable initial bound direction
        (instrument_expr body)
  | _ -> e'
let structure_item mapper si =
  let loc = si.pstr_loc in
  match si.pstr_desc with
  | Pstr_value (rec_flag, bindings) ->
      let bindings =
        bindings |>
          (List.map
             (fun binding ->
                let maybe_name =
                  let open Parsetree in
                    match (binding.pvb_pat).ppat_desc with
                    | Ppat_var ident|Ppat_constraint
                      ({ ppat_desc = Ppat_var ident;_}, _) -> Some ident
                    | _ -> None in
                let do_not_instrument =
                  match maybe_name with
                  | Some name ->
                      Exclusions.contains_value
                        (let open Location in
                           let open Lexing in
                             ((name.loc).loc_start).pos_fname) name.txt
                  | None -> false in
                if do_not_instrument
                then binding
                else
                  {
                    binding with
                    pvb_expr = (instrument_expr (super.expr mapper binding.pvb_expr))
                  })) in
      Str.value ~loc rec_flag bindings
  | Pstr_eval (e, a) ->
      Str.eval ~loc ~attrs:a (instrument_expr (super.expr mapper e))
  | _ -> super.structure_item mapper si
let extension _ e = e
let attribute _ a = a

let saw_top_level_structure_or_signature = ref false

let signature mapper ast =
  if not !saw_top_level_structure_or_signature
  then saw_top_level_structure_or_signature := true;
  super.signature mapper ast
let structure mapper ast =
  if !saw_top_level_structure_or_signature
  then super.structure mapper ast
  else
    (saw_top_level_structure_or_signature := true;
     (let always_ignore_paths = ["//toplevel//"; "(stdin)"] in
      let always_ignore_basenames = [".ocamlinit"; "topfind"] in
      let always_ignore path =
        (List.mem path always_ignore_paths) ||
          (List.mem (Filename.basename path) always_ignore_basenames) in
      if always_ignore (!Location.input_name)
      then ast
      else
        if Exclusions.contains_file (!Location.input_name)
        then ast
        else
          (let instrumented_ast = super.structure mapper ast in
           let runtime_initialization =
             Generated_code.runtime_initialization (points)
               (!Location.input_name) in
           runtime_initialization @ instrumented_ast)))
let mapper =
  {
    super with
    class_expr;
    class_field;
    expr;
    structure_item;
    extension;
    attribute;
    signature;
    structure
  }
end
