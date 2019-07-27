module Bisect_visit___typing___typetexp___ml =
  struct
    let ___bisect_visit___ =
      let point_definitions =
        "\132\149\166\190\000\000\000\001\000\000\000\000\000\000\000\000\000\000\000\000\128" in
      let `Staged cb =
        Bisect.Runtime.register_file "typing/typetexp.ml" ~point_count:0
          ~point_definitions in
      cb
  end
open Bisect_visit___typing___typetexp___ml
open Asttypes
open Misc
open Parsetree
open Typedtree
open Types
open Ctype
exception Already_bound
type error =
  | Unbound_type_variable of string
  | Unbound_type_constructor of Longident.t
  | Unbound_type_constructor_2 of Path.t
  | Type_arity_mismatch of Longident.t * int * int
  | Bound_type_variable of string
  | Recursive_type
  | Unbound_row_variable of Longident.t
  | Type_mismatch of Ctype.Unification_trace.t
  | Alias_type_mismatch of Ctype.Unification_trace.t
  | Present_has_conjunction of string
  | Present_has_no_type of string
  | Constructor_mismatch of type_expr * type_expr
  | Not_a_variant of type_expr
  | Variant_tags of string * string
  | Invalid_variable_name of string
  | Cannot_quantify of string * type_expr
  | Multiple_constraints_on_type of Longident.t
  | Method_mismatch of string * type_expr * type_expr
  | Unbound_value of Longident.t
  | Unbound_constructor of Longident.t
  | Unbound_label of Longident.t
  | Unbound_module of Longident.t
  | Unbound_class of Longident.t
  | Unbound_modtype of Longident.t
  | Unbound_cltype of Longident.t
  | Ill_typed_functor_application of Longident.t * Longident.t *
  Includemod.error list option
  | Illegal_reference_to_recursive_module
  | Wrong_use_of_module of Longident.t *
  [ `Structure_used_as_functor  | `Abstract_used_as_functor
  | `Functor_used_as_structure  | `Abstract_used_as_structure
  | `Generative_used_as_applicative ]
  | Cannot_scrape_alias of Longident.t * Path.t
  | Opened_object of Path.t option
  | Not_an_object of type_expr
  | Unbound_value_missing_rec of Longident.t * Location.t
exception Error of Location.t * Env.t * error
exception Error_forward of Location.error
module TyVarMap = Misc.Stdlib.String.Map[@@ocaml.doc
                                          " Map indexed by type variable names. "]
type variable_context = (int * type_expr TyVarMap.t)
let typemod_update_location =
  ___bisect_visit___ 1; ref (fun _ -> ___bisect_visit___ 0; assert false)
let rec narrow_unbound_lid_error : 'a . _ -> _ -> _ -> _ -> 'a =
  ___bisect_visit___ 25;
  (fun env ->
     fun loc ->
       fun lid ->
         fun make_error ->
           ___bisect_visit___ 24;
           (let check_module mlid =
              ___bisect_visit___ 22;
              (try ignore (Env.lookup_module ~load:true mlid env)
               with
               | Not_found ->
                   (___bisect_visit___ 15;
                    narrow_unbound_lid_error env loc mlid
                      (fun lid -> ___bisect_visit___ 21; Unbound_module lid))
               | Env.Recmodule ->
                   (___bisect_visit___ 16;
                    raise
                      (Error
                         (loc, env, Illegal_reference_to_recursive_module)))) in
            ___bisect_visit___ 23;
            (let error e = ___bisect_visit___ 19; raise (Error (loc, env, e)) in
             ___bisect_visit___ 20;
             (match lid with
              | Longident.Lident _ -> (___bisect_visit___ 12; ())
              | Longident.Ldot (mlid, _) ->
                  (___bisect_visit___ 13;
                   check_module mlid;
                   ___bisect_visit___ 4;
                   (let md =
                      ___bisect_visit___ 2;
                      Env.find_module (Env.lookup_module ~load:true mlid env)
                        env in
                    ___bisect_visit___ 3;
                    (match Env.scrape_alias env md.md_type with
                     | Mty_functor _ ->
                         (___bisect_visit___ 0;
                          error
                            (Wrong_use_of_module
                               (mlid, `Functor_used_as_structure)))
                     | Mty_ident _ ->
                         (___bisect_visit___ 1;
                          error
                            (Wrong_use_of_module
                               (mlid, `Abstract_used_as_structure)))
                     | Mty_alias p ->
                         (___bisect_visit___ 2;
                          error (Cannot_scrape_alias (mlid, p)))
                     | Mty_signature _ -> (___bisect_visit___ 3; ()))))
              | Longident.Lapply (flid, mlid) ->
                  (___bisect_visit___ 14;
                   check_module flid;
                   ___bisect_visit___ 17;
                   (let fmd =
                      ___bisect_visit___ 15;
                      Env.find_module (Env.lookup_module ~load:true flid env)
                        env in
                    ___bisect_visit___ 16;
                    (let mty_param =
                       ___bisect_visit___ 13;
                       (match Env.scrape_alias env fmd.md_type with
                        | Mty_signature _ ->
                            (___bisect_visit___ 7;
                             error
                               (Wrong_use_of_module
                                  (flid, `Structure_used_as_functor)))
                        | Mty_ident _ ->
                            (___bisect_visit___ 8;
                             error
                               (Wrong_use_of_module
                                  (flid, `Abstract_used_as_functor)))
                        | Mty_alias p ->
                            (___bisect_visit___ 9;
                             error (Cannot_scrape_alias (flid, p)))
                        | Mty_functor (_, None, _) ->
                            (___bisect_visit___ 10;
                             error
                               (Wrong_use_of_module
                                  (flid, `Generative_used_as_applicative)))
                        | Mty_functor (_, Some mty_param, _) ->
                            (___bisect_visit___ 11; mty_param)) in
                     ___bisect_visit___ 14;
                     check_module mlid;
                     ___bisect_visit___ 12;
                     (let mpath =
                        ___bisect_visit___ 10;
                        Env.lookup_module ~load:true mlid env in
                      ___bisect_visit___ 11;
                      (let mmd =
                         ___bisect_visit___ 8; Env.find_module mpath env in
                       ___bisect_visit___ 9;
                       (match Env.scrape_alias env mmd.md_type with
                        | Mty_alias p ->
                            (___bisect_visit___ 5;
                             error (Cannot_scrape_alias (mlid, p)))
                        | mty_arg ->
                            (___bisect_visit___ 6;
                             (let details =
                                ___bisect_visit___ 6;
                                (try
                                   Includemod.check_modtype_inclusion ~loc
                                     env mty_arg mpath mty_param;
                                   ___bisect_visit___ 5;
                                   None
                                 with
                                 | Includemod.Error e ->
                                     (___bisect_visit___ 4; Some e)) in
                              ___bisect_visit___ 7;
                              error
                                (Ill_typed_functor_application
                                   (flid, mlid, details)))))))))));
             ___bisect_visit___ 18;
             error (make_error lid))))
let find_component (lookup : ?loc:_ -> ?mark:_ -> _) make_error env loc lid =
  ___bisect_visit___ 26;
  (try
     match lid with
     | Longident.Ldot (Longident.Lident "*predef*", s) ->
         (___bisect_visit___ 17;
          lookup ~loc (Longident.Lident s) Env.initial_safe_string)
     | _ -> (___bisect_visit___ 18; lookup ~loc lid env)
   with
   | Not_found ->
       (___bisect_visit___ 19;
        narrow_unbound_lid_error env loc lid make_error)
   | Env.Recmodule ->
       (___bisect_visit___ 20;
        raise (Error (loc, env, Illegal_reference_to_recursive_module)))
   | err ->
       (___bisect_visit___ 21; raise ((!typemod_update_location) loc err)))
let find_type env loc lid =
  ___bisect_visit___ 33;
  (let path =
     ___bisect_visit___ 31;
     find_component Env.lookup_type
       (fun lid -> ___bisect_visit___ 30; Unbound_type_constructor lid) env
       loc lid in
   ___bisect_visit___ 32;
   (let decl = ___bisect_visit___ 28; Env.find_type path env in
    ___bisect_visit___ 29;
    Builtin_attributes.check_alerts loc decl.type_attributes (Path.name path);
    ___bisect_visit___ 27;
    (path, decl)))
let find_constructor =
  ___bisect_visit___ 35;
  find_component Env.lookup_constructor
    (fun lid -> ___bisect_visit___ 34; Unbound_constructor lid)
let find_all_constructors =
  ___bisect_visit___ 37;
  find_component Env.lookup_all_constructors
    (fun lid -> ___bisect_visit___ 36; Unbound_constructor lid)
let find_label =
  ___bisect_visit___ 39;
  find_component Env.lookup_label
    (fun lid -> ___bisect_visit___ 38; Unbound_label lid)
let find_all_labels =
  ___bisect_visit___ 41;
  find_component Env.lookup_all_labels
    (fun lid -> ___bisect_visit___ 40; Unbound_label lid)
let find_class env loc lid =
  ___bisect_visit___ 46;
  (let (path, decl) as r =
     ___bisect_visit___ 44;
     find_component Env.lookup_class
       (fun lid -> ___bisect_visit___ 43; Unbound_class lid) env loc lid in
   ___bisect_visit___ 45;
   Builtin_attributes.check_alerts loc decl.cty_attributes (Path.name path);
   ___bisect_visit___ 42;
   r)
let find_value env loc lid =
  ___bisect_visit___ 52;
  Env.check_value_name (Longident.last lid) loc;
  ___bisect_visit___ 51;
  (let (path, decl) as r =
     ___bisect_visit___ 49;
     find_component Env.lookup_value
       (fun lid -> ___bisect_visit___ 48; Unbound_value lid) env loc lid in
   ___bisect_visit___ 50;
   Builtin_attributes.check_alerts loc decl.val_attributes (Path.name path);
   ___bisect_visit___ 47;
   r)
let lookup_module ?(load= false)  env loc lid =
  ___bisect_visit___ 55;
  find_component
    (fun ?loc ->
       fun ?mark ->
         fun lid ->
           fun env ->
             ___bisect_visit___ 53;
             Env.lookup_module ~load ?loc ?mark lid env)
    (fun lid -> ___bisect_visit___ 54; Unbound_module lid) env loc lid
let find_module env loc lid =
  ___bisect_visit___ 60;
  (let path = ___bisect_visit___ 58; lookup_module ~load:true env loc lid in
   ___bisect_visit___ 59;
   (let decl = ___bisect_visit___ 56; Env.find_module path env in
    ___bisect_visit___ 57; (path, decl)))
let find_modtype env loc lid =
  ___bisect_visit___ 65;
  (let (path, decl) as r =
     ___bisect_visit___ 63;
     find_component Env.lookup_modtype
       (fun lid -> ___bisect_visit___ 62; Unbound_modtype lid) env loc lid in
   ___bisect_visit___ 64;
   Builtin_attributes.check_alerts loc decl.mtd_attributes (Path.name path);
   ___bisect_visit___ 61;
   r)
let find_class_type env loc lid =
  ___bisect_visit___ 70;
  (let (path, decl) as r =
     ___bisect_visit___ 68;
     find_component Env.lookup_cltype
       (fun lid -> ___bisect_visit___ 67; Unbound_cltype lid) env loc lid in
   ___bisect_visit___ 69;
   Builtin_attributes.check_alerts loc decl.clty_attributes (Path.name path);
   ___bisect_visit___ 66;
   r)
let unbound_constructor_error env lid =
  ___bisect_visit___ 72;
  narrow_unbound_lid_error env lid.loc lid.txt
    (fun lid -> ___bisect_visit___ 71; Unbound_constructor lid)
let unbound_label_error env lid =
  ___bisect_visit___ 74;
  narrow_unbound_lid_error env lid.loc lid.txt
    (fun lid -> ___bisect_visit___ 73; Unbound_label lid)
let transl_modtype_longident =
  ___bisect_visit___ 76; ref (fun _ -> ___bisect_visit___ 75; assert false)
let transl_modtype =
  ___bisect_visit___ 78; ref (fun _ -> ___bisect_visit___ 77; assert false)
let create_package_mty fake loc env (p, l) =
  ___bisect_visit___ 89;
  (let l =
     ___bisect_visit___ 87;
     List.sort
       (fun (s1, _t1) ->
          fun (s2, _t2) ->
            ___bisect_visit___ 86;
            if s1.txt = s2.txt
            then
              (___bisect_visit___ 84;
               raise
                 (Error (loc, env, (Multiple_constraints_on_type (s1.txt)))));
            ___bisect_visit___ 85;
            compare s1.txt s2.txt) l in
   ___bisect_visit___ 88;
   (l,
     (List.fold_left
        (fun mty ->
           fun (s, t) ->
             ___bisect_visit___ 83;
             (let d =
                ___bisect_visit___ 81;
                {
                  ptype_name = (mkloc (Longident.last s.txt) s.loc);
                  ptype_params = [];
                  ptype_cstrs = [];
                  ptype_kind = Ptype_abstract;
                  ptype_private = Asttypes.Public;
                  ptype_manifest =
                    (if fake
                     then (___bisect_visit___ 80; None)
                     else (___bisect_visit___ 79; Some t));
                  ptype_attributes = [];
                  ptype_loc = loc
                } in
              ___bisect_visit___ 82;
              Ast_helper.Mty.mk ~loc
                (Pmty_with (mty, [Pwith_type ({ txt = (s.txt); loc }, d)]))))
        (Ast_helper.Mty.mk ~loc (Pmty_ident p)) l)))
let type_variables =
  ___bisect_visit___ 90; ref (TyVarMap.empty : type_expr TyVarMap.t)
let univars = ___bisect_visit___ 91; ref ([] : (string * type_expr) list)
let pre_univars = ___bisect_visit___ 92; ref ([] : type_expr list)
let used_variables =
  ___bisect_visit___ 93;
  ref (TyVarMap.empty : (type_expr * Location.t) TyVarMap.t)
let reset_type_variables () =
  reset_global_level ();
  ___bisect_visit___ 95;
  Ctype.reset_reified_var_counter ();
  ___bisect_visit___ 94;
  type_variables := TyVarMap.empty
let narrow () = ((increase_global_level ()), (!type_variables))
let widen (gl, tv) =
  restore_global_level gl; ___bisect_visit___ 96; type_variables := tv
let strict_ident c =
  (___bisect_visit___ 101; c = '_') ||
    (___bisect_visit___ 98;
     (___bisect_visit___ 98;
      (___bisect_visit___ 98; c >= 'a') &&
        ((___bisect_visit___ 97; c <= 'z')))
       ||
       ((___bisect_visit___ 100;
         (___bisect_visit___ 100; c >= 'A') &&
           ((___bisect_visit___ 99; c <= 'Z')))))
let validate_name =
  ___bisect_visit___ 106;
  (function
   | None -> None
   | Some name as s ->
       if
         (___bisect_visit___ 103; name <> "") &&
           ((___bisect_visit___ 102; strict_ident (name.[0])))
       then (___bisect_visit___ 105; s)
       else (___bisect_visit___ 104; None))
let new_global_var ?name  () =
  ___bisect_visit___ 107; new_global_var ?name:(validate_name name) ()
let newvar ?name  () =
  ___bisect_visit___ 108; newvar ?name:(validate_name name) ()
let type_variable loc name =
  ___bisect_visit___ 109;
  (try TyVarMap.find name (!type_variables)
   with
   | Not_found ->
       (___bisect_visit___ 22;
        raise (Error (loc, Env.empty, (Unbound_type_variable ("'" ^ name))))))
let valid_tyvar_name name =
  (___bisect_visit___ 111; name <> "") &&
    (___bisect_visit___ 110; (name.[0]) <> '_')
let transl_type_param env styp =
  ___bisect_visit___ 124;
  (let loc = ___bisect_visit___ 122; styp.ptyp_loc in
   ___bisect_visit___ 123;
   (match styp.ptyp_desc with
    | Ptyp_any ->
        (___bisect_visit___ 24;
         (let ty = ___bisect_visit___ 112; new_global_var ~name:"_" () in
          ___bisect_visit___ 113;
          {
            ctyp_desc = Ttyp_any;
            ctyp_type = ty;
            ctyp_env = env;
            ctyp_loc = loc;
            ctyp_attributes = (styp.ptyp_attributes)
          }))
    | Ptyp_var name ->
        (___bisect_visit___ 25;
         (let ty =
            ___bisect_visit___ 120;
            (try
               if not (valid_tyvar_name name)
               then
                 (___bisect_visit___ 118;
                  raise
                    (Error
                       (loc, Env.empty, (Invalid_variable_name ("'" ^ name)))));
               ___bisect_visit___ 119;
               ignore (TyVarMap.find name (!type_variables));
               ___bisect_visit___ 117;
               raise Already_bound
             with
             | Not_found ->
                 (___bisect_visit___ 23;
                  (let v = ___bisect_visit___ 115; new_global_var ~name () in
                   ___bisect_visit___ 116;
                   type_variables := (TyVarMap.add name v (!type_variables));
                   ___bisect_visit___ 114;
                   v))) in
          ___bisect_visit___ 121;
          {
            ctyp_desc = (Ttyp_var name);
            ctyp_type = ty;
            ctyp_env = env;
            ctyp_loc = loc;
            ctyp_attributes = (styp.ptyp_attributes)
          }))
    | _ -> assert false))
let transl_type_param env styp =
  ___bisect_visit___ 126;
  Builtin_attributes.warning_scope styp.ptyp_attributes
    (fun () -> ___bisect_visit___ 125; transl_type_param env styp)
let new_pre_univar ?name  () =
  ___bisect_visit___ 130;
  (let v = ___bisect_visit___ 128; newvar ?name () in
   ___bisect_visit___ 129;
   pre_univars := (v :: (!pre_univars));
   ___bisect_visit___ 127;
   v)
type policy =
  | Fixed
  | Extensible
  | Univars
let rec transl_type env policy styp =
  ___bisect_visit___ 132;
  Builtin_attributes.warning_scope styp.ptyp_attributes
    (fun () -> ___bisect_visit___ 131; transl_type_aux env policy styp)
and transl_type_aux env policy styp =
  ___bisect_visit___ 400;
  (let loc = ___bisect_visit___ 398; styp.ptyp_loc in
   ___bisect_visit___ 399;
   (let ctyp ctyp_desc ctyp_type =
      ___bisect_visit___ 396;
      {
        ctyp_desc;
        ctyp_type;
        ctyp_env = env;
        ctyp_loc = loc;
        ctyp_attributes = (styp.ptyp_attributes)
      } in
    ___bisect_visit___ 397;
    (match styp.ptyp_desc with
     | Ptyp_any ->
         (___bisect_visit___ 86;
          (let ty =
             ___bisect_visit___ 137;
             if policy = Univars
             then (___bisect_visit___ 136; new_pre_univar ())
             else
               (___bisect_visit___ 135;
                if policy = Fixed
                then
                  (___bisect_visit___ 134;
                   raise
                     (Error
                        ((styp.ptyp_loc), env, (Unbound_type_variable "_"))))
                else (___bisect_visit___ 133; newvar ())) in
           ___bisect_visit___ 138; ctyp Ttyp_any ty))
     | Ptyp_var name ->
         (___bisect_visit___ 87;
          (let ty =
             ___bisect_visit___ 146;
             if not (valid_tyvar_name name)
             then
               (___bisect_visit___ 144;
                raise
                  (Error
                     ((styp.ptyp_loc), env,
                       (Invalid_variable_name ("'" ^ name)))));
             ___bisect_visit___ 145;
             (try instance (List.assoc name (!univars))
              with
              | Not_found ->
                  (___bisect_visit___ 27;
                   (try instance (fst (TyVarMap.find name (!used_variables)))
                    with
                    | Not_found ->
                        (___bisect_visit___ 26;
                         (let v =
                            ___bisect_visit___ 142;
                            if policy = Univars
                            then
                              (___bisect_visit___ 141;
                               new_pre_univar ~name ())
                            else (___bisect_visit___ 140; newvar ~name ()) in
                          ___bisect_visit___ 143;
                          used_variables :=
                            (TyVarMap.add name (v, (styp.ptyp_loc))
                               (!used_variables));
                          ___bisect_visit___ 139;
                          v))))) in
           ___bisect_visit___ 147; ctyp (Ttyp_var name) ty))
     | Ptyp_arrow (l, st1, st2) ->
         (___bisect_visit___ 88;
          (let cty1 = ___bisect_visit___ 158; transl_type env policy st1 in
           ___bisect_visit___ 159;
           (let cty2 = ___bisect_visit___ 156; transl_type env policy st2 in
            ___bisect_visit___ 157;
            (let ty1 = ___bisect_visit___ 154; cty1.ctyp_type in
             ___bisect_visit___ 155;
             (let ty1 =
                ___bisect_visit___ 152;
                if Btype.is_optional l
                then
                  (___bisect_visit___ 151;
                   newty (Tconstr (Predef.path_option, [ty1], (ref Mnil))))
                else (___bisect_visit___ 150; ty1) in
              ___bisect_visit___ 153;
              (let ty =
                 ___bisect_visit___ 148;
                 newty (Tarrow (l, ty1, (cty2.ctyp_type), Cok)) in
               ___bisect_visit___ 149; ctyp (Ttyp_arrow (l, cty1, cty2)) ty))))))
     | Ptyp_tuple stl ->
         (___bisect_visit___ 89;
          assert ((List.length stl) >= 2);
          ___bisect_visit___ 165;
          (let ctys =
             ___bisect_visit___ 163; List.map (transl_type env policy) stl in
           ___bisect_visit___ 164;
           (let ty =
              ___bisect_visit___ 161;
              newty
                (Ttuple
                   (List.map
                      (fun ctyp -> ___bisect_visit___ 160; ctyp.ctyp_type)
                      ctys)) in
            ___bisect_visit___ 162; ctyp (Ttyp_tuple ctys) ty)))
     | Ptyp_constr (lid, stl) ->
         (___bisect_visit___ 90;
          (let (path, decl) =
             ___bisect_visit___ 187; find_type env lid.loc lid.txt in
           ___bisect_visit___ 188;
           (let stl =
              ___bisect_visit___ 185;
              (match stl with
               | ({ ptyp_desc = Ptyp_any } as t)::[] when
                   ___bisect_visit___ 33; decl.type_arity > 1 ->
                   (___bisect_visit___ 32;
                    List.map (fun _ -> ___bisect_visit___ 184; t)
                      decl.type_params)
               | _ -> (___bisect_visit___ 34; stl)) in
            ___bisect_visit___ 186;
            if (List.length stl) <> decl.type_arity
            then
              (___bisect_visit___ 182;
               raise
                 (Error
                    ((styp.ptyp_loc), env,
                      (Type_arity_mismatch
                         ((lid.txt), (decl.type_arity), (List.length stl))))));
            ___bisect_visit___ 183;
            (let args =
               ___bisect_visit___ 180; List.map (transl_type env policy) stl in
             ___bisect_visit___ 181;
             (let params =
                ___bisect_visit___ 178; instance_list decl.type_params in
              ___bisect_visit___ 179;
              (let unify_param =
                 ___bisect_visit___ 176;
                 (match decl.type_manifest with
                  | None -> (___bisect_visit___ 30; unify_var)
                  | Some ty ->
                      (___bisect_visit___ 31;
                       if (repr ty).level = Btype.generic_level
                       then (___bisect_visit___ 175; unify_var)
                       else (___bisect_visit___ 174; unify))) in
               ___bisect_visit___ 177;
               List.iter2
                 (fun (sty, cty) ->
                    fun ty' ->
                      ___bisect_visit___ 172;
                      (try unify_param env ty' cty.ctyp_type
                       with
                       | Unify trace ->
                           (___bisect_visit___ 29;
                            (let trace =
                               ___bisect_visit___ 170;
                               Unification_trace.swap trace in
                             ___bisect_visit___ 171;
                             raise
                               (Error
                                  ((sty.ptyp_loc), env,
                                    (Type_mismatch trace)))))))
                 (List.combine stl args) params;
               ___bisect_visit___ 173;
               (let constr =
                  ___bisect_visit___ 168;
                  newconstr path
                    (List.map
                       (fun ctyp -> ___bisect_visit___ 167; ctyp.ctyp_type)
                       args) in
                ___bisect_visit___ 169;
                (try Ctype.enforce_constraints env constr
                 with
                 | Unify trace ->
                     (___bisect_visit___ 28;
                      raise
                        (Error ((styp.ptyp_loc), env, (Type_mismatch trace)))));
                ___bisect_visit___ 166;
                ctyp (Ttyp_constr (path, lid, args)) constr)))))))
     | Ptyp_object (fields, o) ->
         (___bisect_visit___ 91;
          (let (ty, fields) =
             ___bisect_visit___ 189; transl_fields env policy o fields in
           ___bisect_visit___ 190; ctyp (Ttyp_object (fields, o)) (newobj ty)))
     | Ptyp_class (lid, stl) ->
         (___bisect_visit___ 92;
          (let (path, decl, _is_variant) =
             ___bisect_visit___ 242;
             (try
                let path =
                  ___bisect_visit___ 240; Env.lookup_type lid.txt env in
                ___bisect_visit___ 241;
                (let decl = ___bisect_visit___ 238; Env.find_type path env in
                 ___bisect_visit___ 239;
                 (let rec check decl =
                    ___bisect_visit___ 236;
                    (match decl.type_manifest with
                     | None -> (___bisect_visit___ 50; raise Not_found)
                     | Some ty ->
                         (___bisect_visit___ 51;
                          (match (repr ty).desc with
                           | Tvariant row when
                               ___bisect_visit___ 47; Btype.static_row row ->
                               (___bisect_visit___ 46; ())
                           | Tconstr (path, _, _) ->
                               (___bisect_visit___ 48;
                                check (Env.find_type path env))
                           | _ -> (___bisect_visit___ 49; raise Not_found)))) in
                  ___bisect_visit___ 237;
                  check decl;
                  ___bisect_visit___ 235;
                  Location.deprecated styp.ptyp_loc
                    "old syntax for polymorphic variant type";
                  ___bisect_visit___ 234;
                  (path, decl, true)))
              with
              | Not_found ->
                  (___bisect_visit___ 52;
                   (try
                      let lid2 =
                        ___bisect_visit___ 232;
                        (match lid.txt with
                         | Longident.Lident s ->
                             (___bisect_visit___ 42;
                              Longident.Lident ("#" ^ s))
                         | Longident.Ldot (r, s) ->
                             (___bisect_visit___ 43;
                              Longident.Ldot (r, ("#" ^ s)))
                         | Longident.Lapply (_, _) ->
                             (___bisect_visit___ 44;
                              fatal_error "Typetexp.transl_type")) in
                      ___bisect_visit___ 233;
                      (let path =
                         ___bisect_visit___ 230; Env.lookup_type lid2 env in
                       ___bisect_visit___ 231;
                       (let decl =
                          ___bisect_visit___ 228; Env.find_type path env in
                        ___bisect_visit___ 229; (path, decl, false)))
                    with
                    | Not_found ->
                        (___bisect_visit___ 45;
                         ignore (find_class env lid.loc lid.txt);
                         ___bisect_visit___ 227;
                         assert false)))) in
           ___bisect_visit___ 243;
           if (List.length stl) <> decl.type_arity
           then
             (___bisect_visit___ 225;
              raise
                (Error
                   ((styp.ptyp_loc), env,
                     (Type_arity_mismatch
                        ((lid.txt), (decl.type_arity), (List.length stl))))));
           ___bisect_visit___ 226;
           (let args =
              ___bisect_visit___ 223; List.map (transl_type env policy) stl in
            ___bisect_visit___ 224;
            (let params =
               ___bisect_visit___ 221; instance_list decl.type_params in
             ___bisect_visit___ 222;
             List.iter2
               (fun (sty, cty) ->
                  fun ty' ->
                    ___bisect_visit___ 219;
                    (try unify_var env ty' cty.ctyp_type
                     with
                     | Unify trace ->
                         (___bisect_visit___ 41;
                          (let trace =
                             ___bisect_visit___ 217;
                             Unification_trace.swap trace in
                           ___bisect_visit___ 218;
                           raise
                             (Error
                                ((sty.ptyp_loc), env, (Type_mismatch trace)))))))
               (List.combine stl args) params;
             ___bisect_visit___ 220;
             (let ty_args =
                ___bisect_visit___ 215;
                List.map (fun ctyp -> ___bisect_visit___ 214; ctyp.ctyp_type)
                  args in
              ___bisect_visit___ 216;
              (let ty =
                 ___bisect_visit___ 212;
                 (try Ctype.expand_head env (newconstr path ty_args)
                  with
                  | Unify trace ->
                      (___bisect_visit___ 40;
                       raise
                         (Error ((styp.ptyp_loc), env, (Type_mismatch trace))))) in
               ___bisect_visit___ 213;
               (let ty =
                  ___bisect_visit___ 210;
                  (match ty.desc with
                   | Tvariant row ->
                       (___bisect_visit___ 38;
                        (let row = ___bisect_visit___ 204; Btype.row_repr row in
                         ___bisect_visit___ 205;
                         (let fields =
                            ___bisect_visit___ 202;
                            List.map
                              (fun (l, f) ->
                                 ___bisect_visit___ 201;
                                 (l,
                                   ((match Btype.row_field_repr f with
                                     | Rpresent (Some ty) ->
                                         (___bisect_visit___ 35;
                                          Reither
                                            (false, [ty], false, (ref None)))
                                     | Rpresent (None) ->
                                         (___bisect_visit___ 36;
                                          Reither
                                            (true, [], false, (ref None)))
                                     | _ -> (___bisect_visit___ 37; f)))))
                              row.row_fields in
                          ___bisect_visit___ 203;
                          (let row =
                             ___bisect_visit___ 199;
                             {
                               row_closed = true;
                               row_fields = fields;
                               row_bound = ();
                               row_name = (Some (path, ty_args));
                               row_fixed = false;
                               row_more = (newvar ())
                             } in
                           ___bisect_visit___ 200;
                           (let static =
                              ___bisect_visit___ 197; Btype.static_row row in
                            ___bisect_visit___ 198;
                            (let row =
                               ___bisect_visit___ 195;
                               if static
                               then
                                 (___bisect_visit___ 194;
                                  { row with row_more = (newty Tnil) })
                               else
                                 (___bisect_visit___ 193;
                                  if policy <> Univars
                                  then (___bisect_visit___ 192; row)
                                  else
                                    (___bisect_visit___ 191;
                                     {
                                       row with
                                       row_more = (new_pre_univar ())
                                     })) in
                             ___bisect_visit___ 196; newty (Tvariant row)))))))
                   | Tobject (fi, _) ->
                       (___bisect_visit___ 39;
                        (let (_, tv) =
                           ___bisect_visit___ 208; flatten_fields fi in
                         ___bisect_visit___ 209;
                         if policy = Univars
                         then
                           (___bisect_visit___ 206;
                            pre_univars := (tv :: (!pre_univars)));
                         ___bisect_visit___ 207;
                         ty))
                   | _ -> assert false) in
                ___bisect_visit___ 211;
                ctyp (Ttyp_class (path, lid, args)) ty)))))))
     | Ptyp_alias (st, alias) ->
         (___bisect_visit___ 93;
          (let cty =
             ___bisect_visit___ 271;
             (try
                let t =
                  ___bisect_visit___ 269;
                  (try List.assoc alias (!univars)
                   with
                   | Not_found ->
                       (___bisect_visit___ 58;
                        instance
                          (fst (TyVarMap.find alias (!used_variables))))) in
                ___bisect_visit___ 270;
                (let ty = ___bisect_visit___ 267; transl_type env policy st in
                 ___bisect_visit___ 268;
                 (try unify_var env t ty.ctyp_type
                  with
                  | Unify trace ->
                      (___bisect_visit___ 57;
                       (let trace =
                          ___bisect_visit___ 264;
                          Unification_trace.swap trace in
                        ___bisect_visit___ 265;
                        raise
                          (Error
                             ((styp.ptyp_loc), env,
                               (Alias_type_mismatch trace))))));
                 ___bisect_visit___ 266;
                 ty)
              with
              | Not_found ->
                  (___bisect_visit___ 59;
                   if !Clflags.principal
                   then (___bisect_visit___ 262; begin_def ());
                   ___bisect_visit___ 263;
                   (let t = ___bisect_visit___ 260; newvar () in
                    ___bisect_visit___ 261;
                    used_variables :=
                      (TyVarMap.add alias (t, (styp.ptyp_loc))
                         (!used_variables));
                    ___bisect_visit___ 259;
                    (let ty =
                       ___bisect_visit___ 257; transl_type env policy st in
                     ___bisect_visit___ 258;
                     (try unify_var env t ty.ctyp_type
                      with
                      | Unify trace ->
                          (___bisect_visit___ 56;
                           (let trace =
                              ___bisect_visit___ 254;
                              Unification_trace.swap trace in
                            ___bisect_visit___ 255;
                            raise
                              (Error
                                 ((styp.ptyp_loc), env,
                                   (Alias_type_mismatch trace))))));
                     ___bisect_visit___ 256;
                     if !Clflags.principal
                     then
                       (___bisect_visit___ 252;
                        end_def ();
                        ___bisect_visit___ 251;
                        generalize_structure t);
                     ___bisect_visit___ 253;
                     (let t = ___bisect_visit___ 249; instance t in
                      ___bisect_visit___ 250;
                      (let px = ___bisect_visit___ 247; Btype.proxy t in
                       ___bisect_visit___ 248;
                       (match px.desc with
                        | Tvar (None) ->
                            (___bisect_visit___ 53;
                             Btype.log_type px;
                             ___bisect_visit___ 244;
                             px.desc <- (Tvar (Some alias)))
                        | Tunivar (None) ->
                            (___bisect_visit___ 54;
                             Btype.log_type px;
                             ___bisect_visit___ 245;
                             px.desc <- (Tunivar (Some alias)))
                        | _ -> (___bisect_visit___ 55; ()));
                       ___bisect_visit___ 246;
                       { ty with ctyp_type = t })))))) in
           ___bisect_visit___ 272;
           ctyp (Ttyp_alias (cty, alias)) cty.ctyp_type))
     | Ptyp_variant (fields, closed, present) ->
         (___bisect_visit___ 94;
          (let name = ___bisect_visit___ 350; ref None in
           ___bisect_visit___ 351;
           (let mkfield l f =
              ___bisect_visit___ 348;
              newty
                (Tvariant
                   {
                     row_fields = [(l, f)];
                     row_more = (newvar ());
                     row_bound = ();
                     row_closed = true;
                     row_fixed = false;
                     row_name = None
                   }) in
            ___bisect_visit___ 349;
            (let hfields = ___bisect_visit___ 346; Hashtbl.create 17 in
             ___bisect_visit___ 347;
             (let add_typed_field loc l f =
                ___bisect_visit___ 344;
                (let h = ___bisect_visit___ 342; Btype.hash_variant l in
                 ___bisect_visit___ 343;
                 (try
                    let (l', f') =
                      ___bisect_visit___ 340; Hashtbl.find hfields h in
                    ___bisect_visit___ 341;
                    if l <> l'
                    then
                      (___bisect_visit___ 338;
                       raise
                         (Error
                            ((styp.ptyp_loc), env, (Variant_tags (l, l')))));
                    ___bisect_visit___ 339;
                    (let ty = ___bisect_visit___ 335; mkfield l f
                     and ty' = ___bisect_visit___ 336; mkfield l f' in
                     ___bisect_visit___ 337;
                     if equal env false [ty] [ty']
                     then (___bisect_visit___ 334; ())
                     else
                       (___bisect_visit___ 333;
                        (try unify env ty ty'
                         with
                         | Unify _trace ->
                             (___bisect_visit___ 81;
                              raise
                                (Error
                                   (loc, env,
                                     (Constructor_mismatch (ty, ty'))))))))
                  with
                  | Not_found ->
                      (___bisect_visit___ 82; Hashtbl.add hfields h (l, f)))) in
              ___bisect_visit___ 345;
              (let add_field field =
                 ___bisect_visit___ 331;
                 (let rf_loc = ___bisect_visit___ 329; field.prf_loc in
                  ___bisect_visit___ 330;
                  (let rf_attributes =
                     ___bisect_visit___ 327; field.prf_attributes in
                   ___bisect_visit___ 328;
                   (let rf_desc =
                      ___bisect_visit___ 325;
                      (match field.prf_desc with
                       | Rtag (l, c, stl) ->
                           (___bisect_visit___ 79;
                            name := None;
                            ___bisect_visit___ 307;
                            (let tl =
                               ___bisect_visit___ 305;
                               Builtin_attributes.warning_scope rf_attributes
                                 (fun () ->
                                    ___bisect_visit___ 304;
                                    List.map (transl_type env policy) stl) in
                             ___bisect_visit___ 306;
                             (let f =
                                ___bisect_visit___ 302;
                                (match present with
                                 | Some present when
                                     ___bisect_visit___ 65;
                                     not (List.mem l.txt present) ->
                                     (___bisect_visit___ 64;
                                      (let ty_tl =
                                         ___bisect_visit___ 295;
                                         List.map
                                           (fun cty ->
                                              ___bisect_visit___ 294;
                                              cty.ctyp_type) tl in
                                       ___bisect_visit___ 296;
                                       Reither (c, ty_tl, false, (ref None))))
                                 | _ ->
                                     (___bisect_visit___ 66;
                                      if
                                        (___bisect_visit___ 299;
                                         (List.length stl) > 1) ||
                                          ((___bisect_visit___ 298;
                                            (___bisect_visit___ 298; c) &&
                                              ((___bisect_visit___ 297;
                                                stl <> []))))
                                      then
                                        (___bisect_visit___ 300;
                                         raise
                                           (Error
                                              ((styp.ptyp_loc), env,
                                                (Present_has_conjunction
                                                   (l.txt)))));
                                      ___bisect_visit___ 301;
                                      (match tl with
                                       | [] ->
                                           (___bisect_visit___ 62;
                                            Rpresent None)
                                       | st::_ ->
                                           (___bisect_visit___ 63;
                                            Rpresent (Some (st.ctyp_type)))))) in
                              ___bisect_visit___ 303;
                              add_typed_field styp.ptyp_loc l.txt f;
                              ___bisect_visit___ 293;
                              Ttag (l, c, tl))))
                       | Rinherit sty ->
                           (___bisect_visit___ 80;
                            (let cty =
                               ___bisect_visit___ 323;
                               transl_type env policy sty in
                             ___bisect_visit___ 324;
                             (let ty = ___bisect_visit___ 321; cty.ctyp_type in
                              ___bisect_visit___ 322;
                              (let nm =
                                 ___bisect_visit___ 319;
                                 (match repr cty.ctyp_type with
                                  | { desc = Tconstr (p, tl, _) } ->
                                      (___bisect_visit___ 77; Some (p, tl))
                                  | _ -> (___bisect_visit___ 78; None)) in
                               ___bisect_visit___ 320;
                               (try
                                  Hashtbl.iter
                                    (fun _ ->
                                       fun _ ->
                                         ___bisect_visit___ 316; raise Exit)
                                    hfields;
                                  ___bisect_visit___ 317;
                                  name := nm
                                with
                                | Exit ->
                                    (___bisect_visit___ 76; name := None));
                               ___bisect_visit___ 318;
                               (let fl =
                                  ___bisect_visit___ 314;
                                  (match ((expand_head env cty.ctyp_type),
                                           nm)
                                   with
                                   | ({ desc = Tvariant row }, _) when
                                       ___bisect_visit___ 73;
                                       Btype.static_row row ->
                                       (___bisect_visit___ 72;
                                        (let row =
                                           ___bisect_visit___ 312;
                                           Btype.row_repr row in
                                         ___bisect_visit___ 313;
                                         row.row_fields))
                                   | ({ desc = Tvar _ }, Some (p, _)) ->
                                       (___bisect_visit___ 74;
                                        raise
                                          (Error
                                             ((sty.ptyp_loc), env,
                                               (Unbound_type_constructor_2 p))))
                                   | _ ->
                                       (___bisect_visit___ 75;
                                        raise
                                          (Error
                                             ((sty.ptyp_loc), env,
                                               (Not_a_variant ty))))) in
                                ___bisect_visit___ 315;
                                List.iter
                                  (fun (l, f) ->
                                     ___bisect_visit___ 310;
                                     (let f =
                                        ___bisect_visit___ 308;
                                        (match present with
                                         | Some present when
                                             ___bisect_visit___ 70;
                                             not (List.mem l present) ->
                                             (___bisect_visit___ 69;
                                              (match f with
                                               | Rpresent (Some ty) ->
                                                   (___bisect_visit___ 67;
                                                    Reither
                                                      (false, [ty], false,
                                                        (ref None)))
                                               | Rpresent (None) ->
                                                   (___bisect_visit___ 68;
                                                    Reither
                                                      (true, [], false,
                                                        (ref None)))
                                               | _ -> assert false))
                                         | _ -> (___bisect_visit___ 71; f)) in
                                      ___bisect_visit___ 309;
                                      add_typed_field sty.ptyp_loc l f)) fl;
                                ___bisect_visit___ 311;
                                Tinherit cty)))))) in
                    ___bisect_visit___ 326;
                    { rf_desc; rf_loc; rf_attributes }))) in
               ___bisect_visit___ 332;
               (let tfields =
                  ___bisect_visit___ 291; List.map add_field fields in
                ___bisect_visit___ 292;
                (let fields =
                   ___bisect_visit___ 289;
                   Hashtbl.fold
                     (fun _ ->
                        fun p -> fun l -> ___bisect_visit___ 288; p :: l)
                     hfields [] in
                 ___bisect_visit___ 290;
                 (match present with
                  | None -> (___bisect_visit___ 60; ())
                  | Some present ->
                      (___bisect_visit___ 61;
                       List.iter
                         (fun l ->
                            ___bisect_visit___ 286;
                            if not (List.mem_assoc l fields)
                            then
                              (___bisect_visit___ 285;
                               raise
                                 (Error
                                    ((styp.ptyp_loc), env,
                                      (Present_has_no_type l))))) present));
                 ___bisect_visit___ 287;
                 (let row =
                    ___bisect_visit___ 283;
                    {
                      row_fields = (List.rev fields);
                      row_more = (newvar ());
                      row_bound = ();
                      row_closed = (closed = Closed);
                      row_fixed = false;
                      row_name = (!name)
                    } in
                  ___bisect_visit___ 284;
                  (let static = ___bisect_visit___ 281; Btype.static_row row in
                   ___bisect_visit___ 282;
                   (let row =
                      ___bisect_visit___ 279;
                      if static
                      then
                        (___bisect_visit___ 278;
                         { row with row_more = (newty Tnil) })
                      else
                        (___bisect_visit___ 277;
                         if policy <> Univars
                         then (___bisect_visit___ 276; row)
                         else
                           (___bisect_visit___ 275;
                            { row with row_more = (new_pre_univar ()) })) in
                    ___bisect_visit___ 280;
                    (let ty = ___bisect_visit___ 273; newty (Tvariant row) in
                     ___bisect_visit___ 274;
                     ctyp (Ttyp_variant (tfields, closed, present)) ty))))))))))))
     | Ptyp_poly (vars, st) ->
         (___bisect_visit___ 95;
          (let vars =
             ___bisect_visit___ 378;
             List.map (fun v -> ___bisect_visit___ 377; v.txt) vars in
           ___bisect_visit___ 379;
           begin_def ();
           ___bisect_visit___ 376;
           (let new_univars =
              ___bisect_visit___ 374;
              List.map
                (fun name ->
                   ___bisect_visit___ 373; (name, (newvar ~name ()))) vars in
            ___bisect_visit___ 375;
            (let old_univars = ___bisect_visit___ 371; !univars in
             ___bisect_visit___ 372;
             univars := (new_univars @ (!univars));
             ___bisect_visit___ 370;
             (let cty = ___bisect_visit___ 368; transl_type env policy st in
              ___bisect_visit___ 369;
              (let ty = ___bisect_visit___ 366; cty.ctyp_type in
               ___bisect_visit___ 367;
               univars := old_univars;
               ___bisect_visit___ 365;
               end_def ();
               ___bisect_visit___ 364;
               generalize ty;
               ___bisect_visit___ 363;
               (let ty_list =
                  ___bisect_visit___ 361;
                  List.fold_left
                    (fun tyl ->
                       fun (name, ty1) ->
                         ___bisect_visit___ 360;
                         (let v = ___bisect_visit___ 358; Btype.proxy ty1 in
                          ___bisect_visit___ 359;
                          if deep_occur v ty
                          then
                            (___bisect_visit___ 357;
                             (match v.desc with
                              | Tvar name when
                                  ___bisect_visit___ 84;
                                  v.level = Btype.generic_level ->
                                  (___bisect_visit___ 83;
                                   v.desc <- (Tunivar name);
                                   ___bisect_visit___ 355;
                                   v
                                   ::
                                   tyl)
                              | _ ->
                                  (___bisect_visit___ 85;
                                   raise
                                     (Error
                                        ((styp.ptyp_loc), env,
                                          (Cannot_quantify (name, v)))))))
                          else (___bisect_visit___ 356; tyl))) [] new_univars in
                ___bisect_visit___ 362;
                (let ty' =
                   ___bisect_visit___ 353;
                   Btype.newgenty (Tpoly (ty, (List.rev ty_list))) in
                 ___bisect_visit___ 354;
                 unify_var env (newvar ()) ty';
                 ___bisect_visit___ 352;
                 ctyp (Ttyp_poly (vars, cty)) ty'))))))))
     | Ptyp_package (p, l) ->
         (___bisect_visit___ 96;
          (let (l, mty) =
             ___bisect_visit___ 394;
             create_package_mty true styp.ptyp_loc env (p, l) in
           ___bisect_visit___ 395;
           (let z = ___bisect_visit___ 392; narrow () in
            ___bisect_visit___ 393;
            (let mty = ___bisect_visit___ 390; (!transl_modtype) env mty in
             ___bisect_visit___ 391;
             widen z;
             ___bisect_visit___ 389;
             (let ptys =
                ___bisect_visit___ 387;
                List.map
                  (fun (s, pty) ->
                     ___bisect_visit___ 386;
                     (s, (transl_type env policy pty))) l in
              ___bisect_visit___ 388;
              (let path =
                 ___bisect_visit___ 384;
                 (!transl_modtype_longident) styp.ptyp_loc env p.txt in
               ___bisect_visit___ 385;
               (let ty =
                  ___bisect_visit___ 382;
                  newty
                    (Tpackage
                       (path,
                         (List.map
                            (fun (s, _pty) -> ___bisect_visit___ 380; s.txt)
                            l),
                         (List.map
                            (fun (_, cty) ->
                               ___bisect_visit___ 381; cty.ctyp_type) ptys))) in
                ___bisect_visit___ 383;
                ctyp
                  (Ttyp_package
                     {
                       pack_path = path;
                       pack_type = (mty.mty_type);
                       pack_fields = ptys;
                       pack_txt = p
                     }) ty)))))))
     | Ptyp_extension ext ->
         (___bisect_visit___ 97;
          raise (Error_forward (Builtin_attributes.error_of_extension ext))))))
and transl_poly_type env policy t =
  ___bisect_visit___ 401;
  transl_type env policy (Ast_helper.Typ.force_poly t)
and transl_fields env policy o fields =
  ___bisect_visit___ 446;
  (let hfields = ___bisect_visit___ 444; Hashtbl.create 17 in
   ___bisect_visit___ 445;
   (let add_typed_field loc l ty =
      ___bisect_visit___ 442;
      (try
         let ty' = ___bisect_visit___ 440; Hashtbl.find hfields l in
         ___bisect_visit___ 441;
         if equal env false [ty] [ty']
         then (___bisect_visit___ 439; ())
         else
           (___bisect_visit___ 438;
            (try unify env ty ty'
             with
             | Unify _trace ->
                 (___bisect_visit___ 112;
                  raise (Error (loc, env, (Method_mismatch (l, ty, ty')))))))
       with | Not_found -> (___bisect_visit___ 113; Hashtbl.add hfields l ty)) in
    ___bisect_visit___ 443;
    (let add_field { pof_desc; pof_loc; pof_attributes } =
       ___bisect_visit___ 436;
       (let of_loc = ___bisect_visit___ 434; pof_loc in
        ___bisect_visit___ 435;
        (let of_attributes = ___bisect_visit___ 432; pof_attributes in
         ___bisect_visit___ 433;
         (let of_desc =
            ___bisect_visit___ 430;
            (match pof_desc with
             | Otag (s, ty1) ->
                 (___bisect_visit___ 110;
                  (let ty1 =
                     ___bisect_visit___ 416;
                     Builtin_attributes.warning_scope of_attributes
                       (fun () ->
                          ___bisect_visit___ 415;
                          transl_poly_type env policy ty1) in
                   ___bisect_visit___ 417;
                   (let field = ___bisect_visit___ 413; OTtag (s, ty1) in
                    ___bisect_visit___ 414;
                    add_typed_field ty1.ctyp_loc s.txt ty1.ctyp_type;
                    ___bisect_visit___ 412;
                    field)))
             | Oinherit sty ->
                 (___bisect_visit___ 111;
                  (let cty =
                     ___bisect_visit___ 428; transl_type env policy sty in
                   ___bisect_visit___ 429;
                   (let nm =
                      ___bisect_visit___ 426;
                      (match repr cty.ctyp_type with
                       | { desc = Tconstr (p, _, _) } ->
                           (___bisect_visit___ 108; Some p)
                       | _ -> (___bisect_visit___ 109; None)) in
                    ___bisect_visit___ 427;
                    (let t =
                       ___bisect_visit___ 424; expand_head env cty.ctyp_type in
                     ___bisect_visit___ 425;
                     (match (t, nm) with
                      | ({
                           desc = Tobject
                             ({ desc = (Tfield _|Tnil as tf) }, _)
                           },
                         _) as ___bisect_matched_value___ ->
                          ((((match ___bisect_matched_value___ with
                              | ({
                                   desc = Tobject
                                     ({ desc = (Tfield _ as tf) }, _)
                                   },
                                 _) ->
                                  (___bisect_visit___ 104;
                                   ___bisect_visit___ 103;
                                   ())
                              | ({
                                   desc = Tobject
                                     ({ desc = (Tnil as tf) }, _)
                                   },
                                 _) ->
                                  (___bisect_visit___ 105;
                                   ___bisect_visit___ 103;
                                   ())
                              | _ -> ()))
                           [@ocaml.warning "-4-8-9-11-26-27-28"]);
                           if opened_object t
                           then
                             (___bisect_visit___ 422;
                              raise
                                (Error
                                   ((sty.ptyp_loc), env, (Opened_object nm))));
                           ___bisect_visit___ 423;
                           (let rec iter_add =
                              ___bisect_visit___ 420;
                              (function
                               | Tfield (s, _k, ty1, ty2) ->
                                   (___bisect_visit___ 101;
                                    add_typed_field sty.ptyp_loc s ty1;
                                    ___bisect_visit___ 419;
                                    iter_add ty2.desc)
                               | Tnil -> (___bisect_visit___ 102; ())
                               | _ -> assert false) in
                            ___bisect_visit___ 421;
                            iter_add tf;
                            ___bisect_visit___ 418;
                            OTinherit cty))
                      | ({ desc = Tvar _ }, Some p) ->
                          (___bisect_visit___ 106;
                           raise
                             (Error
                                ((sty.ptyp_loc), env,
                                  (Unbound_type_constructor_2 p))))
                      | _ ->
                          (___bisect_visit___ 107;
                           raise
                             (Error ((sty.ptyp_loc), env, (Not_an_object t)))))))))) in
          ___bisect_visit___ 431; { of_desc; of_loc; of_attributes }))) in
     ___bisect_visit___ 437;
     (let object_fields = ___bisect_visit___ 410; List.map add_field fields in
      ___bisect_visit___ 411;
      (let fields =
         ___bisect_visit___ 408;
         Hashtbl.fold
           (fun s -> fun ty -> fun l -> ___bisect_visit___ 407; (s, ty) :: l)
           hfields [] in
       ___bisect_visit___ 409;
       (let ty_init =
          ___bisect_visit___ 405;
          (match (o, policy) with
           | (Closed, _) -> (___bisect_visit___ 98; newty Tnil)
           | (Open, Univars) -> (___bisect_visit___ 99; new_pre_univar ())
           | (Open, _) -> (___bisect_visit___ 100; newvar ())) in
        ___bisect_visit___ 406;
        (let ty =
           ___bisect_visit___ 403;
           List.fold_left
             (fun ty ->
                fun (s, ty') ->
                  ___bisect_visit___ 402;
                  newty (Tfield (s, Fpresent, ty', ty))) ty_init fields in
         ___bisect_visit___ 404; (ty, object_fields))))))))
let rec make_fixed_univars ty =
  let ty = ___bisect_visit___ 454; repr ty in
  ___bisect_visit___ 455;
  if ty.level >= Btype.lowest_level
  then
    (___bisect_visit___ 453;
     Btype.mark_type_node ty;
     ___bisect_visit___ 452;
     (match ty.desc with
      | Tvariant row ->
          (___bisect_visit___ 116;
           (let row = ___bisect_visit___ 450; Btype.row_repr row in
            ___bisect_visit___ 451;
            if Btype.is_Tunivar (Btype.row_more row)
            then
              (___bisect_visit___ 448;
               ty.desc <-
                 (Tvariant
                    {
                      row with
                      row_fixed = true;
                      row_fields =
                        (List.map
                           (fun ((s, f) as p) ->
                              ___bisect_visit___ 447;
                              (match Btype.row_field_repr f with
                               | Reither (c, tl, _m, r) ->
                                   (___bisect_visit___ 114;
                                    (s, (Reither (c, tl, true, r))))
                               | _ -> (___bisect_visit___ 115; p)))
                           row.row_fields)
                    }));
            ___bisect_visit___ 449;
            Btype.iter_row make_fixed_univars row))
      | _ ->
          (___bisect_visit___ 117; Btype.iter_type_expr make_fixed_univars ty)))
let make_fixed_univars ty =
  make_fixed_univars ty; ___bisect_visit___ 456; Btype.unmark_type ty
let create_package_mty = ___bisect_visit___ 457; create_package_mty false
let globalize_used_variables env fixed =
  ___bisect_visit___ 478;
  (let r = ___bisect_visit___ 476; ref [] in
   ___bisect_visit___ 477;
   TyVarMap.iter
     (fun name ->
        fun (ty, loc) ->
          ___bisect_visit___ 474;
          (let v = ___bisect_visit___ 472; new_global_var () in
           ___bisect_visit___ 473;
           (let snap = ___bisect_visit___ 470; Btype.snapshot () in
            ___bisect_visit___ 471;
            if
              (try unify env v ty; ___bisect_visit___ 468; true
               with
               | _ ->
                   (___bisect_visit___ 121;
                    Btype.backtrack snap;
                    ___bisect_visit___ 467;
                    false))
            then
              (___bisect_visit___ 469;
               (try
                  r := ((loc, v, (TyVarMap.find name (!type_variables))) ::
                    (!r))
                with
                | Not_found ->
                    (___bisect_visit___ 120;
                     if
                       (___bisect_visit___ 464; fixed) &&
                         ((___bisect_visit___ 463; Btype.is_Tvar (repr ty)))
                     then
                       (___bisect_visit___ 465;
                        raise
                          (Error
                             (loc, env, (Unbound_type_variable ("'" ^ name)))));
                     ___bisect_visit___ 466;
                     (let v2 = ___bisect_visit___ 461; new_global_var () in
                      ___bisect_visit___ 462;
                      r := ((loc, v, v2) :: (!r));
                      ___bisect_visit___ 460;
                      type_variables :=
                        (TyVarMap.add name v2 (!type_variables)))))))))
     (!used_variables);
   ___bisect_visit___ 475;
   used_variables := TyVarMap.empty;
   ___bisect_visit___ 459;
   (fun () ->
      ___bisect_visit___ 458;
      List.iter
        (function
         | (loc, t1, t2) ->
             (___bisect_visit___ 119;
              (try unify env t1 t2
               with
               | Unify trace ->
                   (___bisect_visit___ 118;
                    raise (Error (loc, env, (Type_mismatch trace))))))) (
        !r)))
let transl_simple_type env fixed styp =
  ___bisect_visit___ 487;
  univars := [];
  ___bisect_visit___ 486;
  used_variables := TyVarMap.empty;
  ___bisect_visit___ 485;
  (let typ =
     ___bisect_visit___ 483;
     transl_type env
       (if fixed
        then (___bisect_visit___ 482; Fixed)
        else (___bisect_visit___ 481; Extensible)) styp in
   ___bisect_visit___ 484;
   globalize_used_variables env fixed ();
   ___bisect_visit___ 480;
   make_fixed_univars typ.ctyp_type;
   ___bisect_visit___ 479;
   typ)
let transl_simple_type_univars env styp =
  ___bisect_visit___ 510;
  univars := [];
  ___bisect_visit___ 509;
  used_variables := TyVarMap.empty;
  ___bisect_visit___ 508;
  pre_univars := [];
  ___bisect_visit___ 507;
  begin_def ();
  ___bisect_visit___ 506;
  (let typ = ___bisect_visit___ 504; transl_type env Univars styp in
   ___bisect_visit___ 505;
   (let new_variables = ___bisect_visit___ 502; !used_variables in
    ___bisect_visit___ 503;
    used_variables := TyVarMap.empty;
    ___bisect_visit___ 501;
    TyVarMap.iter
      (fun name ->
         fun p ->
           ___bisect_visit___ 499;
           if TyVarMap.mem name (!type_variables)
           then
             (___bisect_visit___ 498;
              used_variables := (TyVarMap.add name p (!used_variables))))
      new_variables;
    ___bisect_visit___ 500;
    globalize_used_variables env false ();
    ___bisect_visit___ 497;
    end_def ();
    ___bisect_visit___ 496;
    generalize typ.ctyp_type;
    ___bisect_visit___ 495;
    (let univs =
       ___bisect_visit___ 493;
       List.fold_left
         (fun acc ->
            fun v ->
              ___bisect_visit___ 492;
              (let v = ___bisect_visit___ 490; repr v in
               ___bisect_visit___ 491;
               (match v.desc with
                | Tvar name when
                    ___bisect_visit___ 123; v.level = Btype.generic_level ->
                    (___bisect_visit___ 122;
                     v.desc <- (Tunivar name);
                     ___bisect_visit___ 489;
                     v
                     ::
                     acc)
                | _ -> (___bisect_visit___ 124; acc)))) [] (!pre_univars) in
     ___bisect_visit___ 494;
     make_fixed_univars typ.ctyp_type;
     ___bisect_visit___ 488;
     {
       typ with
       ctyp_type =
         (instance (Btype.newgenty (Tpoly ((typ.ctyp_type), univs))))
     })))
let transl_simple_type_delayed env styp =
  ___bisect_visit___ 516;
  univars := [];
  ___bisect_visit___ 515;
  used_variables := TyVarMap.empty;
  ___bisect_visit___ 514;
  (let typ = ___bisect_visit___ 512; transl_type env Extensible styp in
   ___bisect_visit___ 513;
   make_fixed_univars typ.ctyp_type;
   ___bisect_visit___ 511;
   (typ, (globalize_used_variables env false)))
let transl_type_scheme env styp =
  ___bisect_visit___ 523;
  reset_type_variables ();
  ___bisect_visit___ 522;
  begin_def ();
  ___bisect_visit___ 521;
  (let typ = ___bisect_visit___ 519; transl_simple_type env false styp in
   ___bisect_visit___ 520;
   end_def ();
   ___bisect_visit___ 518;
   generalize typ.ctyp_type;
   ___bisect_visit___ 517;
   typ)
open Format
open Printtyp
let spellcheck ppf fold env lid =
  ___bisect_visit___ 531;
  (let choices ~path  name =
     ___bisect_visit___ 529;
     (let env =
        ___bisect_visit___ 527;
        fold (fun x -> fun xs -> ___bisect_visit___ 526; x :: xs) path env [] in
      ___bisect_visit___ 528; Misc.spellcheck env name) in
   ___bisect_visit___ 530;
   (match lid with
    | Longident.Lapply _ -> (___bisect_visit___ 125; ())
    | Longident.Lident s ->
        (___bisect_visit___ 126;
         Misc.did_you_mean ppf
           (fun () -> ___bisect_visit___ 524; choices ~path:None s))
    | Longident.Ldot (r, s) ->
        (___bisect_visit___ 127;
         Misc.did_you_mean ppf
           (fun () -> ___bisect_visit___ 525; choices ~path:(Some r) s))))
let fold_descr fold get_name f =
  ___bisect_visit___ 533;
  fold
    (fun descr -> fun acc -> ___bisect_visit___ 532; f (get_name descr) acc)
let fold_simple fold4 f =
  ___bisect_visit___ 535;
  fold4
    (fun name ->
       fun _path ->
         fun _descr -> fun acc -> ___bisect_visit___ 534; f name acc)
let fold_values f =
  Env.fold_values
    (fun name ->
       fun _path ->
         fun descr ->
           fun acc ->
             ___bisect_visit___ 536;
             (match descr.val_kind with
              | Val_unbound _ -> (___bisect_visit___ 128; acc)
              | _ -> (___bisect_visit___ 129; f name acc)))
let fold_types = ___bisect_visit___ 537; fold_simple Env.fold_types
let fold_modules = ___bisect_visit___ 538; fold_simple Env.fold_modules
let fold_constructors =
  ___bisect_visit___ 540;
  fold_descr Env.fold_constructors
    (fun d -> ___bisect_visit___ 539; d.cstr_name)
let fold_labels =
  ___bisect_visit___ 542;
  fold_descr Env.fold_labels (fun d -> ___bisect_visit___ 541; d.lbl_name)
let fold_classes = ___bisect_visit___ 543; fold_simple Env.fold_classes
let fold_modtypes = ___bisect_visit___ 544; fold_simple Env.fold_modtypes
let fold_cltypes = ___bisect_visit___ 545; fold_simple Env.fold_cltypes
let report_error env ppf =
  ___bisect_visit___ 580;
  (function
   | Unbound_type_variable name ->
       (___bisect_visit___ 145;
        (let add_name name _ l =
           ___bisect_visit___ 551;
           if name = "_"
           then (___bisect_visit___ 550; l)
           else (___bisect_visit___ 549; ("'" ^ name) :: l) in
         ___bisect_visit___ 552;
         (let names =
            ___bisect_visit___ 547;
            TyVarMap.fold add_name (!type_variables) [] in
          ___bisect_visit___ 548;
          fprintf ppf
            "The type variable %s is unbound in this type declaration.@ %a"
            name did_you_mean
            (fun () -> ___bisect_visit___ 546; Misc.spellcheck names name))))
   | Unbound_type_constructor lid ->
       (___bisect_visit___ 146;
        fprintf ppf "Unbound type constructor %a" longident lid;
        ___bisect_visit___ 553;
        spellcheck ppf fold_types env lid)
   | Unbound_type_constructor_2 p ->
       (___bisect_visit___ 147;
        fprintf ppf "The type constructor@ %a@ is not yet completely defined"
          path p)
   | Type_arity_mismatch (lid, expected, provided) ->
       (___bisect_visit___ 148;
        fprintf ppf
          "@[The type constructor %a@ expects %i argument(s),@ but is here applied to %i argument(s)@]"
          longident lid expected provided)
   | Bound_type_variable name ->
       (___bisect_visit___ 149;
        fprintf ppf "Already bound type parameter %a" Pprintast.tyvar name)
   | Recursive_type ->
       (___bisect_visit___ 150; fprintf ppf "This type is recursive")
   | Unbound_row_variable lid ->
       (___bisect_visit___ 151;
        fprintf ppf "Unbound row variable in #%a" longident lid)
   | Type_mismatch trace ->
       (___bisect_visit___ 152;
        Printtyp.report_unification_error ppf Env.empty trace
          (function
           | ppf -> (___bisect_visit___ 130; fprintf ppf "This type"))
          (function
           | ppf ->
               (___bisect_visit___ 131;
                fprintf ppf "should be an instance of type")))
   | Alias_type_mismatch trace ->
       (___bisect_visit___ 153;
        Printtyp.report_unification_error ppf Env.empty trace
          (function
           | ppf ->
               (___bisect_visit___ 132;
                fprintf ppf "This alias is bound to type"))
          (function
           | ppf ->
               (___bisect_visit___ 133;
                fprintf ppf "but is used as an instance of type")))
   | Present_has_conjunction l ->
       (___bisect_visit___ 154;
        fprintf ppf "The present constructor %s has a conjunctive type" l)
   | Present_has_no_type l ->
       (___bisect_visit___ 155;
        fprintf ppf
          "@[<v>@[The constructor %s is missing from the upper bound@ (between '<'@ and '>')@ of this polymorphic variant@ but is present in@ its lower bound (after '>').@]@,@[Hint: Either add `%s in the upper bound,@ or remove it@ from the lower bound.@]@]"
          l l)
   | Constructor_mismatch (ty, ty') ->
       (___bisect_visit___ 156;
        wrap_printing_env ~error:true env
          (fun () ->
             ___bisect_visit___ 555;
             Printtyp.reset_and_mark_loops_list [ty; ty'];
             ___bisect_visit___ 554;
             fprintf ppf "@[<hov>%s %a@ %s@ %a@]"
               "This variant type contains a constructor" (!Oprint.out_type)
               (tree_of_typexp false ty) "which should be" (!Oprint.out_type)
               (tree_of_typexp false ty')))
   | Not_a_variant ty ->
       (___bisect_visit___ 157;
        Printtyp.reset_and_mark_loops ty;
        ___bisect_visit___ 558;
        fprintf ppf
          "@[The type %a@ does not expand to a polymorphic variant type@]"
          Printtyp.type_expr ty;
        ___bisect_visit___ 557;
        (match ty.desc with
         | Tvar (Some s) ->
             (___bisect_visit___ 134;
              Misc.did_you_mean ppf
                (fun () -> ___bisect_visit___ 556; ["`" ^ s]))
         | _ -> (___bisect_visit___ 135; ())))
   | Variant_tags (lab1, lab2) ->
       (___bisect_visit___ 158;
        fprintf ppf
          "@[Variant tags `%s@ and `%s have the same hash value.@ %s@]" lab1
          lab2 "Change one of them.")
   | Invalid_variable_name name ->
       (___bisect_visit___ 159;
        fprintf ppf "The type variable name %s is not allowed in programs"
          name)
   | Cannot_quantify (name, v) ->
       (___bisect_visit___ 160;
        fprintf ppf
          "@[<hov>The universal type variable %a cannot be generalized:@ "
          Pprintast.tyvar name;
        ___bisect_visit___ 564;
        if Btype.is_Tvar v
        then (___bisect_visit___ 562; fprintf ppf "it escapes its scope")
        else
          (___bisect_visit___ 561;
           if Btype.is_Tunivar v
           then
             (___bisect_visit___ 560;
              fprintf ppf "it is already bound to another variable")
           else
             (___bisect_visit___ 559;
              fprintf ppf "it is bound to@ %a" Printtyp.type_expr v));
        ___bisect_visit___ 563;
        fprintf ppf ".@]")
   | Multiple_constraints_on_type s ->
       (___bisect_visit___ 161;
        fprintf ppf "Multiple constraints for type %a" longident s)
   | Method_mismatch (l, ty, ty') ->
       (___bisect_visit___ 162;
        wrap_printing_env ~error:true env
          (fun () ->
             ___bisect_visit___ 566;
             Printtyp.reset_and_mark_loops_list [ty; ty'];
             ___bisect_visit___ 565;
             fprintf ppf
               "@[<hov>Method '%s' has type %a,@ which should be %a@]" l
               Printtyp.type_expr ty Printtyp.type_expr ty'))
   | Unbound_value lid ->
       (___bisect_visit___ 163;
        fprintf ppf "Unbound value %a" longident lid;
        ___bisect_visit___ 567;
        spellcheck ppf fold_values env lid)
   | Unbound_module lid ->
       (___bisect_visit___ 164;
        fprintf ppf "Unbound module %a" longident lid;
        ___bisect_visit___ 568;
        spellcheck ppf fold_modules env lid)
   | Unbound_constructor lid ->
       (___bisect_visit___ 165;
        fprintf ppf "Unbound constructor %a" longident lid;
        ___bisect_visit___ 569;
        spellcheck ppf fold_constructors env lid)
   | Unbound_label lid ->
       (___bisect_visit___ 166;
        fprintf ppf "Unbound record field %a" longident lid;
        ___bisect_visit___ 570;
        spellcheck ppf fold_labels env lid)
   | Unbound_class lid ->
       (___bisect_visit___ 167;
        fprintf ppf "Unbound class %a" longident lid;
        ___bisect_visit___ 571;
        spellcheck ppf fold_classes env lid)
   | Unbound_modtype lid ->
       (___bisect_visit___ 168;
        fprintf ppf "Unbound module type %a" longident lid;
        ___bisect_visit___ 572;
        spellcheck ppf fold_modtypes env lid)
   | Unbound_cltype lid ->
       (___bisect_visit___ 169;
        fprintf ppf "Unbound class type %a" longident lid;
        ___bisect_visit___ 573;
        spellcheck ppf fold_cltypes env lid)
   | Ill_typed_functor_application (flid, mlid, details) ->
       (___bisect_visit___ 170;
        (match details with
         | None ->
             (___bisect_visit___ 136;
              fprintf ppf "@[Ill-typed functor application %a(%a)@]"
                longident flid longident mlid)
         | Some inclusion_error ->
             (___bisect_visit___ 137;
              fprintf ppf
                "@[The type of %a does not match %a's parameter@\n%a@]"
                longident mlid longident flid Includemod.report_error
                inclusion_error)))
   | Illegal_reference_to_recursive_module ->
       (___bisect_visit___ 171;
        fprintf ppf "Illegal recursive module reference")
   | Wrong_use_of_module (lid, details) ->
       (___bisect_visit___ 172;
        (match details with
         | `Structure_used_as_functor ->
             (___bisect_visit___ 138;
              fprintf ppf
                "@[The module %a is a structure, it cannot be applied@]"
                longident lid)
         | `Abstract_used_as_functor ->
             (___bisect_visit___ 139;
              fprintf ppf
                "@[The module %a is abstract, it cannot be applied@]"
                longident lid)
         | `Functor_used_as_structure ->
             (___bisect_visit___ 140;
              fprintf ppf
                "@[The module %a is a functor, it cannot have any components@]"
                longident lid)
         | `Abstract_used_as_structure ->
             (___bisect_visit___ 141;
              fprintf ppf
                "@[The module %a is abstract, it cannot have any components@]"
                longident lid)
         | `Generative_used_as_applicative ->
             (___bisect_visit___ 142;
              fprintf ppf
                "@[The functor %a is generative,@ it@ cannot@ be@ applied@ in@ type@ expressions@]"
                longident lid)))
   | Cannot_scrape_alias (lid, p) ->
       (___bisect_visit___ 173;
        fprintf ppf
          "The module %a is an alias for module %a, which is missing"
          longident lid path p)
   | Opened_object nm ->
       (___bisect_visit___ 174;
        fprintf ppf "Illegal open object type%a"
          (fun ppf ->
             ___bisect_visit___ 574;
             (function
              | Some p -> (___bisect_visit___ 143; fprintf ppf "@ %a" path p)
              | None -> (___bisect_visit___ 144; fprintf ppf ""))) nm)
   | Not_an_object ty ->
       (___bisect_visit___ 175;
        Printtyp.reset_and_mark_loops ty;
        ___bisect_visit___ 575;
        fprintf ppf "@[The type %a@ is not an object type@]"
          Printtyp.type_expr ty)
   | Unbound_value_missing_rec (lid, loc) ->
       (___bisect_visit___ 176;
        fprintf ppf "Unbound value %a" longident lid;
        ___bisect_visit___ 579;
        spellcheck ppf fold_values env lid;
        ___bisect_visit___ 578;
        (let (_, line, _) =
           ___bisect_visit___ 576;
           Location.get_pos_info loc.Location.loc_start in
         ___bisect_visit___ 577;
         fprintf ppf "@.@[%s@ %s %i@]"
           "Hint: If this is a recursive definition,"
           "you should add the 'rec' keyword on line" line)))
let () =
  ___bisect_visit___ 581;
  Location.register_error_of_exn
    (function
     | Error (loc, env, err) ->
         (___bisect_visit___ 177;
          Some (Location.error_of_printer ~loc (report_error env) err))
     | Error_forward err -> (___bisect_visit___ 178; Some err)
     | _ -> (___bisect_visit___ 179; None))