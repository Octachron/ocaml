

module type Extension = sig type 'a t end


module Highlightable = struct
  type status = On | Off

  type 'a t =
    | Item of status * 'a
    | Ellipsis of int (** ellipsis lenght *)
end


module No_extension= struct
  type 'a t = 'a
end


type out_string =
  | Ostr_string
  | Ostr_bytes

type out_attribute =
  { oattr_name: string }

type out_rec_status =
  | Orec_not
  | Orec_first
  | Orec_next

type out_ext_status =
  | Oext_first
  | Oext_next
  | Oext_exception

module Make(Ext:Extension) = struct
  type 'a ext = 'a Ext.t
  type out_ident =
    | Oide_apply of out_ident ext * out_ident ext
    | Oide_dot of out_ident ext * string ext
    | Oide_ident of string ext

  type out_value =
    | Oval_array of out_value list
    | Oval_char of char
    | Oval_constr of out_ident * out_value list
    | Oval_ellipsis
    | Oval_float of float
    | Oval_int of int
    | Oval_int32 of int32
    | Oval_int64 of int64
    | Oval_nativeint of nativeint
    | Oval_list of out_value list
    | Oval_printer of (Format.formatter -> unit)
    | Oval_record of (out_ident * out_value) list
    | Oval_string of string * int * out_string (* string, size-to-print, kind *)
    | Oval_stuff of string
    | Oval_tuple of out_value list
    | Oval_variant of string * out_value option


  type out_type =
    | Otyp_abstract
    | Otyp_open
    | Otyp_alias of out_type ext * string ext
    | Otyp_arrow of out_labelled ext * out_type ext
    | Otyp_class of bool ext * out_ident ext * out_type ext list
    | Otyp_constr of out_ident ext * out_type ext list
    | Otyp_manifest of out_type ext * out_type ext
    | Otyp_object of out_labelled ext list * bool ext option ext
    | Otyp_record of out_field ext list
    | Otyp_stuff of string
    | Otyp_sum of out_constructor ext list
    | Otyp_tuple of out_type ext list
    | Otyp_var of bool ext * string ext
    | Otyp_variant of
        bool ext * out_variant * bool ext * string ext list option ext
    | Otyp_poly of string ext list * out_type ext
    | Otyp_module of string ext * string ext list * out_type ext list
    | Otyp_attribute of out_type ext * out_attribute ext

  and out_labelled = (string ext * out_type ext)

  and out_constructor =
    {cname:string ext; args:out_type ext list; ret:out_type ext option ext}

  and out_variant =
    | Ovar_fields of out_var_field ext list
    | Ovar_typ of out_type ext

  and out_var_field = {tag:string ext; ampersand:bool ext; conj: out_type ext list}

  and out_field = {label:string ext; mut: bool ext; typ:out_type ext}

  type type_constraint = {lhs:out_type ext ;rhs:out_type ext}

  type out_class_type =
    | Octy_constr of out_ident ext * out_type ext list
    | Octy_arrow of out_labelled ext * out_class_type ext
    | Octy_signature of out_type ext option ext * out_class_sig_item ext list
  and out_class_sig_item =
    | Ocsg_constraint of type_constraint
    | Ocsg_method of string ext * bool ext * bool ext * out_type ext
    | Ocsg_value of string ext * bool ext * bool ext * out_type ext

  type type_param = {covariant:bool ext;contravariant:bool ext;name:string ext}

  type out_module_type =
    | Omty_abstract
    | Omty_functor of functor_arg ext * out_module_type ext
    | Omty_ident of out_ident ext
    | Omty_signature of out_sig_item ext list
    | Omty_alias of out_ident ext
  and functor_arg = string ext * out_module_type ext option ext
  and out_sig_item =
    | Osig_class of
        bool ext * string ext * type_param ext list * out_class_type ext
        * out_rec_status ext
    | Osig_class_type of
        bool ext * string ext * type_param ext list * out_class_type ext
        * out_rec_status ext
    | Osig_typext of out_extension_constructor * out_ext_status ext
    | Osig_modtype of string ext * out_module_type ext
    | Osig_module of string ext * out_module_type ext * out_rec_status ext
    | Osig_type of out_type_decl * out_rec_status ext
    | Osig_value of out_val_decl
    | Osig_ellipsis
  and out_type_decl =
    { otype_name: string ext;
      otype_params: type_param ext list;
      otype_type: out_type ext;
      otype_private: Asttypes.private_flag ext;
      otype_immediate: bool ext;
      otype_unboxed: bool ext;
      otype_cstrs: type_constraint ext list }
  and out_extension_constructor =
    { oext_name: string ext;
      oext_type_name: string ext;
      oext_type_params: string ext list;
      oext_args: out_type ext list;
      oext_ret_type: out_type ext option ext;
      oext_private: Asttypes.private_flag ext }
  and out_type_extension =
    { otyext_name: string ext;
      otyext_params: string ext list;
      otyext_constructors: out_constructor ext list;
      otyext_private: Asttypes.private_flag ext }
  and out_val_decl =
    { oval_name: string ext;
      oval_type: out_type ext;
      oval_prims: string ext list;
      oval_attributes: out_attribute ext list }

  type out_phrase =
    | Ophr_eval of out_value * out_type
    | Ophr_signature of (out_sig_item * out_value option) list
    | Ophr_exception of (exn * out_value)

end

module type S = module type of Make(struct type 'a t end)

include Make(No_extension)
module Decorated = Make(Highlightable)

module Decorate = struct
  module D = Decorated
  let fwd x = Highlightable.(Item(Off,x))
  let fwds x = List.map fwd x
  let fmap f x = fwd (f x)

  let rec out_ident = function
    | Oide_apply(x,y) -> D.Oide_apply(ident x, ident y)
    | Oide_dot(x,y) -> D.Oide_dot(ident x, fwd y)
    | Oide_ident x -> D.Oide_ident(fwd x)
  and ident x = fmap out_ident x

  let may f = function None -> None | Some x -> Some (f x)
  let mayf f x = fwd (may f @@ x)

  let rec out_value = function
    | Oval_array x -> D.Oval_array (List.map out_value x)
    | Oval_char x -> D.Oval_char x
    | Oval_constr(x,y) -> D.Oval_constr( out_ident x, List.map out_value y)
    | Oval_ellipsis -> D.Oval_ellipsis
    | Oval_float f -> D.Oval_float f
    | Oval_int f -> D.Oval_int f
    | Oval_int32 n -> D.Oval_int32 n
    | Oval_int64 n -> D.Oval_int64 n
    | Oval_nativeint n -> D.Oval_nativeint n
    | Oval_list l -> D.Oval_list (List.map out_value l)
    | Oval_printer p -> D.Oval_printer p
    | Oval_record l ->
        D.Oval_record (List.map (fun (x,y) -> out_ident x, out_value y) l)
    | Oval_string (s,n,k) -> D.Oval_string(s,n, k)
    | Oval_stuff s -> D.Oval_stuff s
    | Oval_tuple v -> D.Oval_tuple (List.map out_value v)
    | Oval_variant (s,ov) -> D.Oval_variant (s, may out_value ov)

  let rec out_type = function
    | Otyp_abstract -> D.Otyp_abstract
    | Otyp_open -> D.Otyp_open
    | Otyp_alias(x,y) -> D.Otyp_alias(typ x,fwd y)
    | Otyp_arrow(l,t) -> D.Otyp_arrow(label l, typ t)
    | Otyp_class(b,id,ts) -> D.Otyp_class(fwd b, ident id,types ts)
    | Otyp_constr(id,ts) -> D.Otyp_constr(ident id, types ts)
    | Otyp_manifest(t,t') -> D.Otyp_manifest(typ t, typ t')
    | Otyp_object(lbls, o) -> D.Otyp_object(labels lbls, mayf fwd o)
    | Otyp_record r -> D.Otyp_record(fields r)
    | Otyp_stuff s -> D.Otyp_stuff s
    | Otyp_sum cntrs -> D.Otyp_sum (List.map constr cntrs)
    | Otyp_tuple ts -> D.Otyp_tuple(types ts)
    | Otyp_var(b,s) -> D.Otyp_var(fwd b, fwd s)
    | Otyp_variant (b,vars,b',tags) ->
      D.Otyp_variant(fwd b,out_variant vars, fwd b', mayf fwds tags )
    | Otyp_poly(us, t) -> D.Otyp_poly(fwds us, typ t)
    | Otyp_module (name,with',ts) -> D.Otyp_module(fwd name, fwds with', types ts)
    | Otyp_attribute(t, attrs) -> D.Otyp_attribute(typ t, fwd attrs)

  and typ x = fmap out_type x
  and types l = List.map typ l
  and label (lbl,t) = fwd (fwd lbl, typ t)
  and labels x = List.map label x
  and constr c =
    fwd {D.cname = fwd c.cname; args = types c.args; ret = mayf typ c.ret}

  and out_variant = function
    | Ovar_fields fs -> D.Ovar_fields (List.map var_field fs)
    | Ovar_typ t -> D.Ovar_typ (typ t)

  and var_field f =
    fwd {D.tag = fwd f.tag; ampersand = fwd f.ampersand; conj = types f.conj }

  and field f =
    {D.label= fwd f.label; mut = fwd f.mut; typ = typ f.typ }
  and fields x = List.map (fmap field) x

  let type_constraint c=
   {D.lhs= typ c.lhs ;rhs = typ c.rhs}

  let rec out_class_type = function
    | Octy_constr(id,ts) -> D.Octy_constr(ident id, types ts)
    | Octy_arrow(lbl,t) -> D.Octy_arrow(label lbl, class_type t)
    | Octy_signature(self, items) ->
        D.Octy_signature(mayf typ self, List.map (fmap out_class_sig_item) items)
  and out_class_sig_item = function
    | Ocsg_constraint x -> D.Ocsg_constraint (type_constraint x)
    | Ocsg_method(name,priv,virt,t) ->
        D.Ocsg_method(fwd name,fwd priv,fwd virt, typ t)
    | Ocsg_value(name,priv,virt,t) ->
        D.Ocsg_value(fwd name,fwd priv,fwd virt, typ t)
  and class_type x = fmap out_class_type x
  let type_param tp =
    fwd {D.covariant= fwd tp.covariant; contravariant= fwd tp.contravariant;
     name = fwd tp.name}

  let type_params = List.map type_param

  let rec out_module_type = function
    | Omty_abstract -> D.Omty_abstract
    | Omty_functor ( (name, arg), res) ->
        D.Omty_functor(fwd (fwd name, mayf module_type arg), module_type res)
    | Omty_ident id -> D.Omty_ident (ident id)
    | Omty_signature items -> D.Omty_signature (sigitems items)
    | Omty_alias id -> D.Omty_alias(ident id)
  and module_type x = fmap out_module_type x
  and out_sig_item = function
    | Osig_class (virt, name, tps, ct, recs) ->
        D.Osig_class(fwd virt,fwd name, type_params tps, class_type ct,fwd recs)
    | Osig_class_type(virt, name, tps, ct, recs) ->
        D.Osig_class_type(fwd virt,fwd name, type_params tps, class_type ct,fwd recs)
    | Osig_typext(ext,est) ->
        D.Osig_typext(extension_constructor ext, fwd est)
    | Osig_modtype(name,mt) -> D.Osig_modtype(fwd name,module_type mt)
    | Osig_module(name,mt,recs) ->
        D.Osig_module(fwd name,module_type mt, fwd recs)
    | Osig_type(decl, recs) -> D.Osig_type(type_decl decl, fwd recs)
    | Osig_value v -> D.Osig_value (val_decl v)
    | Osig_ellipsis -> D.Osig_ellipsis
  and sigitems x = List.map (fmap out_sig_item) x
  and type_decl d =
    { D.otype_name= fwd d.otype_name;
      otype_params= type_params d.otype_params;
      otype_type = typ d.otype_type;
      otype_private= fwd d.otype_private;
      otype_immediate= fwd d.otype_immediate;
      otype_unboxed= fwd d.otype_unboxed;
      otype_cstrs= List.map (fmap type_constraint) d.otype_cstrs }
  and extension_constructor e=
    { D.oext_name = fwd e.oext_name;
      oext_type_name = fwd e.oext_type_name;
      oext_type_params = fwds e.oext_type_params;
      oext_args = types e.oext_args;
      oext_ret_type = mayf typ e.oext_ret_type;
      oext_private = fwd e.oext_private }
  and out_type_extension e =
    { D.otyext_name= fwd e.otyext_name;
      otyext_params= fwds e.otyext_params;
      otyext_constructors= List.map constr e.otyext_constructors;
      otyext_private = fwd e.otyext_private}
  and val_decl v =
    { D.oval_name = fwd v.oval_name;
      oval_type = typ  v.oval_type;
      oval_prims = fwds v.oval_prims;
      oval_attributes = fwds v.oval_attributes}

  let out_signature = List.map out_sig_item
  let out_phrase = function
    | Ophr_eval(v,t) -> D.Ophr_eval (out_value v, out_type t)
    | Ophr_signature l ->
        D.Ophr_signature( List.map (fun (x,y) -> out_sig_item x, may out_value y) l )
    | Ophr_exception (exn, v) ->
        D.Ophr_exception(exn, out_value v)

end
