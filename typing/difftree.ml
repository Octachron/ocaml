open Outcometree

type content = { primary: int; secondary: int }
let _empty = { primary=0; secondary=0 }
let one = { primary=1; secondary = 0 }

let secondary secondary = {secondary; primary = 0 }

let (++) x y =
  { primary = x.primary + y.primary; secondary = x.secondary + y.secondary }

type 'a diff =
  | Eq of { cardinal: int; shared: 'a }
  | D of { left: 'a; right:'a; content:content }

type 'a mk_diff = 'a * 'a -> 'a * 'a

let pure f = Eq { cardinal = 1; shared = f }
let (//) left right= D { left; right; content = one }

let right = function
  | Eq e -> e.shared
  | D r -> r.right


let left = function
  | Eq e -> e.shared
  | D r -> r.left

let (<$>) f x = match f, x with
  | Eq f, Eq x -> Eq { cardinal = f.cardinal + x.cardinal;
                       shared = f.shared x.shared }
  | Eq f, D r ->
      D { left = f.shared r.left;
          right = f.shared r.right;
          content = r.content ++ secondary f.cardinal
        }
  | D f, Eq x ->
      D {
        left = f.left x.shared;
        right = f.right x.shared;
        content= f.content ++ secondary x.cardinal
      }
  | D f, D x ->
      D { left = f.left x.left;
          right = f.right x.right;
          content = f.content ++ x.content
        }

let (<*>) f x = pure f <$> x

let diff0 x y = if x = y then pure x else x // y
let diff left right =
  if left = right then
    Eq { cardinal = 1; shared = right }
  else
    D { left; right; content = one }

let cons = List.cons
let id x = x
let ellipsis () = !Oprint.ellipsis


let list ellipsis diff x y =
  let rec list in_ellipsis xs ys = match xs, ys with
    | [], [] -> pure []
    | x :: xs , y :: ys ->
        begin match diff x y with
        | Eq _ when in_ellipsis -> list true xs ys
        | Eq _ -> cons ellipsis <*> list true xs ys
        | D _ as d -> cons <*> d <$> list false xs ys
        end
    | x :: xs, ([] as ys) -> (cons x // id) <$> list false xs ys
    | ([] as xs), y :: ys -> (id // cons y) <$> list false xs ys
  in
  list false x y

let keyed_list cmp ellipsis d x y =
  let may_cons b x l = if not b then x :: l else l in
  let rec list in_ellipsis xs ys = match xs, ys with
    | [], [] -> pure []
    | x :: xs' , y :: ys' ->
        let cmp = cmp x y in
        if  cmp < 0 then
          (cons x // id ) <$> list (diff0 false @@ right in_ellipsis) xs' ys
        else if cmp > 0 then
          (id // cons y ) <$> list (diff0 (left in_ellipsis) false) xs ys'
        else
          let xs = xs' and ys = ys' in
          begin match d x y with
          | Eq _ -> may_cons <*> in_ellipsis <$> pure ellipsis <$>
                    list (pure true) xs ys
          | D _ as d -> cons <*> d <$> list (pure false) xs ys
          end
    | x :: xs, ([] as ys) ->
        (cons x // id) <$> list (diff0 false @@ right in_ellipsis) xs ys
    | ([] as xs), y :: ys ->
        (id // cons y) <$> list (diff0 (left in_ellipsis) false) xs ys
  in
  list (pure false) x y


let rec fn_to_list = function
  | Otyp_arrow (arg,rest) ->
      let rest, ret = fn_to_list rest in
      arg :: rest, ret
  | rest -> [], rest

let rec list_to_fn = function
  | [], ret -> ret
  | arg :: q, ret -> Otyp_arrow (arg,list_to_fn (q,ret))

let pair x y = x, y
let triple x y z = x, y, z
let _pair' d1 d2 (x,y) (x',y') =
  pair <*> (d1 x x') <$> (d2 y y')
let _triple' d1 d2 d3 (x,y,z) (x',y',z') =
  triple <*> (d1 x x') <$> (d2 y y') <$> (d3 z z')

let some x = Some x
let opt diff x y =
  match x, y with
  | None, None -> pure None
  | Some x, Some y -> some <*> diff x y
  | _ -> x // y

let tuple l = Otyp_tuple l
let alias x y = Otyp_alias(x,y)

let arrow = list_to_fn
let slist x = list (ellipsis ()) diff x

let constr x y = Otyp_constr(x,y)
let manifest x y = Otyp_manifest(x,y)
let object' x y = Otyp_object(x,y)
let sum x = Otyp_sum x
let class' x y z = Otyp_class(x,y,z)
let record x = Otyp_record x
let var x y = Otyp_var(x,y)
let variant x y z w = Otyp_variant(x,y,z,w)
let poly x y = Otyp_poly(x,y)
let module' x y z = Otyp_module(x,y,z)
let attribute x y = Otyp_attribute(x,y)

let ocmp x y = match x, y with
  | Oof_field (_,n,_) , Oof_field (_,n',_)  -> compare n n'
  | Oof_ellipsis, _ -> -1
  | _, Oof_ellipsis -> 1

let rcmp x y = match x, y with
  | Of_field r, Of_field r' -> compare r.name r'.name
  | Of_ellipsis, _ -> -1
  | _, Of_ellipsis -> 1


module Type = struct
  let rec type' t1 t2 =
    match t1, t2 with
    | Otyp_abstract, Otyp_abstract
    | Otyp_open, Otyp_open
    | Otyp_ellipsis, Otyp_ellipsis-> pure t1


    | Otyp_alias (ty,as'), Otyp_alias(ty2,as2) ->
        alias <*> type' ty ty2 <$> diff as' as2
    | Otyp_arrow _ , Otyp_arrow _ ->
        let fn =  fn_to_list t1 and fn' = fn_to_list t2 in
        arrow <*> fn_args fn fn'
    | Otyp_class (b, id, args), Otyp_class (b',id',args') ->
        class' <*> diff b b' <$> diff id id' <$> tylist args args'
    | Otyp_constr (t1, args), Otyp_constr(t2,args2) ->
        constr <*> diff t1 t2 <$> tylist args args2
    | Otyp_manifest(x,y), Otyp_manifest(x',y') ->
        manifest <*> type' x x' <$> type' y y'
    | Otyp_object (l,closed), Otyp_object (l2,closed2)  ->
        object'
        <*> olist l l2
        <$> (diff closed closed2)
    | Otyp_record r, Otyp_record r' ->
        record <*> rlist r r'
    | Otyp_sum s, Otyp_sum s' ->
        sum <*> list Oc_ellipsis dconstr s s'
    | Otyp_tuple l, Otyp_tuple l' -> tuple <*> tylist l l'
    | Otyp_var (b,name), Otyp_var(b',name') ->
        var <*> diff b b' <$> diff name name'
    | Otyp_variant (b,fields,b2,tags), Otyp_variant(b',fields',b2',tags') ->
        variant
        <*> diff b b'
        <$> dvariant fields fields'
        <$> diff b2 b2'
        <$> opt flist tags tags'
    | Otyp_poly (forall,ty), Otyp_poly (forall',ty') ->
        poly <*> flist forall forall' <$> type' ty ty'
    | Otyp_module (name, args, tyl), Otyp_module (name',args',tyl') ->
        module' <*> diff name name' <$> flist args args' <$> tylist tyl tyl'
    | Otyp_attribute (t,attr), Otyp_attribute (t',attr') ->
        attribute <*> type' t t' <$> diff attr attr'
    | Otyp_focus _, Otyp_focus _
    | Otyp_stuff _, Otyp_stuff _ -> t1 // t2
    | _ -> t1 // t2

  and tylist x y = list Otyp_ellipsis type' x y
  and flist x y = list Ofoc_ellipsis diff x y
  and fn_args (x,ret) (y,ret') =
    pair <*> list Ofa_ellipsis dfn_arg x y <$> type' ret ret'
  and dofield x y = match x, y with
    | Oof_ellipsis, Oof_ellipsis -> pure x
    | Oof_field(_, n,ty), Oof_field(_, n',ty') ->
        (fun x y -> Oof_field(false,x,y) )
        <*> diff n n'
        <$> type' ty ty'
    | _ -> x // y
  and olist x = keyed_list ocmp Oof_ellipsis dofield x

  and dconstr x y = match x, y with
    | Oc_ellipsis, Oc_ellipsis -> pure x
    | Oc_constr c, Oc_constr c' ->
        (fun name args ret -> Oc_constr{focus=false;name;args;ret})
        <*> diff c.name c'.name
        <$> tylist c.args c'.args
        <$> opt type' c.ret c'.ret
    | _ -> x // y

  and dfield x y = match x, y with
    | Of_ellipsis, Of_ellipsis -> pure x
    | Of_field f, Of_field f' ->
        (fun name mut typ -> Of_field {focus=false; name; mut; typ} )
        <*> diff f.name f'.name
        <$> diff f.mut f'.mut
        <$> type' f.typ f'.typ
    | _ -> x // y

  and rlist x = keyed_list rcmp Of_ellipsis dfield x

  and dvariant x y = match x, y with
    | Ovar_typ t, Ovar_typ t' -> (fun x -> Ovar_typ x) <*> type' t t'
    | Ovar_fields f, Ovar_fields f' ->
        (fun x -> Ovar_fields x) <*> list Ovf_ellipsis dvfield f f'
    | _ -> x // y

  and dvfield x y = match x, y with
    | Ovf_ellipsis, Ovf_ellipsis -> pure x
    | Ovf_field f, Ovf_field f' ->
        (fun label ampersand conj -> Ovf_field {focus=false;label;ampersand;conj} )
        <*> diff f.label f'.label
        <$> diff f.ampersand f'.ampersand
        <$> tylist f.conj f'.conj
    | _ -> x // y

  and dfn_arg x y = match x, y with
    | Ofa_ellipsis, Ofa_ellipsis -> pure x
    | Ofa_arg(label,ty),  Ofa_arg(label',ty')  ->
        (fun lbl ty -> Ofa_arg(lbl,ty))
        <*> diff label label'
        <$> type' ty ty'
    | _ -> x // y
end open Type

module Ct = struct
  let constr x y = Octy_constr (x,y)

  let rec to_list = function
    | Octy_arrow (arg,z) -> let q, e = to_list z in
        arg :: q, e
    | rest -> [], rest

  let rec arrow = function
    | [], ret -> ret
    | arg :: q, ret -> Octy_arrow(arg, arrow (q,ret) )

  let signature x y = Octy_signature (x,y)

  let constraint' x y = Ocsg_constraint(Otc_constraint{focus=false;lhs=x;rhs=y})
  let method' x y z w = Ocsg_method(x,y,z,w)
  let value x y z w = Ocsg_value(x,y,z,w)

  let rec ct x y = match x, y with
    | Octy_constr (id,tyl), Octy_constr(id',tyl') ->
        constr <*> diff id id' <$> tylist tyl tyl'
    | Octy_arrow _ , Octy_arrow _ ->
        arrow <*> ct_args (to_list x) (to_list y)
    | Octy_signature (x,items), Octy_signature(y,items') ->
        signature <*> opt type' x y <$> item_list items items'
    | _ -> x // y
  and item_list x = list Ocsg_ellipsis items x
  and ct_args (x,ctx) (y,cty) =
    pair <*> list Ofa_ellipsis dfn_arg x y <$> ct ctx cty
  and items x y = match x,y with
    | Ocsg_ellipsis, Ocsg_ellipsis -> pure x
    | Ocsg_focus _, Ocsg_focus _ -> x // y

    | Ocsg_constraint Otc_constraint c, Ocsg_constraint Otc_constraint c' ->
        constraint' <*> type' c.lhs c'.lhs <$> type' c.rhs c'.rhs

    | Ocsg_constraint Otc_ellipsis, Ocsg_constraint Otc_ellipsis ->
        pure Ocsg_ellipsis

    | Ocsg_method(name,priv,virt,ty), Ocsg_method(name',priv',virt',ty') ->
        method' <*> diff name name'
        <$> diff priv priv' <$> diff virt virt'
        <$> type' ty ty'
    | Ocsg_value(name,priv,virt,ty), Ocsg_value(name',priv',virt',ty') ->
        value <*> diff name name'
        <$> diff priv priv' <$> diff virt virt'
        <$> type' ty ty'
    | _ -> x//y

end


module Sig = struct
  let class' a b c d e = Osig_class(a,b,c,d,e)
  let class_type a b c d e = Osig_class_type(a,b,c,d,e)
  let typext x y = Osig_typext(x,y)
  let modtype x y = Osig_modtype(x,y)
  let module' x y z = Osig_module(x,y,z)
  let type' x y = Osig_type(x,y)
  let value x = Osig_value x
end

module Mty = struct
  let functor' x y z = Omty_functor(x,y,z)
  let ident x = Omty_ident x
  let signature x = Omty_signature x
  let alias x = Omty_alias x
end

let type_decl otype_name otype_params otype_type otype_private otype_immediate
    otype_unboxed otype_cstrs =
  {otype_name; otype_params; otype_type; otype_private; otype_immediate;
   otype_unboxed; otype_cstrs}

let extension_constructor oext_name oext_type_name oext_type_params oext_args
    oext_ret_type oext_private =
  {oext_name; oext_type_name; oext_type_params; oext_args; oext_ret_type;
   oext_private }

let _type_extension  otyext_name otyext_params otyext_constructors otyext_private =
  {otyext_name; otyext_params; otyext_constructors; otyext_private}

let val_decl oval_name oval_type oval_prims oval_attributes =
  {oval_name;oval_type;oval_prims;oval_attributes}

let dparam p p' = match p, p' with
  | Otp_param p, Otp_param p' ->
      (fun co cn name -> Otp_param {covariant=co;contravariant=cn;name;focus=false})
      <*> diff p.contravariant p'.contravariant
      <$> diff p.covariant p'.covariant
      <$> diff p.name p'.name
  | _ -> p // p'

let typ = Type.type'
let dct x x' = match x, x' with
  | Otc_ellipsis, Otc_ellipsis -> pure x
  | Otc_constraint c, Otc_constraint c' ->
      (fun lhs rhs -> Otc_constraint{focus=false;lhs;rhs})
      <*> typ c.lhs c'.lhs <$> typ c.rhs c'.rhs
  | _ -> x // x'

let clist = list Otc_ellipsis dct
let plist = list Otp_ellipsis dparam

let alist = list {oattr_name=ellipsis() } diff

let rec sig_item s1 s2 =
  let open Sig in
  match s1, s2 with
  | Osig_ellipsis, Osig_ellipsis -> pure s1
  | Osig_focus _, Osig_focus _ -> s1 // s2

  | Osig_class (b,name,params,typ,recs), Osig_class (b',name',params',typ',recs') ->
      class' <*> diff b b' <$> diff name name' <$> plist params params'
      <$> Ct.ct typ typ' <$> diff recs recs'
  | Osig_class_type (b,name,params,typ,recs),
    Osig_class_type (b',name',params',typ',recs') ->
      class_type <*> diff b b' <$> diff name name' <$> plist params params'
      <$> Ct.ct typ typ' <$> diff recs recs'
  | Osig_typext (te,st), Osig_typext (te',st') ->
      typext <*>
      ( extension_constructor
        <*> diff    te.oext_name        te'.oext_name
        <$> diff    te.oext_type_name   te'.oext_type_name
        <$> slist   te.oext_type_params te'.oext_type_params
        <$> tylist  te.oext_args        te'.oext_args
        <$> opt typ te.oext_ret_type    te'.oext_ret_type
        <$> diff    te.oext_private     te'.oext_private
      )
      <$> diff st st'
  | Osig_modtype (name,typ), Osig_modtype (name',typ') ->
      modtype <*> diff name name' <$> module_type typ typ'

  | Osig_module (name,typ,recs), Osig_module (name',typ',recs') ->
      module'
      <*> diff        name name'
      <$> module_type typ  typ'
      <$> diff        recs recs'

  | Osig_type (decl, recs),  Osig_type (decl', recs') ->
      type' <*>
      ( type_decl
        <*> diff  decl.otype_name      decl'.otype_name
        <$> plist decl.otype_params    decl'.otype_params
        <$> typ   decl.otype_type      decl'.otype_type
        <$> diff  decl.otype_private   decl'.otype_private
        <$> diff  decl.otype_immediate decl'.otype_immediate
        <$> diff  decl.otype_unboxed   decl'.otype_unboxed
        <$> clist decl.otype_cstrs     decl'.otype_cstrs
      )
      <$> diff recs recs'
  | Osig_value v, Osig_value v' ->
      value <*>
      ( val_decl
        <*> diff v.oval_name         v'.oval_name
        <$> typ  v.oval_type         v'.oval_type
        <$> slist v.oval_prims       v'.oval_prims
        <$> alist v.oval_attributes  v'.oval_attributes
      )
  | _ -> s1 // s2

and module_type x y =
  let open Mty in
  match x, y with
  | Omty_abstract, Omty_abstract -> pure x

  | Omty_functor(name,arg,res), Omty_functor(name',arg',res')->
      functor' <*> diff name name' <$> opt module_type arg arg'
      <$> module_type res res' (* TODO: expand *)
  | Omty_ident id, Omty_ident id' -> ident <*> diff id id'
  | Omty_signature s, Omty_signature s' ->
      signature <*> list Osig_ellipsis sig_item s s'
  | Omty_alias x, Omty_alias y -> alias <*> diff x y

  | _ -> x // y


let simplify f (x,y)= match f x y with
  | Eq x -> x.shared, x.shared
  | D r -> r.left, r.right

let typ = simplify type'
let sig_item = simplify sig_item
let class_type = simplify Ct.ct
let modtype = simplify module_type
