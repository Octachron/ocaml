open Outcometree

(** {2 Fuel interface} *)

(* Fuel decay paramater, control the left to right preference of the diff tree *)
let beta = Clflags.error_beta

(* Default fuel available for printing an error *)
let fuel = Clflags.error_fuel

let get_beta () = !beta
let get_fuel () = !fuel

(** {2 Two dimensional size } *)

type size = { primary: int; (* size of elements that must be printed *)
              secondary: int (* size of elements that may be printed if there
                                is some spare space *)
            }

let one = { primary=1; secondary = 0 }
let empty = { primary=0; secondary=0 }
let secondary secondary = { empty with secondary }
let primary primary = { empty with primary }

let (++) x y =
  { primary = x.primary + y.primary; secondary = x.secondary + y.secondary }

let card s = s.primary + s.secondary

(** {2 Diffed type } *)

(** Generator type *)
type ('a,'b) gen = {
  gen: int -> 'a; (** [gen fuel x] generate a representation of the underlying
                      element with size fuel *)
  min_size:'b;
  max_size:'b;
}

(** Main type *)
type 'a diff =
  | Eq of ('a, int) gen
  | D of ('a * 'a , size) gen

type 'a mk_diff = 'a * 'a -> 'a * 'a


let _min_size = function
  | D r -> r.min_size
  | Eq r -> secondary r.min_size

let max_size = function
  | D r -> r.max_size
  | Eq r -> secondary r.max_size

let flatten = function
  | D r -> r
  | Eq r -> { min_size = secondary r.min_size; max_size = secondary r.max_size;
              gen = (fun fuel -> let r = r.gen fuel in r, r) }


(** {3 Combinators } *)
let const x _fuel = x
let pure f = Eq { min_size =0; max_size=0; gen = const f }
let (//) left right=
  D { gen = const (left, right);
      max_size = one;
      min_size = empty;  }

let pure0 f = Eq { min_size =0; max_size=0; gen = const f }
let d0 left right=
  D { gen = const (left, right);
      max_size = empty;
      min_size = empty;  }

let stitch x y = match x, y with
  | Eq x, Eq y ->
      D { min_size = secondary @@ max x.min_size y.min_size;
          max_size = secondary @@ max x.max_size y.max_size;
          gen =(fun fuel -> x.gen fuel, y.gen fuel)
        }
  | _ -> raise (Invalid_argument "Stitching difference")


let ( =~ ) left right =
  D { gen = (fun _ -> (left, right)); min_size = empty; max_size = empty }

let focus_on f = function
  | Eq _ as x -> x
  | D r -> D { r with gen = (fun fuel -> let x, y = r.gen fuel in f x, f y) }

(** {4 Fuel splitting function } *)
let split_fuel mx mi fuel =
  let l = min mx @@ 1 + ( int_of_float @@ float fuel /. ( 1. +. get_beta() ) ) in
  let r = max (fuel - l) (mi) in
    l, r

let split zip x y fuel =
  if fuel > x.max_size + y.max_size then
     zip (x.gen @@  x.max_size) (y.gen @@ y.max_size)
  else if fuel < x.min_size + y.min_size then
     zip (x.gen x.min_size) (y.gen y.min_size)
  else
    let l, r = split_fuel x.max_size y.min_size fuel in
    zip (x.gen l) (y.gen r)

let zip f x sf sx = match f, x with
  | Eq f, Eq x ->  let r = (f.gen sf) (x.gen sx) in r, r
  | Eq f, D r -> let x, y = r.gen sx and f = f.gen sf in
    f x, f y
  | D f, Eq x ->
      let x = x.gen sx and f, g = f.gen sf in
      f x, g x
  | D f, D x ->
      let x, y = x.gen sx and f, g = f.gen sf in
      f x, g y

let _min2 x y =
  { primary = min x.primary y.primary; secondary = min x.secondary y.secondary}

let _all x = { primary= x; secondary = x }

let split_2d zip x y fuel =
  if fuel >= card (x.max_size ++ y.max_size) then
   (* do we have more fuel than needed? *)
   zip (card @@ x.max_size) (card @@ y.max_size)
  else if fuel < card (x.min_size ++ y.min_size) then
     zip (card x.min_size) (card y.min_size)
  else if fuel < (x.max_size ++ y.max_size).primary then
    let l, r = split_fuel x.max_size.primary y.min_size.primary fuel in
    zip l r
  else
    let fuel = fuel - (x.max_size++y.max_size).primary in
    let l, r = split_fuel x.max_size.secondary y.min_size.secondary fuel in
    zip (x.max_size.primary + l) (y.max_size.primary + r)

let (<$>) f x = match f, x with
  | Eq f, Eq x -> Eq { min_size = 1;
                       max_size = f.max_size + x.max_size;
                       gen = split (@@) f x }
  | _ , _ ->
      D { gen = split_2d (zip f x) (flatten f) (flatten x);
          min_size = one ;
          max_size = max_size f ++ max_size x;
        }

(** {3 Applicative combinators } *)
let (<*>) f x = pure f <$> x

let diff0 x y = if x = y then pure0 x else d0 x y

let diff (left_focus,right_focus) left right =
  if left = right then
    Eq { min_size = 1; max_size = 1; gen = const right }
  else
    D { gen = const (left_focus left,right_focus right); max_size = one;
        min_size = one }

let _cons = List.cons
let id x = x
let ellipsis () = !Oprint.ellipsis

let size_bounds = function
  | D r -> r.min_size, r.max_size
  | Eq e -> secondary e.min_size, secondary e.max_size

let cons2 (x,y) (l,l') = x::l, y :: l'
let dup x = x, x

let is_eq = function
  | Eq _ -> true
  | _ -> false

(** {3 List combinators } *)
let list_diff ellipsis l =
  let cons_el (b1,b2) (l1,l2) =
    (if not b1 then ellipsis :: l1 else l1),
    (if not b2 then ellipsis::l2 else l2) in

  let maycons (b1,b2) (x,y) (l1,l2) =
    (if not b1 then x :: l1 else l1),
    (if not b2 then y ::l2 else l2) in

  let (&&&) (a,b) (a',b') = (a && a', b && b') in

  let max_size =
    List.fold_left (fun mx (x,_) -> let _,mx' = size_bounds x in
                     mx ++ mx' ) empty l in

  let rec at_least_one ellip = function
    | [] -> empty
    | (D x, _) :: _  when x.min_size.primary > 0 -> x.min_size
    | _ :: q -> primary (if ellip then 0 else 1) ++ at_least_one true q in

  let _count_ellipsis (x,y) = if x && y then 0 else 1 in

  let rec ellide in_ellipsis sfuel l fuel =
    match l with
    | [] -> [], []
    | _ when fuel < 1 -> cons_el in_ellipsis ([], [])
    | x :: xs ->
        begin match x with

        | Eq e, _ when e.max_size <= sfuel ->
            (* we print equal element only if we have some spare fuel *)
            cons2 (dup @@ e.gen e.max_size) @@ ellide (dup false)
              (sfuel - e.max_size) xs (fuel - e.max_size)
        | Eq _, _ ->
            cons_el in_ellipsis @@ ellide (dup true) sfuel xs fuel

        | D {max_size={primary=0; _ }; _ }, _ ->
        (* We are not printing D.max_size.primary elements  because they are
           potentially distracting since they are equal but not obviously so:
           Typical example: 'a -> int compared to
           <a:int; very:long; object:type'; that:is; not:problematic> -> float
        *)
            cons_el in_ellipsis @@ ellide (dup true) sfuel xs fuel

        | D d, x ->
            (* we check if the current elements contains ellipsis on any side *)
            let status = match x with
              | None -> dup false
              | Some x -> x in

            if d.max_size.secondary <= sfuel then
              (* if we have some spare fuel, we can extend the current elements *)
              maycons (status &&& in_ellipsis) (d.gen @@ card d.max_size)
              @@ ellide status (sfuel - d.max_size.secondary) xs
                (fuel - card d.max_size)
            else
              let consumed = max (min fuel d.max_size.primary) d.min_size.primary in
              maycons (status &&& in_ellipsis) (d.gen consumed)
              @@ ellide status sfuel xs (fuel - consumed)
        end
  in

  let gen fuel =
      ellide (dup false) (max 0 (fuel - max_size.primary)) l fuel in

  if List.for_all (fun (x,_) -> is_eq x) l then
    Eq { min_size = 1; (* a list can always ellipsed to "..." *)
         max_size = card max_size;
         gen = fun fuel -> fst @@ gen fuel }
  else
    D { min_size= at_least_one false l;
        (* we need to be able to expand at least one element *)
        max_size;
        gen }

(* Simple list where the k-th elements should be compared to the k-th element  *)
let list ellipsis diff x y =
  let rec list xs ys = match xs, ys with
    | [], [] -> []
    | x :: xs , y :: ys ->
         (diff x y, None) :: list xs ys
    | x :: xs, ([] as ys) -> ((x // ellipsis), Some (false,true))  :: list xs ys
    | ([] as xs), y :: ys -> ((ellipsis // y), Some(true,false)) :: list xs ys in
  list_diff ellipsis @@ list x y

(* Keyed list where the element with key k should be compared to the element
   associated with the same key *)
let keyed_list cmp ellipsis diff x y =

  let rec mk xs ys = match xs, ys with
    | [], [] -> []
    | x :: xs, ([] as ys) -> ( (diff x ellipsis), Some(false,true)) :: mk xs ys
    | ([] as xs), y :: ys -> ( (diff ellipsis y), Some(true,false)) :: mk xs ys
    | x :: xs' , y :: ys' ->
        let cmp = cmp x y in
        if  cmp < 0 then
          (( diff x ellipsis ), Some(false,true)) :: mk xs' ys
        else if cmp > 0 then
         ( ( diff ellipsis y ), Some(true,false))  :: mk xs ys'
        else
          (diff x y, None) :: mk xs' ys' in
  list_diff ellipsis @@ mk x y

(** {2 Utility functions } *)

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

let nofocus = id, id

let sdiff: string -> _ = diff nofocus
let bdiff: bool -> _ = diff0
let recs_diff: out_rec_status -> _ = diff0
let priv_diff: Asttypes.private_flag -> _ = diff0
let ext_diff: out_ext_status -> _ = diff0

let attr_diff: out_attribute -> _ = diff0

let focus = function
  | Ofoc_simple s -> Ofoc_focused s
  | Ofoc_focused _ as f -> f
  | Ofoc_ellipsis -> Ofoc_ellipsis

let fdiff = diff (dup focus)

let id_diff: out_ident -> _ = diff nofocus (*FIXME*)


let slist x = list (ellipsis ()) sdiff x

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

let _rcmp x y = match x, y with
  | Of_field r, Of_field r' -> compare r.name r'.name
  | Of_ellipsis, _ -> -1
  | _, Of_ellipsis -> 1

(** {2 Outcome tree difference computation functions} *)
module Type = struct
  module M = Misc.StringSet
  let unfree_vars = ref M.empty
  let reset_free () = unfree_vars := M.empty

  let is_free var =
    if M.mem var !unfree_vars then
      false
    else
      (unfree_vars := M.add var !unfree_vars; true)

  let focus = focus_on (function
    | Otyp_ellipsis | Otyp_focus _ as x -> x
    | x -> Otyp_focus x)

  let rec type' t1 t2 =
    match t1, t2 with
    | Otyp_abstract, Otyp_abstract
    | Otyp_open, Otyp_open
    | Otyp_ellipsis, Otyp_ellipsis-> pure t1


    | Otyp_alias (ty,as'), Otyp_alias(ty2,as2) ->
        alias <*> type' ty ty2 <$> sdiff as' as2
    | Otyp_arrow _ , Otyp_arrow _ ->
        let fn =  fn_to_list t1 and fn' = fn_to_list t2 in
        arrow <*> fn_args fn fn'
    | Otyp_class (b, id, args), Otyp_class (b',id',args') ->
        class' <*> bdiff b b' <$> id_diff id id' <$> tylist args args'
    | Otyp_constr (t1, args), Otyp_constr(t2,args2) ->
        focus (constr <*> id_diff t1 t2 <$> tylist args args2)
    | Otyp_manifest(x,y), Otyp_manifest(x',y') ->
        manifest <*> type' x x' <$> type' y y'
    | Otyp_object (l,closed), Otyp_object (l2,closed2)  ->
        object'
        <*> olist l l2
        <$> (opt bdiff closed closed2)
    | Otyp_record r, Otyp_record r' ->
        record <*> rlist r r'
    | Otyp_sum s, Otyp_sum s' ->
        sum <*> list Oc_ellipsis dconstr s s'
    | Otyp_tuple l, Otyp_tuple l' -> tuple <*> tylist l l'
    | Otyp_var(_, name), _ when is_free name -> t1 =~ t2
    | Otyp_var (b,name), Otyp_var(b',name') ->
        focus ( var <*> bdiff b b' <$> sdiff name name')
    | Otyp_variant (b,fields,b2,tags), Otyp_variant(b',fields',b2',tags') ->
        variant
        <*> bdiff b b'
        <$> dvariant fields fields'
        <$> bdiff b2 b2'
        <$> opt flist tags tags'
    | Otyp_poly (forall,ty), Otyp_poly (forall',ty') ->
        poly <*> flist forall forall' <$> type' ty ty'
    | Otyp_module (name, args, tyl), Otyp_module (name',args',tyl') ->
        module' <*> sdiff name name' <$> flist args args' <$> tylist tyl tyl'
    | Otyp_attribute (t,attr), Otyp_attribute (t',attr') ->
        attribute <*> type' t t' <$> attr_diff attr attr'
    | Otyp_focus _, Otyp_focus _
    | Otyp_stuff _, Otyp_stuff _ -> t1 // t2

    | (Otyp_var _ | Otyp_constr _ ), (Otyp_var _ | Otyp_constr _ ) ->
        focus (t1 // t2)

    | _ -> focus @@ stitch (type' t1 t1) (type' t2 t2)

  and tylist x y = list Otyp_ellipsis type' x y
  and flist x y = list Ofoc_ellipsis fdiff x y
  and fn_args (x,ret) (y,ret') =
    pair <*> list Ofa_ellipsis dfn_arg x y <$> type' ret ret'
  and dofield x y =
    focus_on (function Oof_field (_,x,y) -> Oof_field (true,x,y)| x -> x )
    begin match x, y with
    | Oof_ellipsis, Oof_ellipsis -> pure x
    | Oof_field(_, n,ty), Oof_field(_, n',ty') ->
        (fun x y -> Oof_field(false,x,y) )
        <*> sdiff n n'
        <$> type' ty ty'
    | _ -> x // y
    end
  and olist x = keyed_list ocmp Oof_ellipsis dofield x

  and dconstr x y =
    focus_on (function Oc_constr c -> Oc_constr {c with focus = true}| x -> x )
      begin match x, y with
      | Oc_ellipsis, Oc_ellipsis -> pure x
      | Oc_constr c, Oc_constr c' ->
          (fun name args ret -> Oc_constr{focus=false;name;args;ret})
          <*> sdiff c.name c'.name
          <$> tylist c.args c'.args
          <$> opt type' c.ret c'.ret
      | _ -> x // y
      end

  and dfield x y =
    focus_on (function Of_field f -> Of_field {f with focus = true}| x -> x )
    begin match x, y with
    | Of_ellipsis, Of_ellipsis -> pure x
    | Of_field f, Of_field f' ->
            (fun name mut typ -> Of_field {focus=false; name; mut; typ} )
            <*> sdiff f.name f'.name
            <$> bdiff f.mut f'.mut
            <$> type' f.typ f'.typ
    | _ -> x // y
    end
  and rlist x = list Of_ellipsis dfield x

  and dvariant x y = match x, y with
    | Ovar_typ t, Ovar_typ t' -> (fun x -> Ovar_typ x) <*> type' t t'
    | Ovar_fields f, Ovar_fields f' ->
        (fun x -> Ovar_fields x) <*> list Ovf_ellipsis dvfield f f'
    | _ -> x // y

  and dvfield x y =
    focus_on (function Ovf_field f -> Ovf_field { f with focus = true} | x -> x)
      begin match x, y with
      | Ovf_ellipsis, Ovf_ellipsis -> pure x
      | Ovf_field f, Ovf_field f' ->
          (fun label ampersand conj ->
             Ovf_field {focus=false;label;ampersand;conj} )
          <*> sdiff f.label f'.label
          <$> bdiff f.ampersand f'.ampersand
          <$> tylist f.conj f'.conj
      | _ -> x // y
      end

  and dfn_arg x y = match x, y with
    | Ofa_ellipsis, Ofa_ellipsis -> pure x
    | Ofa_arg(label,ty),  Ofa_arg(label',ty')  ->
        (fun lbl ty -> Ofa_arg(lbl,ty))
        <*> fdiff label label'
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
  let csg = focus_on
      (function
        | Ocsg_focus _ | Ocsg_ellipsis as x -> x
        | x -> Ocsg_focus x)


  let rec ct x y = match x, y with
    | Octy_constr (id,tyl), Octy_constr(id',tyl') ->
        constr <*> id_diff id id' <$> tylist tyl tyl'
    | Octy_arrow _ , Octy_arrow _ ->
        arrow <*> ct_args (to_list x) (to_list y)
    | Octy_signature (x,items), Octy_signature(y,items') ->
        signature <*> opt type' x y <$> item_list items items'
    | _ -> x // y
  and item_list x = list Ocsg_ellipsis items x
  and ct_args (x,ctx) (y,cty) =
    pair <*> list Ofa_ellipsis dfn_arg x y <$> ct ctx cty
  and items x y = csg @@
    match x,y with
    | Ocsg_ellipsis, Ocsg_ellipsis -> pure x
    | Ocsg_focus _, Ocsg_focus _ -> x // y

    | Ocsg_constraint Otc_constraint c, Ocsg_constraint Otc_constraint c' ->
        constraint' <*> type' c.lhs c'.lhs <$> type' c.rhs c'.rhs

    | Ocsg_constraint Otc_ellipsis, Ocsg_constraint Otc_ellipsis ->
        pure Ocsg_ellipsis

    | Ocsg_method(name,priv,virt,ty), Ocsg_method(name',priv',virt',ty') ->
        method' <*> sdiff name name'
        <$> bdiff priv priv' <$> bdiff virt virt'
        <$> type' ty ty'
    | Ocsg_value(name,priv,virt,ty), Ocsg_value(name',priv',virt',ty') ->
        value <*> sdiff name name'
        <$> bdiff priv priv' <$> bdiff virt virt'
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

let dparam p p' =
    focus_on (function Otp_param p -> Otp_param { p with focus = true } | x -> x )
      begin match p, p' with
      | Otp_param p, Otp_param p' ->
          (fun co cn name ->
             Otp_param {covariant=co;contravariant=cn;name;focus=false})
          <*> bdiff p.contravariant p'.contravariant
          <$> bdiff p.covariant p'.covariant
          <$> sdiff p.name p'.name
      | _ -> p // p'
      end

let typ = Type.type'
let dct x x' =
  focus_on (function Otc_constraint c -> Otc_constraint { c with focus = true }
                   | x -> x )
    begin
      match x, x' with
      | Otc_ellipsis, Otc_ellipsis -> pure x
      | Otc_constraint c, Otc_constraint c' ->
          (
            (fun lhs rhs -> Otc_constraint{focus=false;lhs;rhs})
            <*> typ c.lhs c'.lhs <$> typ c.rhs c'.rhs
          )
      | _ -> x // x'
    end

let clist = list Otc_ellipsis dct
let plist = list Otp_ellipsis dparam

let alist = list {oattr_name=ellipsis() } attr_diff

let rec sig_item s1 s2 =
  Type.reset_free ();
  let open Sig in
  match s1, s2 with
  | Osig_ellipsis, Osig_ellipsis -> pure s1
  | Osig_focus _, Osig_focus _ -> s1 // s2

  | Osig_class (b,name,params,typ,recs), Osig_class (b',name',params',typ',recs') ->
      class' <*> bdiff b b' <$> sdiff name name' <$> plist params params'
      <$> Ct.ct typ typ' <$> recs_diff recs recs'
  | Osig_class_type (b,name,params,typ,recs),
    Osig_class_type (b',name',params',typ',recs') ->
      class_type <*> bdiff b b' <$> sdiff name name' <$> plist params params'
      <$> Ct.ct typ typ' <$> recs_diff recs recs'
  | Osig_typext (te,st), Osig_typext (te',st') ->
      typext <*>
      ( extension_constructor
        <*> sdiff     te.oext_name        te'.oext_name
        <$> sdiff     te.oext_type_name   te'.oext_type_name
        <$> slist     te.oext_type_params te'.oext_type_params
        <$> tylist    te.oext_args        te'.oext_args
        <$> opt typ   te.oext_ret_type    te'.oext_ret_type
        <$> priv_diff te.oext_private     te'.oext_private
      )
      <$> ext_diff st st'
  | Osig_modtype (name,typ), Osig_modtype (name',typ') ->
      modtype <*> sdiff name name' <$> module_type typ typ'

  | Osig_module (name,typ,recs), Osig_module (name',typ',recs') ->
      module'
      <*> sdiff       name name'
      <$> module_type typ  typ'
      <$> recs_diff   recs recs'

  | Osig_type (decl, recs),  Osig_type (decl', recs') ->
      type' <*>
      ( type_decl
        <*> sdiff     decl.otype_name      decl'.otype_name
        <$> plist     decl.otype_params    decl'.otype_params
        <$> typ       decl.otype_type      decl'.otype_type
        <$> priv_diff decl.otype_private   decl'.otype_private
        <$> bdiff     decl.otype_immediate decl'.otype_immediate
        <$> bdiff     decl.otype_unboxed   decl'.otype_unboxed
        <$> clist     decl.otype_cstrs     decl'.otype_cstrs
      )
      <$> recs_diff recs recs'
  | Osig_value v, Osig_value v' ->
      value <*>
      ( val_decl
        <*> sdiff v.oval_name        v'.oval_name
        <$> typ   v.oval_type        v'.oval_type
        <$> slist v.oval_prims       v'.oval_prims
        <$> alist v.oval_attributes  v'.oval_attributes
      )
  | _ -> s1 // s2

and module_type x y =
  let open Mty in
  match x, y with
  | Omty_abstract, Omty_abstract -> pure x

  | Omty_functor(name,arg,res), Omty_functor(name',arg',res')->
      functor' <*> sdiff name name' <$> opt module_type arg arg'
      <$> module_type res res' (* TODO: expand *)
  | Omty_ident id, Omty_ident id' -> ident <*> id_diff id id'
  | Omty_signature s, Omty_signature s' ->
      signature <*> list Osig_ellipsis sig_item s s'
  | Omty_alias x, Omty_alias y -> alias <*> id_diff x y

  | _ -> x // y

(** {2 Exported functions} *)

let simplify f (x,y)=
  Type.reset_free ();
  match f x y with
  | Eq x -> dup (x.gen @@ get_fuel () )
  | D r -> r.gen @@ get_fuel ()

let typ = simplify type'
let sig_item = simplify sig_item
let class_type = simplify Ct.ct
let modtype = simplify module_type
