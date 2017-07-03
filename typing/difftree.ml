open Outcometree

(** {2 Fuel interface} *)

(* Default fuel available for printing an error *)
let fuel = Clflags.error_size

let get_fuel () = !fuel

(** {2 Two dimensional size } *)

type size = { primary: int; (* size of elements that must be printed *)
              secondary: int (* size of elements that may be printed if there
                                is some spare space *)
            }
(*
let debug fmt = Format.eprintf ("debug:" ^^ fmt ^^ "@.")

  let pp_list pp ppf l =
    Format.fprintf ppf "@[<hov 2>[%a]@]"
    (Format.pp_print_list
      ~pp_sep:(fun ppf () -> Format.fprintf ppf ";@ ")
      pp) l

  let pp_size ppf m = Format.fprintf ppf "{p:%d; s:%d}" m.primary m.secondary
  let pp_int ppf = Format.pp_print_int ppf
  let pp_bound ppf (x,y) = Format.fprintf ppf "min:%a, max:%a" pp_size x pp_size y
*)
(*

      debug "Distributing [%d] fuel" fuel;
      debug "bounds: %a" (pp_list pp_bound) bounds;
      debug "distribution: %a" (pp_list pp_int) distribution;
    debug "List, fuel:%d, len:%d size:{%d;%d}" fuel (List.length l)
   max_size.primary max_size.secondary;
*)

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
  size:'b;
}

(** Main type *)
type 'a diff =
  | Eq of ('a, int) gen
  | D of ('a * 'a , size) gen


let is_eq = function
  | Eq _ -> true
  | _ -> false


let size = function
  | D r -> r.size
  | Eq r -> secondary r.size

let flatten = function
  | D r -> r
  | Eq r -> { size = secondary r.size;
              gen = (fun fuel -> let r = r.gen fuel in r, r) }


(** {3 Combinators } *)
let const x _fuel = x
let pure f = Eq { size=0; gen = const f }
let (//) left right=
  D { gen = const (left, right);
      size = one }

let pure0 f = Eq { size =0; gen = const f }
let d0 left right=
  D { gen = const (left, right);
      size = empty }

let stitch2 x y = match x, y with
  | Eq x, Eq y ->
      D { size = primary (max x.size y.size);
          gen =(fun fuel -> x.gen fuel, y.gen fuel)
        }
  | r, r' ->
      let r = flatten r and r' = flatten r' in
      D {
        size = r.size ++ r'.size;
        gen = (fun fuel -> let x, _ = r.gen fuel and _, y = r'.gen fuel in
                  x, y)
        }
      (*raise (Invalid_argument "Stitching difference")*)

let stitch f x y = stitch2 (f x x) (f y y)

let ( =~ ) left right =
  D { gen = (fun _ -> (left, right)); size = secondary 1 }

let id x = x

let foc x = Ofoc(On,x)
let unfoc x = Ofoc(Off,x)
let _refoc = function
  | Ofoc(Off,x) -> Ofoc(On,x)
  | x -> x

let fmap f g = function
  | Eq r -> Eq { r with gen = (fun fuel -> f (r.gen fuel) ) }
  | D r -> D { r with gen = (fun fuel -> let x, y = r.gen fuel in g x, g y) }


let focus_diff x = fmap unfoc foc x
let nofoc x = fmap unfoc unfoc x

(** {3 Hlist } *)

module Std = struct
  type nonrec 'a list = 'a list = [] | (::) of 'a * 'a list
end

module AppList = struct

  module T = struct

    type ('a,'res) t =
      | [] : ('res,'res) t
      | (::) : 'a diff * ('any,'res) t -> ('a -> 'any, 'res) t

  end

  type 'a concrete = Single of 'a | Pair of 'a * 'a

  let concrete_pure f = Single f

  let map2_2 f x = match f, x with
    | Single f, Single x -> Single(f x)
    | Pair (f,g), Single x -> Pair(f x,g x)
    | Single f, Pair(x,y) -> Pair(f x, f y)
    | Pair (f,g), Pair(x,y) -> Pair(f x, g y)

  let global_size l =
    List.fold_left (++) empty l

  let rec sizes: type a b. (a,b) T.t -> size list =
    function
    | T.[] -> []
    | T.( a :: q ) -> size a :: sizes q

  let beta = 0.75
  let z proj l =
    let _, z, l =
      List.fold_left (fun (m, z, cumulative ) x ->
          beta *. m, z +. m *. float(proj x), z :: cumulative )
        (1.,0., []) l in
    z, l


  let _exp_split proj fuel l =
    let z, zl = z proj l in
    let split (m,l,zl,fuel) x =
      let x = float @@ proj x in
      let proposition =
        ceil @@ fuel *. m *. x /. z in
      let capped = min x proposition in
      let fuel = ceil @@ (fuel -. proposition)
                         +. z/.List.hd zl *. ( proposition -. capped )
      in
      beta *. m, int_of_float capped :: l, List.tl zl, fuel in
    let _, l, _, _ =
      List.fold_left split (1.,[],zl,float fuel) l in
    List.rev l


  let _first_served proj fuel l =
    let split (l,fuel) x =
      let x = min fuel (proj x) in
      let fuel = fuel - x in
      x :: l, fuel in
    let l , _ =
      List.fold_left split ([], fuel) l in
    List.rev l

  module Level_map = Map.Make(struct type t = int
      let compare (x:int) (y:int) = compare x y  end)

  let rising_tide proj fuel l =
    let levels =
      List.fold_left (fun lvls s ->
          let lvl = proj s in
          if lvl <> 0 then
            let c = try Level_map.find lvl lvls with Not_found -> 0 in
            Level_map.add lvl (1 + c) lvls
          else
            lvls)
        Level_map.empty l in
    let max_level, fuel_left = Level_map.fold(fun k x (lvl,fuel) ->
        if k * x <= fuel then (k, fuel - k * x)
        else (lvl,fuel)
      ) levels (0, fuel) in
     let rec finish fuel = function
      | [] -> []
      | x :: q when proj x > max_level ->
          let consumed = min (proj x) fuel in
          consumed :: finish (fuel-consumed) q
      | x :: q -> (proj x) :: finish fuel q in
    finish fuel_left l


  let distribute gmax sizes fuel =
    if fuel >= card gmax then
      List.map (fun size -> card size) sizes
    else if fuel <= gmax.primary then
      rising_tide (fun y -> y.primary) fuel sizes
    else
      let fuel' = fuel - gmax.primary in
      let d = rising_tide (fun y -> y.secondary) fuel' sizes in
      List.map2 (fun x y -> x.primary + y) sizes d

  let rec apply : type a res. a concrete -> (a,res) T.t -> int list
    -> res concrete =
    fun f l fs -> match l, fs with
      | T.[], [] -> f
      | T.( a :: q ), fuel :: fs ->
          let f' = map2_2 f
          (match a with
            | D r -> let x, y = r.gen fuel in Pair(x,y)
            | Eq r -> Single (r.gen fuel)
          ) in
          apply f' q fs
      | _ -> raise (Invalid_argument(
          "Difftree.H.apply: mismatched fuel and list lenght"
        ))

  let rec is_all_eq: type a b. (a,b) T.t -> bool  = function
    | T.(a :: q) -> if is_eq a then is_all_eq q else false
    | T.[] -> true

  let eq = function
    | Single x -> x
    | Pair _ -> raise (Invalid_argument ("Unexpected pair output"))

  let pair = function
    | Single x -> x, x
    | Pair (x, y) -> x, y

  let mkdiff f l =
    let sizes = sizes l in
    let global = global_size sizes in

    let gen proj fuel =
      let distribution = distribute global sizes fuel in
      proj (apply f l distribution) in
    if is_all_eq l then
      Eq { size = card global; gen = gen eq }
    else
      D { size=global ; gen = gen pair }

  let (<*>) f = mkdiff (concrete_pure f)

end

(** {3 Applicative combinators } *)
(*module AppPair = struct
  let (<$>) f x = match f, x with
    | Eq f, Eq x -> Eq { min_size = 1;
                         max_size = f.max_size + x.max_size;
                         gen = split (@@) f x }
    | _ , _ ->
        D { gen = split_2d (zip f x) (flatten f) (flatten x);
            min_size = one ;
            max_size = max_size f ++ max_size x;
          }

  let (<*>) f x = pure f <$> x
  end*)
open AppList


(** {3 Simple combinators }*)
let diff0 x y = if x = y then pure0 x else d0 x y

let diff size (left_focus,right_focus) left right =
  if left = right then
    Eq { size = card size; gen = const right }
  else
    D { gen = const (left_focus left,right_focus right); size }

let _cons = List.cons

let cons2 (x,y) (l,l') = x::l, y :: l'
let dup x = x, x


(** {3 List combinators } *)
let ellipsis = Ofoc_ellipsis
let list_diff l =
  let cons_el (b1,b2) (l1,l2) =
    (if b1 then ellipsis :: l1 else l1),
    (if b2 then ellipsis::l2 else l2) in

  let maycons (b1,b2) (x,y) (l1,l2) =
    (if not b1 then x :: l1 else l1),
    (if not b2 then y ::l2 else l2) in

  let (|||) (a,b) (a',b') = (a || a', b || b') in
  let (&&&) (a,b) (a',b') = (a && a', b && b') in
  let not2 (a,b) = (not a, not b) in

  let sizes = List.map (fun (x,_) -> size x) l in
  let global = AppList.global_size sizes in

  let _count_ellipsis (x,y) = if x && y then 0 else 1 in

  let distribute fuel = AppList.distribute global sizes fuel in

  let with_fuel fuel = List.map2 (fun x y -> x, y) l (distribute fuel) in

  let expand x = match x with
    | None -> dup false
    | Some x -> x in

  let fueled ((x,_),f) = f > 0 || card (size x) = 0 in

  let rec full = function
    | [] -> dup []
    | ((diff,st),f) :: xs ->
        maycons (expand st) ( (flatten diff).gen f ) @@ full xs in

  let is_full = List.for_all fueled in

  let rec ellide not_first status in_ellipsis = function
    | [] -> cons_el (not_first &&& status) @@ dup []
    | ((_,st) ,  _ as x) :: q when not (fueled x) ->
        ellide (not_first ||| not2 (expand st) ) (dup true) (dup true) q
    | (x, f) :: xs ->
        begin match x with
        | Eq e, st ->
            (* we print equal element only if we have some spare fuel *)
            cons_el (in_ellipsis &&& not_first ) @@
            cons2 (dup @@ e.gen f)
            @@ ellide (expand st) (dup false) (dup false) xs
 (*       | D {max_size={primary=0; _ }; _ }, _ ->
        (* We are not printing D.max_size.primary elements  because they are
           potentially distracting since they are equal but not obviously so:
           Typical example: 'a -> int compared to
           <a:int; very:long; object:type'; that:is; not:problematic> -> float
        *)
            ellide (dup true) (dup true) xs
 *)
        | D d, st ->
            let st = expand st in
            (* we check if the current elements contains ellipsis on any side *)
            cons_el (in_ellipsis &&& not2 st &&& not_first)
            @@ maycons st (d.gen f)
            @@ ellide (not_first ||| not2 st) (status &&& st) st xs
        end
  in

  let gen fuel =
    let l = with_fuel fuel in
    if is_full l then
      full l
    else
      ellide (dup false) (dup true) (dup false) l in

  if List.for_all (fun (x,_) -> is_eq x) l then
    Eq { size = card global; gen = fun fuel -> fst @@ gen fuel }
  else
    D {  size = global; gen }

(* Simple list where the k-th elements should be compared to the k-th element  *)
let list diff x y =
  let rec list xs ys = match xs, ys with
    | [], [] -> []
    | x :: xs , y :: ys ->
         (diff x y, None) :: list xs ys
    | x :: xs, ([] as ys) -> ((diff x ellipsis), Some (false,true))  :: list xs ys
    | ([] as xs), y :: ys -> ((diff ellipsis y), Some(true,false)) :: list xs ys in
  list_diff @@ list x y

(* Keyed list where the element with key k should be compared to the element
   associated with the same key *)

let lift_cmp cmp x y = match x,y with
  | Ofoc_ellipsis, _ -> -1
  | _, Ofoc_ellipsis  -> 1
  | Ofoc(_,x), Ofoc(_,y) -> cmp x y

let keyed_list cmp diff x y =
  let cmp x = lift_cmp cmp x in

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
  list_diff @@ mk x y

(** {2 Utility functions } *)


let pair x y = x, y
let _triple x y z = x, y, z
open T
let some x = Some x
let opt diff x y =
  match x, y with
  | None, None -> pure None
  | Some x, Some y -> some <*> [diff x y]
  | _ -> x // y


let nofocus = id, id

let sfocus = function
  | Ofoc(_, s) -> Ofoc(On,s)
  | Ofoc_ellipsis -> Ofoc_ellipsis

let _sdiff: string -> _ = diff one nofocus

let bdiff: bool -> _ = diff0
let bfdiff: bool focusable -> _ = diff empty (dup sfocus)

let recs_diff: out_rec_status focusable -> _ = diff empty (dup sfocus)
let recs_diff_0: out_rec_status -> _ = diff0


let priv_diff: Asttypes.private_flag focusable -> _ = diff empty (dup sfocus)
let ext_diff: out_ext_status -> _ = diff0

let attr_diff: out_attribute -> _ = diff0

let fdiff: string focusable -> _ = diff one (dup sfocus)
let _fdiff0: string focusable -> _ = diff empty (dup sfocus)



(** {2 Outcome tree difference computation functions} *)
module Ident = struct
  let apply x y = Oide_apply (x,y)
  let dot x y = Oide_dot (x,y)
  let base_ident x = Oide_ident x

  let rec diff x y = match x, y with
    | Oide_apply(x,y), Oide_apply(x',y') -> apply <*> [diff x x'; diff y y']
    | Oide_dot(x,y), Oide_dot(x',y') -> dot <*> [diff x x'; fdiff y y']
    | Oide_ident s, Oide_ident s' -> base_ident <*> [fdiff s s']
    | x, y -> x // y
end
let id_diff: out_ident -> _ = Ident.diff


let ocmp (n,_) (n',_) = compare n n'

let _rcmp r r' = compare r.name r'.name


let fmap2 f x y = match x, y with
  | Ofoc(_,x), Ofoc(_,y) ->  nofoc @@ f x y
  | Ofoc_ellipsis, Ofoc(_,x) -> stitch2 (pure Ofoc_ellipsis) (focus_diff @@ f x x)
  | Ofoc(_,x), Ofoc_ellipsis -> stitch2 (focus_diff @@ f x x) (pure Ofoc_ellipsis)
  | Ofoc_ellipsis, Ofoc_ellipsis -> pure Ofoc_ellipsis


let bind2 f x y = match x, y with
  | Ofoc(_,x), Ofoc(_,y) ->  f x y
  | Ofoc_ellipsis, Ofoc(_,x) -> stitch2 (pure Ofoc_ellipsis) (f x x)
  | Ofoc(_,x), Ofoc_ellipsis -> stitch2 (f x x) (pure Ofoc_ellipsis)
  | Ofoc_ellipsis, Ofoc_ellipsis -> pure Ofoc_ellipsis


module Type = struct

  let constr x y = unfoc @@ Otyp_constr(x,y)
  let manifest x y = unfoc @@ Otyp_manifest(x,y)
  let object' x y  = unfoc @@ Otyp_object(x,y)
  let sum x = unfoc @@ Otyp_sum x
  let class' x y z =unfoc @@ Otyp_class(x,y,z)
  let record x = unfoc @@ Otyp_record x
  let var x y = unfoc @@ Otyp_var(x,y)
  let variant x y z w = unfoc @@ Otyp_variant(x,y,z,w)
  let poly x y = unfoc @@ Otyp_poly(x,y)
  let module' x y z = unfoc @@ Otyp_module(x,y,z)
  let attribute x y = unfoc @@ Otyp_attribute(x,y)
  let tuple l =  unfoc @@ Otyp_tuple l
  let alias x y = unfoc @@ Otyp_alias(x,y)

  let rec fn_to_list =
    let open Std in
    function
    | Otyp_arrow (arg,Ofoc(_,rest)) ->
        let rest, ret = fn_to_list rest in
        arg :: rest, ret
    | Otyp_arrow (arg, (Ofoc_ellipsis as ret)) ->
        [arg], ret
    | rest -> [], unfoc rest

  let rec list_to_fn =
    let open Std in
    function
    | [], ret -> ret
    | arg :: q, ret -> unfoc @@ Otyp_arrow (arg, list_to_fn (q,ret))

  let arrow l = list_to_fn l

  module M = Misc.StringSet
  let unfree_vars = ref M.empty
  let reset_free () = unfree_vars := M.empty

  let is_free =
    function
    | Ofoc_ellipsis -> false
    | Ofoc(_,var) ->
    if M.mem var !unfree_vars then
      false
    else
      (unfree_vars := M.add var !unfree_vars; true)

  let pure f = nofoc @@ pure f
  let rec type' t1 t2 =
    match t1, t2 with
    | Otyp_abstract, Otyp_abstract
    | Otyp_open, Otyp_open -> pure t1

    | Otyp_alias (ty,as'), Otyp_alias(ty2,as2) ->
       alias <*> [bind2 type' ty ty2; fdiff as' as2]
    | Otyp_arrow _ , Otyp_arrow _ ->
        let fn =  fn_to_list t1 and fn' = fn_to_list t2 in
        arrow <*> [ fn_args fn fn' ]
    | Otyp_class (b, id, args), Otyp_class (b',id',args') ->
        class' <*> [ bdiff b b'; fmap2 id_diff id id'; tylist args args' ]
    | Otyp_constr (t1, args), Otyp_constr(t2,args2) ->
        constr <*> [ fmap2 id_diff t1 t2; tylist args args2 ]
    | Otyp_manifest(x,y), Otyp_manifest(x',y') ->
        manifest <*> [bind2 type' x x'; bind2 type' y y']
    | Otyp_object (l,closed), Otyp_object (l2,closed2)  ->
        object' <*> [olist l l2; opt bdiff closed closed2]
    | Otyp_record r, Otyp_record r' ->
        record <*> [rlist r r']
    | Otyp_sum s, Otyp_sum s' ->
        sum <*> [list (fmap2 dconstr) s s']
    | Otyp_tuple l, Otyp_tuple l' -> tuple <*> [tylist l l']

    | Otyp_var (b,name), Otyp_var(b',name') ->
        var <*> [bfdiff b b'; fdiff name name']
    | Otyp_var(_, name), _ when is_free name -> nofoc (t1 =~ t2)
    | Otyp_variant (b,fields,b2,tags), Otyp_variant(b',fields',b2',tags') ->
        variant <*> [
          bfdiff b b';
          dvariant fields fields';
          bfdiff b2 b2';
          opt flist tags tags'
        ]
    | Otyp_poly (forall,ty), Otyp_poly (forall',ty') ->
        poly <*> [flist forall forall'; bind2 type' ty ty']
    | Otyp_module (name, args, tyl), Otyp_module (name',args',tyl') ->
        module' <*> [fdiff name name'; flist args args'; tylist tyl tyl']
    | Otyp_attribute (t,attr), Otyp_attribute (t',attr') ->
        attribute <*> [bind2 type' t t'; fmap2 attr_diff attr attr']
    | Otyp_stuff _, Otyp_stuff _ -> nofoc (t1 // t2)

    | (Otyp_var _ | Otyp_constr _ ), (Otyp_var _ | Otyp_constr _ ) ->
        focus_diff (t1 // t2)

    | _ -> stitch type' t1 t2

  and tylist x y = list (bind2 type') x y
  and flist x y = list fdiff x y
  and fn_args (x,ret) (y,ret') =
    pair <*> [ list (bind2 dfn_arg) x y; bind2 type' ret ret' ]
  and dofield (n,ty) (n',ty') =
        pair <*> [ fdiff n n'; bind2 type' ty ty']
  and olist x = keyed_list ocmp (fmap2 dofield) x

  and dconstr c c' =
    (fun cname args ret -> {cname;args;ret})
          <*> [ fdiff c.cname c'.cname;
                tylist c.args c'.args;
                opt (bind2 type') c.ret c'.ret
              ]

  and dfield f f' =
    (fun label mut typ -> {label; mut; typ} )
    <*> [fdiff f.label f'.label
        ; bfdiff f.mut f'.mut
        ; bind2 type' f.typ f'.typ ]
  and rlist x = list (fmap2 dfield) x

  and dvariant x y = match x, y with
    | Ovar_typ t, Ovar_typ t' -> (fun x -> Ovar_typ x) <*> [bind2 type' t t']
    | Ovar_fields f, Ovar_fields f' ->
        (fun x -> Ovar_fields x) <*> [list (fmap2 dvfield) f f']
    | _ -> x // y

  and dvfield f f' =
          (fun tag ampersand conj -> {tag;ampersand;conj} )
          <*> [fdiff f.tag f'.tag;
               bfdiff f.ampersand f'.ampersand;
               tylist f.conj f'.conj]

  and dfn_arg (label,ty) (label',ty') =
        nofoc (pair <*> [fdiff label label'; bind2 type' ty ty' ])
end open Type

module Ct = struct
  let constr x y = unfoc @@ Octy_constr (x,y)

  let rec to_list = function
    | Octy_arrow (arg,Ofoc(_,z)) -> let q, e = to_list z in
        Std.(arg :: q), e
    | Octy_arrow (arg,Ofoc_ellipsis) ->
       Std.[arg], Ofoc_ellipsis
    | rest -> Std.[], unfoc rest

  let rec arrow = function
    | Std.[], ret -> ret
    | Std.(arg :: q), ret -> unfoc @@ Octy_arrow(arg, arrow (q,ret) )

  let signature x y = unfoc @@ Octy_signature (x,y)

  let constraint' x y = unfoc @@ Ocsg_constraint {lhs=x;rhs=y}
  let method' x y z w = unfoc @@ Ocsg_method(x,y,z,w)
  let value x y z w = unfoc @@ Ocsg_value(x,y,z,w)

  let rec ct x y = match x, y with
    | Octy_constr (id,tyl), Octy_constr(id',tyl') ->
        constr <*> [fmap2 id_diff id id'; tylist tyl tyl']
    | Octy_arrow _ , Octy_arrow _ ->
        arrow <*> [ ct_args x y ]
    | Octy_signature (x,items), Octy_signature(y,items') ->
        signature <*> [opt (bind2 type') x y; item_list items items']
    | _ -> stitch ct x y
  and item_list x = list (bind2 items) x
  and ct_args c c' =
    let (x,ctx), (y,cty) = to_list c, to_list c' in
    pair <*> [list (bind2 dfn_arg) x y; bind2 ct ctx cty]
  and items x y = match x,y with

    | Ocsg_constraint c, Ocsg_constraint c' ->
        constraint' <*> [bind2 type' c.lhs c'.lhs; bind2 type' c.rhs c'.rhs]

    | Ocsg_method(name,priv,virt,ty), Ocsg_method(name',priv',virt',ty') ->
        method' <*> [ fdiff name name';
                      bfdiff priv priv'; bfdiff virt virt';
                      bind2 type' ty ty' ]
    | Ocsg_value(name,priv,virt,ty), Ocsg_value(name',priv',virt',ty') ->
        value <*> [fdiff name name';
                   bfdiff priv priv'; bfdiff virt virt';
                   bind2 type' ty ty']
    | _ -> stitch items x y

end


module Sig = struct
  let class' a b c d e = unfoc @@ Osig_class(a,b,c,d,e)
  let class_type a b c d e = unfoc @@ Osig_class_type(a,b,c,d,e)
  let typext x y = unfoc @@ Osig_typext(x,y)
  let modtype x y = unfoc @@ Osig_modtype(x,y)
  let module' x y z = unfoc @@ Osig_module(x,y,z)
  let type' x y = unfoc @@ Osig_type(x,y)
  let value x = unfoc @@ Osig_value x
end

module Mty = struct
  let functor' x y z = unfoc @@ Omty_functor(x,y,z)
  let ident x = unfoc @@ Omty_ident x
  let signature x = unfoc @@ Omty_signature x
  let alias x = unfoc @@ Omty_alias x
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
  (fun co cn name -> {covariant=co;contravariant=cn;name})
  <*> [ bfdiff p.covariant p'.covariant;
        bfdiff p.contravariant p'.contravariant;
        fdiff  p.name p'.name ]

let typ = bind2 Type.type'
let dct c c' =
  (fun lhs rhs -> {lhs;rhs}) <*> [typ c.lhs c'.lhs; typ c.rhs c'.rhs]

let clist = list (fmap2 dct)
let plist = list (fmap2 dparam)

let alist = list (fmap2 attr_diff)

let foc_flatten = function
  | Ofoc(_,x) -> x
  | Ofoc_ellipsis -> "..."

let sig_item_key = function
  | Osig_class(_,name,_,_,_) -> "class", foc_flatten name
  | Osig_class_type(_,name,_,_,_) -> "class_type", foc_flatten name
  | Osig_typext (te,_) -> "type_ext", foc_flatten te.oext_name
  | Osig_modtype (name,_) -> "modtype", foc_flatten name
  | Osig_module(name,_,_) -> "module", foc_flatten name
  | Osig_type(name,_) -> "type", foc_flatten name.otype_name
  | Osig_value v -> "val", foc_flatten v.oval_name

let sigcmp x y = compare (sig_item_key x) (sig_item_key y)

let rec sig_item s1 s2 =
  Type.reset_free ();
  let open Sig in
  match s1, s2 with

  | Osig_class (b,name,params,typ,recs), Osig_class (b',name',params',typ',recs') ->
      class' <*> [bfdiff b b'; fdiff name name'; plist params params';
                  bind2 Ct.ct typ typ'; recs_diff recs recs']
  | Osig_class_type (b,name,params,typ,recs),
    Osig_class_type (b',name',params',typ',recs') ->
      class_type <*> [bfdiff b b'; fdiff name name'; plist params params';
                      bind2 Ct.ct typ typ'; recs_diff recs recs']
  | Osig_typext (te,st), Osig_typext (te',st') ->
      typext <*>
     [ extension_constructor
        <*> [ fdiff     te.oext_name        te'.oext_name;
              fdiff     te.oext_type_name   te'.oext_type_name;
              flist     te.oext_type_params te'.oext_type_params;
              tylist    te.oext_args        te'.oext_args;
              opt typ   te.oext_ret_type    te'.oext_ret_type;
              priv_diff te.oext_private     te'.oext_private ]
        ;
        fmap2 ext_diff st st'
      ]
  | Osig_modtype (name,typ), Osig_modtype (name',typ') ->
      modtype <*> [fdiff name name'; bind2 module_type typ typ']

  | Osig_module (name,typ,recs), Osig_module (name',typ',recs') ->
      module' <*> [ fdiff       name name';
                    bind2 module_type typ  typ';
                    fmap2 recs_diff_0 recs recs'
                  ]
  | Osig_type (decl, recs),  Osig_type (decl', recs') ->
      type' <*>
      [ type_decl <*> [
            fdiff     decl.otype_name      decl'.otype_name;
            plist     decl.otype_params    decl'.otype_params;
            typ       decl.otype_type      decl'.otype_type;
            priv_diff decl.otype_private   decl'.otype_private;
            bfdiff    decl.otype_immediate decl'.otype_immediate;
            bfdiff    decl.otype_unboxed   decl'.otype_unboxed;
            clist     decl.otype_cstrs     decl'.otype_cstrs;
          ];
        recs_diff recs recs']
  | Osig_value v, Osig_value v' ->
      value <*>
      [ val_decl
        <*> [ fdiff v.oval_name        v'.oval_name;
              typ   v.oval_type        v'.oval_type;
              flist v.oval_prims       v'.oval_prims;
              alist v.oval_attributes  v'.oval_attributes
            ]
      ]
  | _ -> stitch sig_item s1 s2

and module_type x y =
  let open Mty in
  match x, y with
  | Omty_abstract, Omty_abstract -> pure x

  | Omty_functor(name,arg,res), Omty_functor(name',arg',res')->
      functor' <*> [fdiff name name'; opt (bind2 module_type) arg arg';
                    bind2 module_type res res' ] (* TODO: expand *)
  | Omty_ident id, Omty_ident id' -> ident <*> [fmap2 id_diff id id']
  | Omty_signature s, Omty_signature s' ->
      let _sort = List.sort (lift_cmp sigcmp) in
      signature <*> [ keyed_list sigcmp (bind2 sig_item) s s' ]
  | Omty_alias x, Omty_alias y -> alias <*> [fmap2 id_diff x y]

  | _ -> stitch module_type x y

(** {2 Exported functions} *)

module Gen = struct

  type 'a t = 'a * 'a -> int -> 'a * 'a

  let extract = function
    | Ofoc_ellipsis -> assert false
    | Ofoc(_,x) -> x

  let simplify f (x,y)=
  Type.reset_free ();
  match f x y with
  | Eq x -> fun fuel -> dup ( extract @@ x.gen fuel )
  | D r -> fun fuel -> let x, y = r.gen fuel in extract x, extract y

  let typ = simplify type'
  let sig_item = simplify sig_item
  let class_type = simplify Ct.ct
  let modtype = simplify module_type

end

type 'a t = 'a * 'a -> 'a * 'a
let simplify f x = f x @@ get_fuel ()

let typ = simplify Gen.typ
let sig_item = simplify Gen.sig_item
let class_type = simplify Gen.class_type
let modtype = simplify Gen.modtype
