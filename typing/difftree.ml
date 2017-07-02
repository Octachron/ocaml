open Outcometree

(*
let debug fmt = Format.eprintf ("debug:" ^^ fmt ^^ "@.")

      debug "Distributing [%d] fuel" fuel;
      debug "bounds: %a" (pp_list pp_bound) bounds;
      debug "distribution: %a" (pp_list pp_int) distribution;


  let pp_list pp ppf l =
    Format.fprintf ppf "@[<hov 2>[%a]@]"
    (Format.pp_print_list
      ~pp_sep:(fun ppf () -> Format.fprintf ppf ";@ ")
      pp) l

  let pp_size ppf m = Format.fprintf ppf "{p:%d; s:%d}" m.primary m.secondary
  let pp_int ppf = Format.pp_print_int ppf
  let pp_bound ppf (x,y) = Format.fprintf ppf "min:%a, max:%a" pp_size x pp_size y

    debug "List, fuel:%d, len:%d size:{%d;%d}" fuel (List.length l)
   max_size.primary max_size.secondary;


*)

(** {2 Fuel interface} *)

(* Default fuel available for printing an error *)
let fuel = Clflags.error_size

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


let is_eq = function
  | Eq _ -> true
  | _ -> false


let _min_size = function
  | D r -> r.min_size
  | Eq r -> secondary r.min_size

let _max_size = function
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

let stitch f x y = match (f x x), (f y y) with
  | Eq x, Eq y ->
      D { min_size = one;
          max_size = primary 1 ++ secondary (max x.max_size y.max_size);
          gen =(fun fuel -> x.gen fuel, y.gen fuel)
        }
  | r, r' ->
      let r = flatten r and r' = flatten r' in
      D { min_size = one;
          max_size = primary 1 ++ r.max_size ++ r'.max_size;
          gen = (fun fuel -> let x, _ = r.gen fuel and _, y = r'.gen fuel in
                  x, y)
        }
      (*raise (Invalid_argument "Stitching difference")*)


let ( =~ ) left right =
  D { gen = (fun _ -> (left, right)); min_size = empty; max_size = secondary 1 }

let focus_on f = function
  | Eq _ as x -> x
  | D r -> D { r with gen = (fun fuel -> let x, y = r.gen fuel in f x, f y) }

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

  let size_bounds = function
    | D r -> r.min_size, r.max_size
    | Eq e -> secondary e.min_size, secondary e.max_size

  let global_bounds l =
    List.fold_left (fun (mi,mx) (mi',mx') -> mi ++ mi', mx ++ mx' )
      (empty,empty) l

  let rec bounds: type a b. (a,b) T.t -> (size * size) list =
    function
    | T.[] -> []
    | T.( a :: q ) -> size_bounds a :: bounds q

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


  let distribute (gmin,gmax) bounds fuel =
    if fuel >= card gmax then
      List.map (fun (_,max) -> card max) bounds
    else if fuel <= gmin.primary then
      List.map(fun (min,_) -> card min) bounds
    else if fuel <= gmax.primary then
      let fuel' = fuel - gmin.primary in
      let bounds' = List.map
          ( fun (x,y) -> (x, { y with primary = max 0 (y.primary - x.primary) }))
          bounds in
      let d = rising_tide (fun (_,y) -> y.primary) fuel' bounds' in
      List.map2 (fun (x,_) y -> x.primary + y) bounds d
    else
      let fuel' = fuel - gmax.primary in
      let d = rising_tide (fun (_,y) -> y.secondary) fuel' bounds in
      List.map2 (fun (_,x) y -> x.primary + y) bounds d

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
    let bounds = bounds l in
    let _min_size, max_size as global = global_bounds bounds in
    let gen proj fuel =
      let distribution = distribute global bounds fuel in
      proj (apply f l distribution) in
    if is_all_eq l then
      Eq { min_size = 0; max_size = card max_size; gen = gen eq }
    else
      D {min_size = empty; max_size; gen = gen pair }

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

let diff (left_focus,right_focus) left right =
  if left = right then
    Eq { min_size = 0; max_size = 1; gen = const right }
  else
    D { gen = const (left_focus left,right_focus right); max_size = one;
        min_size = empty }

let _cons = List.cons
let id x = x

let cons2 (x,y) (l,l') = x::l, y :: l'
let dup x = x, x


(** {3 List combinators } *)
let list_diff ellipsis l =
  let cons_el (b1,b2) (l1,l2) =
    (if b1 then ellipsis :: l1 else l1),
    (if b2 then ellipsis::l2 else l2) in

  let maycons (b1,b2) (x,y) (l1,l2) =
    (if not b1 then x :: l1 else l1),
    (if not b2 then y ::l2 else l2) in

  let (|||) (a,b) (a',b') = (a || a', b || b') in
  let (&&&) (a,b) (a',b') = (a && a', b && b') in
  let not2 (a,b) = (not a, not b) in

  let bounds = List.map (fun (x,_) -> AppList.size_bounds x) l in
  let global = AppList.global_bounds bounds in

  let rec at_least_one ellip = function
    | [] -> empty
    | (D x, _) :: _  when x.min_size.primary > 0 -> x.min_size
    | _ :: q -> primary (if ellip then 0 else 1) ++ at_least_one true q in

  let _count_ellipsis (x,y) = if x && y then 0 else 1 in

  let distribute fuel = AppList.distribute global bounds fuel in

  let with_fuel fuel = List.map2 (fun x y -> x, y) l (distribute fuel) in

  let expand x = match x with
    | None -> dup false
    | Some x -> x in

  let rec full = function
    | [] -> dup []
    | ((diff,st),f) :: xs ->
        maycons (expand st) ( (flatten diff).gen f ) @@ full xs in

  let is_full = List.for_all (fun (_,f) -> f > 0 ) in

  let rec ellide not_first status in_ellipsis = function
    | [] -> cons_el (not_first &&& status) @@ dup []
    | ((_,st) , 0) :: q ->
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
    Eq { min_size = 1; (* a list can always ellipsed to "..." *)
         max_size = card (snd global);
         gen = fun fuel -> fst @@ gen fuel }
  else
    D { min_size= at_least_one false l;
        (* we need to be able to expand at least one element *)
        max_size = snd global;
        gen }

(* Simple list where the k-th elements should be compared to the k-th element  *)
let list ellipsis diff x y =
  let rec list xs ys = match xs, ys with
    | [], [] -> []
    | x :: xs , y :: ys ->
         (diff x y, None) :: list xs ys
    | x :: xs, ([] as ys) -> ((diff x ellipsis), Some (false,true))  :: list xs ys
    | ([] as xs), y :: ys -> ((diff ellipsis y), Some(true,false)) :: list xs ys in
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
let _triple x y z = x, y, z
open T
let some x = Some x
let opt diff x y =
  match x, y with
  | None, None -> pure None
  | Some x, Some y -> some <*> [diff x y]
  | _ -> x // y

let tuple l = Otyp_tuple l
let alias x y = Otyp_alias(x,y)

let arrow = list_to_fn

let nofocus = id, id

let sfocus = function
  | Ofoc_unfocused s -> Ofoc_focused s
  | Ofoc_focused _ as f -> f
  | Ofoc_ellipsis -> Ofoc_ellipsis

let sdiff: string -> _ = diff nofocus

let bdiff: bool -> _ = diff0
let bfdiff: bool focusable -> _ = diff (dup sfocus)

let recs_diff: out_rec_status focusable -> _ = diff (dup sfocus)
let recs_diff_0: out_rec_status -> _ = diff0


let priv_diff: Asttypes.private_flag focusable -> _ = diff (dup sfocus)
let ext_diff: out_ext_status -> _ = diff0

let attr_diff: out_attribute -> _ = diff0

let fdiff = diff (dup sfocus)

let id_diff: out_ident -> _ = diff nofocus (*FIXME*)


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
  | Oof_field (n,_) , Oof_field (n',_)  -> compare n n'
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

  let is_free =
    function
    | Ofoc_ellipsis -> false
    | Ofoc_focused var | Ofoc_unfocused var ->
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
        alias <*> [type' ty ty2; sdiff as' as2]
    | Otyp_arrow _ , Otyp_arrow _ ->
        let fn =  fn_to_list t1 and fn' = fn_to_list t2 in
        arrow <*> [ fn_args fn fn' ]
    | Otyp_class (b, id, args), Otyp_class (b',id',args') ->
        class' <*> [ bdiff b b'; id_diff id id'; tylist args args' ]
    | Otyp_constr (t1, args), Otyp_constr(t2,args2) ->
        focus (constr <*> [ id_diff t1 t2; tylist args args2 ])
    | Otyp_manifest(x,y), Otyp_manifest(x',y') ->
        manifest <*> [type' x x'; type' y y']
    | Otyp_object (l,closed), Otyp_object (l2,closed2)  ->
        object' <*> [olist l l2; opt bdiff closed closed2]
    | Otyp_record r, Otyp_record r' ->
        record <*> [rlist r r']
    | Otyp_sum s, Otyp_sum s' ->
        sum <*> [list Oc_ellipsis dconstr s s']
    | Otyp_tuple l, Otyp_tuple l' -> tuple <*> [tylist l l']

    | Otyp_var (b,name), Otyp_var(b',name') ->
        focus ( var <*> [bfdiff b b'; fdiff name name'])
    | Otyp_var(_, name), _ when is_free name -> t1 =~ t2
    | Otyp_variant (b,fields,b2,tags), Otyp_variant(b',fields',b2',tags') ->
        variant <*> [
          bfdiff b b';
          dvariant fields fields';
          bfdiff b2 b2';
          opt flist tags tags'
        ]
    | Otyp_poly (forall,ty), Otyp_poly (forall',ty') ->
        poly <*> [flist forall forall'; type' ty ty']
    | Otyp_module (name, args, tyl), Otyp_module (name',args',tyl') ->
        module' <*> [fdiff name name'; flist args args'; tylist tyl tyl']
    | Otyp_attribute (t,attr), Otyp_attribute (t',attr') ->
        attribute <*> [type' t t'; attr_diff attr attr']
    | Otyp_focus _, Otyp_focus _
    | Otyp_stuff _, Otyp_stuff _ -> t1 // t2

    | (Otyp_var _ | Otyp_constr _ ), (Otyp_var _ | Otyp_constr _ ) ->
        focus (t1 // t2)

    | _ -> focus @@ stitch type' t1 t2

  and tylist x y = list Otyp_ellipsis type' x y
  and flist x y = list Ofoc_ellipsis fdiff x y
  and fn_args (x,ret) (y,ret') =
    pair <*> [ list Ofa_ellipsis dfn_arg x y; type' ret ret' ]
  and of_focus = function
      | Oof_ellipsis -> Oof_ellipsis
      | Oof_field(n,ty) -> Oof_field(sfocus n,  Otyp_focus ty)
  and dofield x y =
    begin match x, y with
    | Oof_ellipsis, Oof_ellipsis -> pure x
    | Oof_field(n,ty), Oof_field(n',ty') ->
        (fun x y -> Oof_field(x,y) )
        <*> [ fdiff n n'; type' ty ty']
    | _ -> focus_on of_focus @@
        stitch dofield x y
    end
  and olist x = keyed_list ocmp Oof_ellipsis dofield x

  and dconstr x y =
      begin match x, y with
      | Oc_ellipsis, Oc_ellipsis -> pure x
      | Oc_constr c, Oc_constr c' ->
          (fun name args ret -> Oc_constr{name;args;ret})
          <*> [ fdiff c.name c'.name;
                tylist c.args c'.args;
                opt type' c.ret c'.ret
              ]
      | _ -> x // y
      end

  and dfield x y =
    begin match x, y with
    | Of_ellipsis, Of_ellipsis -> pure x
    | Of_field f, Of_field f' ->
            (fun name mut typ -> Of_field {name; mut; typ} )
            <*> [fdiff f.name f'.name
                ; bfdiff f.mut f'.mut
                ; type' f.typ f'.typ ]
    | _ -> x // y
    end
  and rlist x = list Of_ellipsis dfield x

  and dvariant x y = match x, y with
    | Ovar_typ t, Ovar_typ t' -> (fun x -> Ovar_typ x) <*> [type' t t']
    | Ovar_fields f, Ovar_fields f' ->
        (fun x -> Ovar_fields x) <*> [list Ovf_ellipsis dvfield f f']
    | _ -> x // y

  and dvfield x y =
      begin match x, y with
      | Ovf_ellipsis, Ovf_ellipsis -> pure x
      | Ovf_field f, Ovf_field f' ->
          (fun label ampersand conj -> Ovf_field {label;ampersand;conj} )
          <*> [fdiff f.label f'.label;
               bfdiff f.ampersand f'.ampersand;
               tylist f.conj f'.conj]
      | _ -> x // y
      end

  and dfn_arg x y = match x, y with
    | Ofa_ellipsis, Ofa_ellipsis -> pure x
    | Ofa_arg(label,ty),  Ofa_arg(label',ty')  ->
        (fun lbl ty -> Ofa_arg(lbl,ty))
        <*> [fdiff label label'; type' ty ty' ]
    | _ -> x // y
end open Type

module Ct = struct
  let constr x y = Octy_constr (x,y)

  let rec to_list = function
    | Octy_arrow (arg,z) -> let q, e = to_list z in
        Std.(arg :: q), e
    | rest -> Std.[], rest

  let rec arrow = function
    | Std.[], ret -> ret
    | Std.(arg :: q), ret -> Octy_arrow(arg, arrow (q,ret) )

  let signature x y = Octy_signature (x,y)

  let constraint' x y = Ocsg_constraint(Otc_constraint{lhs=x;rhs=y})
  let method' x y z w = Ocsg_method(x,y,z,w)
  let value x y z w = Ocsg_value(x,y,z,w)
  let csg = focus_on
      (function
        | Ocsg_focus _ | Ocsg_ellipsis as x -> x
        | x -> Ocsg_focus x)


  let rec ct x y = match x, y with
    | Octy_constr (id,tyl), Octy_constr(id',tyl') ->
        constr <*> [id_diff id id'; tylist tyl tyl']
    | Octy_arrow _ , Octy_arrow _ ->
        arrow <*> [ ct_args (to_list x) (to_list y) ]
    | Octy_signature (x,items), Octy_signature(y,items') ->
        signature <*> [opt type' x y; item_list items items']
    | _ -> x // y
  and item_list x = list Ocsg_ellipsis items x
  and ct_args (x,ctx) (y,cty) =
    pair <*> [list Ofa_ellipsis dfn_arg x y; ct ctx cty]
  and items x y = csg @@
    match x,y with
    | Ocsg_ellipsis, Ocsg_ellipsis -> pure x
    | Ocsg_focus _, Ocsg_focus _ -> x // y

    | Ocsg_constraint Otc_constraint c, Ocsg_constraint Otc_constraint c' ->
        constraint' <*> [type' c.lhs c'.lhs; type' c.rhs c'.rhs]

    | Ocsg_constraint Otc_ellipsis, Ocsg_constraint Otc_ellipsis ->
        pure Ocsg_ellipsis

    | Ocsg_method(name,priv,virt,ty), Ocsg_method(name',priv',virt',ty') ->
        method' <*> [ fdiff name name';
                      bfdiff priv priv'; bfdiff virt virt';
                      type' ty ty' ]
    | Ocsg_value(name,priv,virt,ty), Ocsg_value(name',priv',virt',ty') ->
        value <*> [fdiff name name';
                   bfdiff priv priv'; bfdiff virt virt';
                   type' ty ty']
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
      begin match p, p' with
      | Otp_param p, Otp_param p' ->
          (fun co cn name -> Otp_param {covariant=co;contravariant=cn;name})
          <*> [ bfdiff p.covariant p'.covariant;
                bfdiff p.contravariant p'.contravariant;
                fdiff  p.name p'.name ]
      | _ -> p // p'
      end

let typ = Type.type'
let dct x x' =
    begin
      match x, x' with
      | Otc_ellipsis, Otc_ellipsis -> pure x
      | Otc_constraint c, Otc_constraint c' ->
          (
            (fun lhs rhs -> Otc_constraint{lhs;rhs})
            <*> [typ c.lhs c'.lhs; typ c.rhs c'.rhs]
          )
      | _ -> x // x'
    end

let clist = list Otc_ellipsis dct
let plist = list Otp_ellipsis dparam

let alist = list {oattr_name= Ofoc_ellipsis } attr_diff

let foc_flatten = function
  | Ofoc_focused x -> x
  | Ofoc_unfocused x -> x
  | Ofoc_ellipsis -> "..."

let rec sig_item_key = function
  | Osig_ellipsis -> "ellipsis", "..."
  | Osig_focus x -> sig_item_key x
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
  | Osig_ellipsis, Osig_ellipsis -> pure s1
  | Osig_focus _, Osig_focus _ -> s1 // s2

  | Osig_class (b,name,params,typ,recs), Osig_class (b',name',params',typ',recs') ->
      class' <*> [bfdiff b b'; fdiff name name'; plist params params';
                  Ct.ct typ typ'; recs_diff recs recs']
  | Osig_class_type (b,name,params,typ,recs),
    Osig_class_type (b',name',params',typ',recs') ->
      class_type <*> [bfdiff b b'; fdiff name name'; plist params params';
                      Ct.ct typ typ'; recs_diff recs recs']
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
        ext_diff st st'
      ]
  | Osig_modtype (name,typ), Osig_modtype (name',typ') ->
      modtype <*> [fdiff name name'; module_type typ typ']

  | Osig_module (name,typ,recs), Osig_module (name',typ',recs') ->
      module' <*> [ fdiff       name name';
                    module_type typ  typ';
                    recs_diff_0 recs recs'
                  ]
  | Osig_type (decl, recs),  Osig_type (decl', recs') ->
      type' <*>
      [ type_decl <*> [
            fdiff     decl.otype_name      decl'.otype_name;
            plist     decl.otype_params    decl'.otype_params;
            typ       decl.otype_type      decl'.otype_type;
            priv_diff decl.otype_private   decl'.otype_private;
            bdiff     decl.otype_immediate decl'.otype_immediate;
            bdiff     decl.otype_unboxed   decl'.otype_unboxed;
            clist     decl.otype_cstrs     decl'.otype_cstrs;
          ];
        recs_diff_0 recs recs']
  | Osig_value v, Osig_value v' ->
      value <*>
      [ val_decl
        <*> [ fdiff v.oval_name        v'.oval_name;
              typ   v.oval_type        v'.oval_type;
              flist v.oval_prims       v'.oval_prims;
              alist v.oval_attributes  v'.oval_attributes
            ]
      ]
  | _ ->
      focus_on (fun x -> Osig_focus x) (stitch sig_item s1 s2)

and module_type x y =
  let open Mty in
  match x, y with
  | Omty_abstract, Omty_abstract -> pure x

  | Omty_functor(name,arg,res), Omty_functor(name',arg',res')->
      functor' <*> [fdiff name name'; opt module_type arg arg';
                    module_type res res' ] (* TODO: expand *)
  | Omty_ident id, Omty_ident id' -> ident <*> [id_diff id id']
  | Omty_signature s, Omty_signature s' ->
      signature <*> [keyed_list sigcmp Osig_ellipsis sig_item
                       (List.sort sigcmp s)
                       (List.sort sigcmp s')
                    ]
  | Omty_alias x, Omty_alias y -> alias <*> [id_diff x y]

  | _ -> x // y

(** {2 Exported functions} *)

module Gen = struct

  type 'a t = 'a * 'a -> int -> 'a * 'a

  let simplify f (x,y)=
  Type.reset_free ();
  match f x y with
  | Eq x -> fun fuel -> dup (x.gen fuel )
  | D r -> fun fuel -> r.gen fuel

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
