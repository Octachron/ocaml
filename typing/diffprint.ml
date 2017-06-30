open Outcometree

type printer =
  | P of (Format.formatter -> unit)
  | Dots

type ellision = None | Before | After | Both
let fusion_ellision x y = match x, y with
  | Both, _ | _, Both | After, Before | Before, After -> Both
  | Before, Before -> Before
  | After, After -> After
  | None, x | x, None -> x
let (++) = fusion_ellision


type side = Expected | Real
type 'a status =
  | Eq of printer
  | Diff of ellision * printer * printer


let fp = Format.fprintf

let const fmt ppf = Format.fprintf ppf fmt

let to_printer = function
  | P p -> p
  | Dots -> const "..."

let connect connector x y =
  match x, y with
  | Dots, Dots -> Dots
  | x, y ->
      P (fun ppf -> fp ppf "%t%t%t" (to_printer x) connector (to_printer y))

let ellide printer = Dots (*P(fun ppf -> fp ppf "@{<ellide>%t@}" printer)*)

let eq printer = Eq(ellide printer)
let expected printer = P(fun ppf -> fp ppf "@{<expected>%t@}" printer)
let real printer = P(fun ppf -> fp ppf "@{<real>%t@}" printer)
let mkdiff status elt x y = Diff(status, expected (elt x), real (elt y))

let lift (<*>) x y = match x, y with
    | Eq x, Eq y -> Eq (x <*> y)
    | Eq x, Diff(el, y1,y2) ->
      Diff(Before ++ el, x <*> y1, x <*> y2)
    | Diff(el, x,x'), Diff(el2, y,y') -> Diff(el ++ el2, x <*> y, x' <*> y')
    | Diff(el,x,x'), Eq y ->
        Diff(el ++ After, x <*> y, x' <*> y)


let connector middle = lift @@ connect @@ const middle
let (-->) = connector "@ ->@ "
let (-:) = connector ""

let semi = connector ";@ "
let product = connector "@ *@ "

let diff (=) pp x y =
  if x = y then
    eq (pp x)
  else
    mkdiff None pp x y

let left x =
  Diff(None, expected x, Dots)

let right x =
  Diff(None, Dots, real x)

let string s ppf = fp ppf "%s" s
let label = diff (=) @@ fun s ppf ->
  if s = "" then () else fp ppf "%s:" s

let typ0 t1 = (fun ppf -> !(Oprint.out_type) ppf t1)

let rec list_elt sep equ elt x y =
  let d = diff equ elt in
  match x, y with
  | [], [] -> assert false
  | [x], [y] -> d x y
  | x :: xs, y :: ys -> sep (d x y) (list_elt sep equ elt xs ys)
  | x :: xs, [] -> sep (left x) (list_elt sep equ elt xs [])
  | [], y :: ys -> sep (right y) (list_elt sep equ elt [] ys)


let rec list sep single diff x y =
  match x, y with
  | [], [] -> assert false
  | [x], [y] -> diff x y
  | x :: xs, y :: ys -> sep (diff x y) (list sep single diff xs ys)
  | x :: xs, [] -> sep (left @@ single x) (list sep single diff xs [])
  | [], y :: ys -> sep (right @@ single y) (list sep single diff [] ys)

let fmap f = function
  | Dots -> Dots
  | P p -> P(f p)

let fmap' f = function
  | Eq x -> Eq (fmap f x)
  | Diff(s,x,y) -> Diff(s,fmap f x,fmap f y)

let paren = fmap' (fun pr ppf -> fp ppf "(%t)" pr)
let bracket = fmap' (fun pr ppf -> fp ppf "[%t]" pr)

let ($=) = connector "@ =@ "

let (as') = connector "@ as@ "

let ident i1 i2 = match i1, i2 with
  | Oide_apply (f,x), Oide_apply(g,y) -> assert false
  | Oide_dot (id,s), Oide_dot(id',s') -> assert false
  | Oide_ident s, Oide_ident s' -> assert false
  | _ -> assert false

let rec type' t1 t2 =
  let eq = eq (typ0 t1) in
  match t1, t2 with
  | Otyp_abstract, Otyp_abstract
  | Otyp_open, Otyp_open -> eq
  | Otyp_alias (ty,alias), Otyp_alias(ty2,alias2) ->
      as' (type' ty ty2) (diff (=) string alias alias2)
  | Otyp_arrow(lbl,arg,res), Otyp_arrow(lbl2,arg2,res2) ->
      (* TODO: ellide beyond a certain size *)
      (label lbl lbl2) -: (type' arg arg2) --> (type' res res2)

  | Otyp_tuple l, Otyp_tuple l' ->
      paren @@ list product typ0 type' l l'
  | Otyp_constr (t1, args), Otyp_constr(t2,args2) -> assert false
  | _ -> mkdiff None typ0 t1 t2
