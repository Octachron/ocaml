
(* Print a raw type expression, with sharing *)

let debug_on = ref (fun () -> false)
let debug f = if !debug_on () then f ()

let debug_off f =
  let old = !debug_on in
  debug_on := Fun.const false;
  Fun.protect f
    ~finally:(fun () -> debug_on := old)

open Format

type color =
  | Red
  | Green
  | Blue
  | Purple
  | Black

type style =
  | Filled
  | Dotted
  | Dash

type modal =
| Color of color
| Background of color
| Style of style
| Label of string

type label = modal list

type rlabel = { background:color option; color: color option; style: style option; label: string option}
let none = { background=None; color = None; style = None; label = None}

let update r l = match l with
  | Color c -> { r with color = Some c}
  | Background c -> { r with background = Some c}
  | Style s -> { r with style = Some s}
  | Label s -> { r with label = Some s}


let make l = List.fold_left update none l

let label r = Option.map (fun x -> Label x) r.label
let color r = Option.map (fun x -> Color x) r.color
let style r = Option.map (fun x -> Style x) r.style

let decompose r =
  let (@?) x l = match x with
    | None -> l
    | Some x -> x :: l
   in
  label r @? color r @? style r @? []

(*let fill_default r l = match l with
  | Color default ->
      { r with color = Some (Option.value ~default r.color) }
  | Style default ->
      { r with style = Some (Option.value ~default r.style) }
  | Label default ->
      { r with label = Some(Option.value ~default r.label) }
**)
let arg n = { none with label = Some (string_of_int n) }
let std = none
let memo = { none with label = Some "expand"; style=Some Dash }

let alt x y = match x with
  | None -> y
  | Some _ -> x

let merge l r =
  { color = alt l.color r.color;
    style = alt l.style r.style;
    label = alt l.label r.label;
    background = alt l.background r.background
  }

module Pp = struct

  let semi ppf () = fprintf ppf ";@ "
  let list ~sep = pp_print_list ~pp_sep:sep
  let rec longident ppf = function
    | Longident.Lident s -> fprintf ppf "%s" s
    | Longident.Ldot (l,s) -> fprintf ppf "%a.%s"  longident l s
    | Longident.Lapply(f,x) -> fprintf ppf "%a(%a)" longident f  longident x

  let color ppf = function
    | Red -> fprintf ppf "red"
    | Blue -> fprintf ppf "blue"
    | Green -> fprintf ppf "green"
    | Purple -> fprintf ppf "purple"
    | Black -> fprintf ppf "black"

  let style ppf = function
    | Filled -> fprintf ppf "filled"
    | Dash -> fprintf ppf "dashed"
    | Dotted -> fprintf ppf "dotted"

  let modal ppf = function
    | Color c -> fprintf ppf {|color="%a"|} color c
    | Style s -> fprintf ppf {|style="%a"|} style s
    | Label s -> fprintf ppf {|label=<%s>|} s
    | Background c -> fprintf ppf {|fill_color="%a"|} color c

  let decorate ppf r =
    match decompose r with
    | [] -> ()
    | l -> fprintf ppf "[%a]" (list ~sep:semi modal) l
end

module Int_map = Map.Make(Int)
type params = {
  short_ids:bool;
  ellide_links:bool;
  expansion_as_hyperedge:bool;
  labels: rlabel Int_map.t
}

let ellide_links ty =
  let rec follow_safe visited t =
    let t = Types.Transient_expr.coerce t in
    if List.memq t visited then t
    else match t.Types.desc with
      | Tlink t' -> follow_safe (t::visited) t'
      | _ -> t
  in
  follow_safe [] ty

let repr params ty =
  if params.ellide_links then ellide_links ty
  else Types.Transient_expr.coerce ty



let (.%()) map (params,ty) =
  Option.value ~default:none @@
  Int_map.find_opt (repr params ty).id map

let string_of_field_kind v =
  match Types.field_kind_repr v with
  | Fpublic -> "public"
  | Fabsent -> "absent"
  | Fprivate -> "private"


type dir = Toward | From

type entity =
  | Node of Types.type_expr
  | Edge of Types.type_expr * Types.type_expr
  | Hyperedge of (dir * label * Types.type_expr) list

module Index: sig
(*  type t = private int
   val index: params -> Types.type_expr -> t
  val pretty_index: params -> t -> int *)
  val get_id: params -> Types.type_expr -> int
end = struct

  type t = int
  type name_map = {
    last: int ref;
    tbl: (t,int) Hashtbl.t;
  }
  let id_map = { last = ref 0; tbl = Hashtbl.create 20 }

  let index params x = (repr params x).id
  let pretty_index params id =
    if not params.short_ids then id else
      match Hashtbl.find_opt id_map.tbl id with
      | Some x -> x
      | None ->
          incr id_map.last;
          let last = !(id_map.last) in
          Hashtbl.replace id_map.tbl id last;
          last

  let get_id params x = pretty_index params (index params x)
end
(*
type index = Index.t

type indexed_entity =
  | Inode of index
  | IEdge of index * index
  | IHyperedge of (dir * label * index) list

module Node_set = Set.Make(struct
    type t = Index.t
    let compare = Stdlib.compare
end)

module Edge_set = Set.Make(struct
    type t = Index.t
    let compare = Stdlib.compare
end)

module Hyperedge_set = Set.Make(struct
    type t = Index.t list
    let compare = Stdlib.compare
end)

type subgraph =
  {
    nodes: Node_set.t;
    edges: Edge_set.t;
    hyperedges: Hyperedge_set.t
  }

module Entity_map = Map.Make(struct
    type t = indexed_entity
    let compare = Stdlib.compare
  end)
*)
let ty_id params ppf x =
  fprintf ppf "%d" (Index.get_id params x)

let pp_id params ppf x =
  let ty_id = ty_id params in
  match x with
  | Node x -> fprintf ppf "n%a" ty_id  x
  | Edge (x,y) -> fprintf ppf "e%ae%a" ty_id x ty_id y
  | Hyperedge l ->
      let sep ppf () = fprintf ppf "h" in
      let elt ppf (_,_,x) = ty_id ppf x in
      fprintf ppf "h%a" Pp.(list ~sep elt) l

let rec hyperedges_of_memo ty = function
  | Types.Mnil -> []
  | Types.Mcons (_priv, _p, t1, t2, rem) ->
      (memo,
       Hyperedge [From, [Style Dotted], ty
                 ;Toward, [Style Dotted], t1;
                  Toward, [Label "expand"], t2
                 ]) :: hyperedges_of_memo ty rem
  | Types.Mlink rem -> hyperedges_of_memo ty !rem

let rec edges_of_memo = function
  | Types.Mnil -> []
  | Types.Mcons (_priv, _p, t1, t2, rem) ->
      (memo, Edge (t1,t2)) :: edges_of_memo rem
  | Types.Mlink rem -> edges_of_memo !rem

let exponent_of_label ppf = function
  | Asttypes.Nolabel -> ()
  | Asttypes.Labelled s -> fprintf ppf "<SUP>%s</SUP>" s
  | Asttypes.Optional s -> fprintf ppf "<SUP>?%s</SUP>" s

let pretty_var ppf name =
  let name = Option.value ~default:"_" name in
  let name' =
    match name with
    | "a" -> "𝛼"
    | "b" -> "𝛽"
    | "c" -> "𝛾"
    | "d" -> "𝛿"
    | "e" -> "𝜀"
    | "f" -> "𝜑"
    | "t" -> "𝜏"
    | "r" ->  "𝜌"
    | "s" ->  "𝜎"
    | "p" -> "𝜋"
    | "i" -> "𝜄"
    | "h" -> "𝜂"
    | "k" -> "k"
    | "l" -> "𝜆"
    | "m" -> ""
    | "x" -> "𝜒"
    | "n" -> "𝜐"
    | "o" -> "𝜔"
    | name -> name
  in
  if name = name' then
    fprintf ppf "'%s" name
  else pp_print_string ppf name'

let numbered ty tl = List.mapi (fun i x -> arg i, Edge (ty, x)) tl


let expansions params ty memo =
  if params.expansion_as_hyperedge then
    hyperedges_of_memo ty memo
  else
    edges_of_memo memo

let rec typ params visited ppf ty0 =
  let ty = repr params ty0 in
  if List.memq ty !visited then ()
  else begin
    visited := ty :: !visited;
    node params visited ty0 ppf ty.desc
  end
and node params visited ty0 ppf desc =
  let user_label =params.labels.%(params,ty0) in
  let node_label, node_meta , sub_entities = split_node params ty0 desc in
  let label = asprintf "%t<SUB>%d</SUB>" node_label (Index.get_id params ty0) in
  let node_label = update (make node_meta) (Label label) in
  let label = merge user_label node_label in
  fprintf ppf "%a%a;@," (pp_id params) (Node ty0) Pp.decorate label;
  List.iter (entity params visited ppf) sub_entities
and split_node params ty0 = function
| Types.Tvar name -> dprintf "%a" pretty_var name, [], []
| Types.Tarrow(l,t1,t2,_) ->
    dprintf "→%a" exponent_of_label l,
    [],
    numbered ty0 [t1; t2]
| Types.Ttuple tl ->
    dprintf ",",
    [],
    numbered ty0 tl
| Types.Tconstr (p,tl,abbrevs) ->
    dprintf "%a" Path.print p,
    [],
    numbered ty0 tl @ expansions params ty0 !abbrevs
| Types.Tobject (t, name) ->
    dprintf "obj", [],
     begin match !name with
      | None -> [std, Edge (ty0, t)]
      | Some (_p,tl) -> (std, Edge (ty0, t))::numbered ty0 tl
      end
| Types.Tfield (f, k, t1, t2) ->
    dprintf "%s<SUP>%s</SUP>"
      f (string_of_field_kind k),
    [],
    numbered ty0 [t1;t2]
| Types.Tnil -> dprintf "∅", [], []
| Types.Tlink t -> ignore, [Style Dash], [std, Edge(ty0, t)]
| Types.Tsubst (t, o) ->
    dprintf "Subst", [Style Dotted],
    numbered ty0 (t :: Option.to_list o)
| Types.Tunivar name ->
    dprintf "%a<SUP>∀</SUP>" pretty_var name, [], []
| Types.Tpoly (t, tl) ->
    dprintf "∀", [],
    (std, Edge(ty0, t)) :: (numbered ty0 tl)
| Types.Tvariant row ->
    let Row {fields; more; name; fixed; closed} = Types.row_repr row in
    let pr, args = match name with
    | None ->
        dprintf "[Row(%B,%a,[%a])]" closed row_fixed fixed
          Pp.(list ~sep:semi field_node) fields, []
    | Some (p,tl) ->
        dprintf "[Row %a(%B,%a,%a)]"
          Path.print p closed row_fixed fixed
          Pp.(list ~sep:semi field_node) fields,
        tl
    in
    pr, [],
    numbered ty0 @@ args @ more :: List.concat_map field_edge (List.map snd fields)
| Types.Tpackage (p, fl) ->
    dprintf "mod %a[%a]"
      Path.print p
      Pp.(list ~sep:semi longident) (List.map fst fl),
    [],
    numbered ty0 (List.map snd fl)
and edge params ppf (lbl,x,y) =
  fprintf ppf "%a->%a%a;@ "
    (pp_id params) x
    (pp_id params) y
    Pp.decorate lbl;
and edges params ppf tys=
  List.iter (edge params ppf) tys
and row_fixed ppf = function
| None -> fprintf ppf ""
| Some Types.Fixed_private -> fprintf ppf "private"
| Some Types.Rigid -> fprintf ppf "rigid"
| Some Types.Univar _t -> fprintf ppf "univar"
| Some Types.Reified _p -> fprintf ppf "reified)"

and field_node ppf (lbl,rf) =
  Types.match_row_field
    ~absent:(fun _ -> fprintf ppf "%s(absent)" lbl)
    ~present:(fun _ -> fprintf ppf "%s(present)" lbl)
    ~either:(fun c _tl m _e -> fprintf ppf "%s?(%B,%B)" lbl c m)
    rf
and field_edge rf = Types.match_row_field
  ~absent:(fun _ -> [])
  ~present:Option.to_list
  ~either:(fun _ tl _ e ->
      List.concat_map field_edge (Option.to_list e) @ tl
    )
  rf
and entity params visited ppf (lbl,entry) = match entry with
  | Node ty -> typ params visited ppf ty
  | Edge (x,y) ->
      typ params visited ppf x;
      typ params visited ppf y;
      edges params ppf [lbl, Node x, Node y]
  | Hyperedge l as h->
      hyper params ppf h;
      List.iter (fun (_,_,x) -> typ params visited ppf x) l;
      List.iter (fun (dir,lbl,x) ->
          let lbl = make lbl in
          let e = match dir with
            | From -> [lbl, Node x, h]
            | Toward -> [lbl, h, Node x]
          in
          edges params ppf e
        ) l
and hyper params ppf h =
  fprintf ppf "%a[shape=\"triangle\"; label=\"\"];@,"
    (pp_id params) h


let add_info params map (info,entry) =
  match entry with
  | Edge _ | Hyperedge _ -> map
  | Node ty ->
      let ty = repr params ty in
      let info = match Int_map.find_opt ty.id map with
        | None -> info
        | Some i -> merge i info
      in
      Int_map.add ty.id info map


let params
    ?(ellide_links=true)
    ?(expansion_as_hyperedge=false)
    ?(short_ids=true)
    () =
  {
    labels=Int_map.empty;
    expansion_as_hyperedge;
    short_ids;
    ellide_links
  }

let entries params ppf ts =
  let visited = ref [] in
  let map = List.fold_left (add_info params) Int_map.empty ts in
  let params = { params with labels=map} in
  fprintf ppf "@[<v>digraph G {@,";
  List.iter (entity params visited ppf) ts;
  fprintf ppf "@,}@]@."


let file_counter = ref 0

let compact_loc ppf (loc:Warnings.loc) =
  let startline = loc.loc_start.pos_lnum in
  let endline = loc.loc_end.pos_lnum in
  let startchar = loc.loc_start.pos_cnum - loc.loc_start.pos_bol in
  let endchar = loc.loc_end.pos_cnum - loc.loc_end.pos_bol in
  if startline = endline then
    fprintf ppf "l%d[%d-%d]" startline startchar endchar
  else
    fprintf ppf "l%d-%d[%d-%d]" startline endline startchar endchar

type 'a context = 'a option ref * (Format.formatter -> 'a -> unit)

let set_context (r,_pr) x = r := Some x
let pp (r,pr) ppf = match !r with
  | None -> ()
  | Some x -> fprintf ppf "%a" pr x

let with_context (r,_) x f =
  let old = !r in
  r:= Some x;
  Fun.protect f ~finally:(fun () -> r := old)

let global = ref None, pp_print_string
let loc = ref None, compact_loc
let context = [pp global; pp loc]
let dash ppf () = fprintf ppf "-"

let node_register = ref []
let register_type (label,ty) =
  node_register := (make label,Node ty) :: !node_register
let forget () = node_register := []


let label x = x
let node x = Node x
let edge x y = Edge(x,y)
let hyperedge l = Hyperedge l


let nodes ~title params ts =
  incr file_counter;
  let filename =
    match !Clflags.dump_dir with
    | None -> asprintf "%04d-%s.dot"  !file_counter title
    | Some d ->
        asprintf "%s%s%04d-%s-%a.dot"
          d Filename.dir_sep
          !file_counter
          title
          Pp.(list ~sep:dash (fun ppf pr -> pr ppf)) context
  in
  Out_channel.with_open_bin filename (fun ch ->
      let ppf = Format.formatter_of_out_channel ch in
      let ts = List.map (fun (l,t) -> make l, t) ts in
      entries params ppf (ts @ !node_register)
    )

let types ~title params ts =
  nodes ~title params (List.map (fun (lbl,ty) -> lbl, Node ty) ts)
