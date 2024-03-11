
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
| Label of string list

type label = modal list

type rlabel = { background:color option; color: color option; style: style option; label: string list}

let update r l = match l with
  | Color c -> { r with color = Some c}
  | Background c -> { r with background = Some c}
  | Style s -> { r with style = Some s}
  | Label s -> { r with label = s}

let none = { background = None; color = None; style=None; label = [] }

let make l = List.fold_left update none l

let label r = if r.label = [] then None else Some (Label r.label)
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
let arg n = { none with label = [string_of_int n] }
let std = none
let memo = { none with label = ["expand"]; style=Some Dash }

let alt x y = match x with
  | None -> y
  | Some _ -> x

module String_set = Set.Make(String)
let merge_label l r =
  let r' = String_set.of_list r in
  let l' = String_set.of_list l in
  List.filter (fun x -> not (String_set.mem x r') ) l
  @ List.filter (fun x -> not (String_set.mem x l') ) r
let merge l r =
  { color = alt l.color r.color;
    style = alt l.style r.style;
    label = l.label @ r.label;
    background = alt l.background r.background
  }

module Pp = struct

  let semi ppf () = fprintf ppf ";@ "
  let space ppf () = fprintf ppf "@ "
  let string =pp_print_string
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
    | Label s -> fprintf ppf {|label=<%a>|} (list ~sep:space string) s
    | Background c -> fprintf ppf {|fill_color="%a"|} color c

  let decorate ppf r =
    match decompose r with
    | [] -> ()
    | l -> fprintf ppf "[%a]" (list ~sep:semi modal) l

  let row_fixed ppf = function
    | None -> fprintf ppf ""
    | Some Types.Fixed_private -> fprintf ppf "private"
    | Some Types.Rigid -> fprintf ppf "rigid"
    | Some Types.Univar _t -> fprintf ppf "univar"
    | Some Types.Reified _p -> fprintf ppf "reified"

  let field_node ppf (lbl,rf) =
    Types.match_row_field
      ~absent:(fun _ -> fprintf ppf "%s(absent)" lbl)
      ~present:(fun _ -> fprintf ppf "%s(present)" lbl)
      ~either:(fun c _tl m _e -> fprintf ppf "%s?(%B,%B)" lbl c m)
      rf

  let rec entity ppf (lbl,x) =
    let lbl = match x with
      | Hyperedge -> merge lbl @@ make [Shape Triangle]
      | _ -> lbl
    in
    fprintf ppf "%a%a" pp_id x label lbl;
    match x with
    | Node _ | Edge _ -> ()
    | Hyperedge l as h->
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

type 'index entity =
  | Node of 'index
  | Edge of 'index * 'index
  | Hyperedge of (dir * label * 'index) list

module Index: sig
  type t = private int
   val index: params -> Types.type_expr -> t
   val pretty_index: params -> t -> int
   val get_id: params -> Types.type_expr -> int
   val split: params -> Types.type_expr -> t * Types.type_desc
 end = struct

  type t = int
  type name_map = {
    last: int ref;
    tbl: (t,int) Hashtbl.t;
  }
  let id_map = { last = ref 0; tbl = Hashtbl.create 20 }

  let split params x =
    let x = repr params x in
    x.id, x.desc
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

type index = Index.t


module Node_set = Set.Make(struct
    type t = Index.t
    let compare = Stdlib.compare
end)

module Edge_set = Set.Make(struct
    type t = Index.t * Index.t
    let compare = Stdlib.compare
end)

module Hyperedge_set = Set.Make(struct
    type t = (dir * label * index) list
    let compare = Stdlib.compare
end)

type subgraph =
  {
    nodes: Node_set.t;
    edges: Edge_set.t;
    hyperedges: Hyperedge_set.t
  }

module Entity_map = Map.Make(struct
    type t = Index.t entity
    let compare = Stdlib.compare
  end)

let ty_id ppf x =
  fprintf ppf "%d" (x: index :> int)

let pp_id ppf x =
  match x with
  | Node x -> fprintf ppf "n%a" ty_id  x
  | Edge (x,y) -> fprintf ppf "e%ae%a" ty_id x ty_id y
  | Hyperedge l ->
      let sep ppf () = fprintf ppf "h" in
      let elt ppf (_,_,x) = ty_id ppf x in
      fprintf ppf "h%a" Pp.(list ~sep elt) l



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



let update_label e l em =
  let lbl =
    match Entity_map.find_opt e em with
    | None -> l
    | Some lbl -> merge lbl l
  in
  Entity_map.add e lbl em

module Collect = struct
  let empty = [], []
  let (@^) (a,b) (c,d) = a @ c, b @ d
  let (@:) (a,b) (c,d) = a :: c, b :: d
  let edge params x ty = Edge (x, Index.index params ty)


  let rec hyperedges_of_memo params id = function
  | Types.Mnil -> empty
  | Types.Mcons (_priv, _p, t1, t2, rem) ->
      let s = Index.index params t1 and exp = Index.index params t2 in
      ([t1;t2],
      [memo,
       Hyperedge [From, [Style Dotted], id
                 ;Toward, [Style Dotted], s;
                  Toward, [Label ["expand"]], exp
                 ]]) @^ hyperedges_of_memo params id rem
  | Types.Mlink rem -> hyperedges_of_memo params id !rem

let rec edges_of_memo params = function
  | Types.Mnil -> empty
  | Types.Mcons (_priv, _p, t1, t2, rem) ->
      let x = Index.index params t1 and y = Index.index params t2 in
      ([t1;t2], [memo, Edge (x,y)]) @^ edges_of_memo params rem
  | Types.Mlink rem -> edges_of_memo params !rem


let expansions params id memo =
  if params.expansion_as_hyperedge then
    hyperedges_of_memo params id memo
  else
    edges_of_memo params memo

  let labelf fmt = kasprintf (fun s -> make [Label [s]]) fmt
  let numbered params id tl =
    tl, List.mapi (fun i x -> arg i, edge params id x) tl


  let entity params (g,s as sgs) (lbl,entry) =
    match Entity_map.find_opt entry g with
    | Some lbl' -> Entity_map.add entry (merge lbl' lbl) g, s
    | None ->
        let g = Entity_map.add entry lbl g in
        match entry with
        | Node ty ->
            let nodes = Node_set.add ty s.nodes in
            g, { s with nodes }
        | Edge (x,y) ->
            let edges = Edge_set.add (x,y) s.edges in
            g, { s with edges }
        | Hyperedge l as h->
            let hyperedges = Hyperedge_set.add l s.hyperedges in
            g, { s with hyperedges }

  let rec inject_typ params (graph, s as gh) ty0 =
    let (id, desc) = Index.split params ty0 in
    let tynode = Node id in
    if Entity_map.mem tynode graph then gh
    else
      let label, (types, graph_objects) = node params gh id desc in
      let g = update_label tynode label graph in
      let gh = (g,s) in
      let gh = List.fold_left (inject_typ params) gh types in
      List.fold_left (entity params) gh graph_objects
  and node params gs id desc =
    let numbered = numbered params id in
    let edge = edge params id in
    let std_edge x = [x], [std, edge x] in
    match desc with
    | Types.Tvar name -> labelf "%a" pretty_var name, empty
    | Types.Tarrow(l,t1,t2,_) ->
        labelf "→%a" exponent_of_label l,
        numbered [t1; t2]
    | Types.Ttuple tl ->
        labelf ",", numbered tl
    | Types.Tconstr (p,tl,abbrevs) ->
        labelf "%a" Path.print p,
        expansions params id !abbrevs @^ numbered tl
    | Types.Tobject (t, name) ->
        begin match !name with
        | None -> labelf "obj", std_edge t
        | Some (p,tl) ->
            labelf "obj(%a)" Path.print p, std_edge t @^ numbered tl
        end
    | Types.Tfield (f, k, t1, t2) ->
        labelf "%s<SUP>%s</SUP>" f (string_of_field_kind k),
        numbered [t1;t2]
    | Types.Tnil -> labelf "∅", empty
    | Types.Tlink t -> make [Style Dash], std_edge t
    | Types.Tsubst (t, o) ->
        make [Label ["Subst"]; Style Dotted],
        numbered (t :: Option.to_list o)
    | Types.Tunivar name ->
        labelf "%a<SUP>∀</SUP>" pretty_var name, empty
    | Types.Tpoly (t, tl) ->
        labelf "∀", (std_edge t) @^ numbered tl
    | Types.Tvariant row ->
        let Row {fields; more; name; fixed; closed} = Types.row_repr row in
        let pr, args = match name with
          | None ->
              labelf "[Row(%B,%a,[%a])]" closed Pp.row_fixed fixed
                Pp.(list ~sep:semi field_node) fields, []
          | Some (p,tl) ->
              labelf "[Row %a(%B,%a,%a)]"
                Path.print p closed Pp.row_fixed fixed
                Pp.(list ~sep:semi field_node) fields,
              tl
        in
        let types = args @ more :: List.concat_map field_edge (List.map snd fields) in
        pr, numbered types
    | Types.Tpackage (p, fl) ->
        let types = List.map snd fl in
        labelf "mod %a[%a]"
          Path.print p
          Pp.(list ~sep:semi longident) (List.map fst fl),
        numbered types
  and field_edge rf = Types.match_row_field
      ~absent:(fun _ -> [])
      ~present:Option.to_list
      ~either:(fun _ tl _ e ->
          List.concat_map field_edge (Option.to_list e) @ tl
        )
      rf
end


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
