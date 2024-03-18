
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
  | Named of string
  | HSL of {h:float;s:float;l:float}

let red = Named "red"
let blue = Named "blue"
let green = Named "green"
let purple = Named "purple"
let lightgrey = Named "lightgrey"
let hsl ~h ~s ~l = HSL {h;s;l}

type style =
  | Filled of color option
  | Dotted
  | Dash

type modal =
| Color of color
| Style of style
| Label of string list

type label = modal list

type rlabel = { color: color option; style: style option; label: string list}


type dir = Toward | From

let update r l = match l with
  | Color c -> { r with color = Some c}
  | Style s -> { r with style = Some s}
  | Label s -> { r with label = s}

let none = { color = None; style=None; label = [] }

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
    label = merge_label l.label r.label;
  }

type params = {
  short_ids:bool;
  ellide_links:bool;
  expansion_as_hyperedge:bool;
  colorize:bool;
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

let colorize params id =
  if not params.colorize then None
  else
    let nhues = 200 in
    let h = float_of_int (17 * id mod nhues) /. float_of_int nhues in
    Some (hsl ~h ~s:0.3 ~l:0.7)

let string_of_field_kind v =
  match Types.field_kind_repr v with
  | Fpublic -> "public"
  | Fabsent -> "absent"
  | Fprivate -> "private"


module Index: sig
  type t = private int
   val split: params -> Types.type_expr -> t * color option * Types.type_desc
 end = struct

  type t = int
  type name_map = {
    last: int ref;
    tbl: (t,int) Hashtbl.t;
  }
  let id_map = { last = ref 0; tbl = Hashtbl.create 20 }

  let pretty_id params id =
    if not params.short_ids then id else
      match Hashtbl.find_opt id_map.tbl id with
      | Some x -> x
      | None ->
          incr id_map.last;
          let last = !(id_map.last) in
          Hashtbl.replace id_map.tbl id last;
          last

  let split params x =
    let x = repr params x in
    let color = colorize params x.id in
    pretty_id params x.id, color, x.desc

  let _index params ty = pretty_id params (repr params ty).id

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
    hyperedges: Hyperedge_set.t;
    subgraphes: (rlabel * subgraph) list;
  }


let empty_subgraph=
  { nodes = Node_set.empty;
    edges=Edge_set.empty;
    hyperedges = Hyperedge_set.empty;
    subgraphes = [];
  }

type 'index entity =
  | Node of 'index
  | Edge of 'index * 'index
  | Hyperedge of (dir * label * 'index) list
type element = Types.type_expr entity


module Entity_map = Map.Make(struct
    type t = Index.t entity
    let compare = Stdlib.compare
  end)
let (.%()) map e =
  Option.value ~default:none @@
  Entity_map.find_opt e map

module Pp = struct

  let semi ppf () = fprintf ppf ";@ "
  let space ppf () = fprintf ppf "@ "
  let empty ppf () = fprintf ppf ""
  let string =pp_print_string
  let list ~sep = pp_print_list ~pp_sep:sep
  let seq ~sep = pp_print_seq ~pp_sep:sep
  let rec longident ppf = function
    | Longident.Lident s -> fprintf ppf "%s" s
    | Longident.Ldot (l,s) -> fprintf ppf "%a.%s"  longident l s
    | Longident.Lapply(f,x) -> fprintf ppf "%a(%a)" longident f  longident x

  let color ppf = function
    | Named s -> fprintf ppf "%s" s
    | HSL r -> fprintf ppf "%1.3f %1.3f %1.3f" r.h r.s r.l

  let style ppf = function
    | Filled _ -> fprintf ppf "filled"
    | Dash -> fprintf ppf "dashed"
    | Dotted -> fprintf ppf "dotted"

  let modal ppf = function
    | Color c -> fprintf ppf {|color="%a"|} color c
    | Style s ->
        fprintf ppf {|style="%a"|} style s;
        begin match s with
        | Filled (Some c) -> fprintf ppf {|;@ fillcolor="%a"|} color c;
        | _ -> ()
        end;
    | Label s -> fprintf ppf {|label=<%a>|} (list ~sep:space string) s

  let inline_label ppf r =
    match decompose r with
    | [] -> ()
    | l -> fprintf ppf "@[<v>%a@]" (list ~sep:semi modal) l

  let label ppf r =
    match decompose r with
    | [] -> ()
    | l -> fprintf ppf "[@[<h>%a@]]" (list ~sep:semi modal) l


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

  let index ppf x =
    fprintf ppf "%d" (x: Index.t :> int)

  let hyperedge_id ppf l =
    let sep ppf () = fprintf ppf "h" in
    let elt ppf (_,_,x) = index ppf x in
    fprintf ppf "h%a" (list ~sep elt) l

  let node graph ppf x =
    let lbl = graph.%(Node x) in
    fprintf ppf "%a%a;@ " index x label lbl

  let edge graph ppf (x,y) =
    let lbl = graph.%(Edge (x,y)) in
    fprintf ppf "%a->%a%a;@ " index x index y label lbl

  let hyperedge graph ppf l =
    let lbl = graph.%(Hyperedge l) in
    fprintf ppf "%a%a;@ " hyperedge_id l label lbl;
    List.iter (fun (dir,lbl,x) ->
        let lbl = make lbl in
        match dir with
        | From -> fprintf ppf "%a->%a%a;@ " index x hyperedge_id l label lbl
        | Toward -> fprintf ppf "%a->%a%a;@ " hyperedge_id l index x label lbl
      ) l

  let rec subgraph graph ppf (lbl,sg) =
    fprintf ppf
      "@[<v 2>subgraph cluster {@,\
       %a;@ \
       %a%a%a%a}@]@."
      inline_label lbl
      (seq ~sep:empty (node graph)) (Node_set.to_seq sg.nodes)
      (seq ~sep:empty (edge graph)) (Edge_set.to_seq sg.edges)
      (seq ~sep:empty (hyperedge graph)) (Hyperedge_set.to_seq sg.hyperedges)
      (list ~sep:empty (subgraph graph)) sg.subgraphes


  let graph ppf (graph,sg) =
    fprintf ppf "@[<v 2>digraph {@,%a%a%a%a}@]@."
    (seq ~sep:empty (node graph)) (Node_set.to_seq sg.nodes)
    (seq ~sep:empty (edge graph)) (Edge_set.to_seq sg.edges)
    (seq ~sep:empty (hyperedge graph)) (Hyperedge_set.to_seq sg.hyperedges)
    (list ~sep:empty (subgraph graph)) sg.subgraphes


end






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

module Digraph = struct

    let add_to_subgraph s = function
    | Node ty ->
        let nodes = Node_set.add ty s.nodes in
        { s with nodes }
    | Edge (x,y) ->
        let edges = Edge_set.add (x,y) s.edges in
        { s with edges }
    | Hyperedge l ->
        let hyperedges = Hyperedge_set.add l s.hyperedges in
        { s with hyperedges }

    let add_subgraph sub g =
      { g with subgraphes = sub :: g.subgraphes }

  let add lbl entry (g,s) =
    match Entity_map.find_opt entry g with
    | Some lbl' -> Entity_map.add entry (merge lbl' lbl) g, s
    | None ->
        let g = Entity_map.add entry lbl g in
        g, add_to_subgraph s entry

  let rec hyperedges_of_memo ty params id abbrev gh =
    match abbrev with
    | Types.Mnil -> gh
    | Types.Mcons (_priv, _p, t1, t2, rem) ->
        let s, gh = ty params t1 gh in
        let exp, gh = ty params t2 gh in
        gh |>
        add memo
          (Hyperedge
             [From, [Style Dotted], id;
              Toward, [Style Dotted], s;
              Toward, [Label ["expand"]], exp
             ])
        |> hyperedges_of_memo ty params id rem
    | Types.Mlink rem -> hyperedges_of_memo ty params id !rem gh

  let rec edges_of_memo ty params abbrev gh =
    match abbrev with
    | Types.Mnil -> gh
    | Types.Mcons (_priv, _p, t1, t2, rem) ->
        let x, gh = ty params t1 gh in
        let y, gh = ty params t2 gh in
        gh |> add memo (Edge (x,y)) |> edges_of_memo ty params rem
    | Types.Mlink rem -> edges_of_memo ty params !rem gh


  let expansions ty params id memo gh =
    if params.expansion_as_hyperedge then
      hyperedges_of_memo ty params id memo gh
    else
      edges_of_memo ty params memo gh

  let labelk k fmt = kasprintf (fun s -> k (make [Label [s]])) fmt
  let labelf fmt = labelk Fun.id fmt

  let add_node label color id tynode gh =
    let lbl = Label [asprintf "<SUB>%a</SUB>" Pp.index id] in
    let lbl = match color with
    | None -> make [lbl]
    | Some _ as x -> make [lbl; Style (Filled x)]
    in
    let label = merge label lbl in
    add label tynode gh

  let group ty lbl l (g,sgs) =
    let g, nested = List.fold_left (fun gh t -> snd (ty t gh)) (g,empty_subgraph) l in
    g, add_subgraph (lbl,nested) sgs

  let rec inject_typ params ty0 (g,_ as gh) =
    let (id, color, desc) = Index.split params ty0 in
    let tynode = Node id in
    if Entity_map.mem tynode g then id, gh
    else id, node params color id tynode desc gh
  and edge params id0 lbl ty gh =
    let id, gh = inject_typ params ty gh in
    add lbl (Edge(id0,id)) gh
  and numbered_edge params id0 (i,gh) ty =
    let l = labelf "%d" i in
    i + 1, edge params id0 l ty gh
  and numbered_edges params id0 l gh =
    snd @@ List.fold_left (numbered_edge params id0) (0,gh) l
  and node params color id tynode desc gh =
    let add_tynode l = add_node l color id tynode gh in
    let group = group (inject_typ params) in
    let mk fmt = labelk add_tynode fmt in
    let numbered = numbered_edges params id in
    let edge = edge params id in
    let std_edge = edge std in
    match desc with
    | Types.Tvar name -> mk "%a" pretty_var name
    | Types.Tarrow(l,t1,t2,_) ->
       let gh = mk "→%a" exponent_of_label l in numbered [t1; t2] gh
    | Types.Ttuple tl ->
        mk "," |> numbered tl
    | Types.Tconstr (p,tl,abbrevs) ->
        mk "%a" Path.print p
        |> expansions inject_typ params id !abbrevs
        |> numbered tl
    | Types.Tobject (t, name) ->
        begin match !name with
        | None -> mk "obj" |> std_edge t
        | Some (p,tl) ->
            mk "obj(%a)" Path.print p |> std_edge t |> numbered tl
        end
    | Types.Tfield (f, k, t1, t2) ->
        mk "%s<SUP>%s</SUP>" f (string_of_field_kind k)
        |> numbered [t1;t2]
    | Types.Tnil -> mk "∅"
    | Types.Tlink t -> add_tynode (make [Style Dash]) |> std_edge t
    | Types.Tsubst (t, o) ->
        add_tynode (make [Label ["Subst"]; Style Dotted])
        |> numbered (t :: Option.to_list o)
    | Types.Tunivar name ->
        mk "%a<SUP>∀</SUP>" pretty_var name
    | Types.Tpoly (t, tl) ->
        let params = merge (make [Style (Filled (Some lightgrey))]) (labelf "∀%a" Pp.index id) in
        mk "∀" |> group params tl |> std_edge t
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
        add_tynode pr |> numbered types
    | Types.Tpackage (p, fl) ->
        let types = List.map snd fl in
        mk "mod %a[%a]"
          Path.print p
          Pp.(list ~sep:semi longident) (List.map fst fl)
        |> numbered types
  and field_edge rf = Types.match_row_field
      ~absent:(fun _ -> [])
      ~present:Option.to_list
      ~either:(fun _ tl _ e ->
          List.concat_map field_edge (Option.to_list e) @ tl
        )
      rf
end

let params
    ?(ellide_links=true)
    ?(expansion_as_hyperedge=false)
    ?(short_ids=true)
    ?(colorize=true)
    () =
  {
    expansion_as_hyperedge;
    short_ids;
    ellide_links;
    colorize;
  }


let translate params gh (label,entry) =
  let node, gh = match entry with
    | Node ty ->
        let id, gh = Digraph.inject_typ params ty gh in
        Node id, gh
    | Edge (ty,ty') ->
        let id, gh = Digraph.inject_typ params ty gh in
        let id', gh = Digraph.inject_typ params ty' gh in
        Edge(id,id'), gh
    | Hyperedge l ->
        let l, gh = List.fold_left (fun (l,gh) (d,lbl,ty) ->
            let id, gh = Digraph.inject_typ params ty gh in
            (d,lbl,id)::l, gh
          ) ([],gh) l
        in
       Hyperedge l, gh
  in
  Digraph.add label node gh

let translate_entries params ts =
  List.fold_left (translate params) (Entity_map.empty, empty_subgraph) ts

let entries params ppf ts =
  let (g,gs) = translate_entries params ts in
  Pp.graph ppf (g,gs)

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
