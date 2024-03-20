
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

type shape =
  | Ellipse
  | Circle
  | Diamond

type modal =
| Color of color
| Font_color of color
| Style of style
| Label of string list
| Shape of shape

let filled c = Style (Filled (Some c))

type label = modal list

type rlabel = {
  color: color option;
  font_color:color option;
  style: style option;
  label: string list;
  shape: shape option;
}


type dir = Toward | From

let update r l = match l with
  | Color c -> { r with color = Some c}
  | Style s -> { r with style = Some s}
  | Label s -> { r with label = s}
  | Font_color c -> { r with font_color = Some c}
  | Shape s -> { r with shape = Some s }

let none = { color=None; font_color=None; style=None; shape=None; label = [] }

let make l = List.fold_left update none l

let label r = if r.label = [] then None else Some (Label r.label)
let color r = Option.map (fun x -> Color x) r.color
let font_color r = Option.map (fun x -> Font_color x) r.font_color
let style r = Option.map (fun x -> Style x) r.style
let shape r = Option.map (fun x -> Shape x) r.shape

let decompose r =
  let (@?) x l = match x with
    | None -> l
    | Some x -> x :: l
   in
  label r @? color r @? font_color r @? style r @? shape r @? []

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
    font_color = alt l.font_color r.font_color;
    shape = alt l.shape r.shape;
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
    let s = match id mod 3 with
      | 0 -> 0.3
      | 1 -> 0.5
      | 2 | _ -> 0.7
     in
     let l = match id mod 7 with
       | 0 -> 0.5
       | 1 -> 0.55
       | 2 -> 0.60
       | 3 -> 0.65
       | 4 -> 0.70
       | 5 -> 0.75
       | 6 | _ -> 0.8
     in
     Some (hsl ~h ~s ~l)

let string_of_field_kind v =
  match Types.field_kind_repr v with
  | Fpublic -> "public"
  | Fabsent -> "absent"
  | Fprivate -> "private"


module Index: sig
  type t = private
    | Main of int
    | Synthetic of int
    | Field of { id:int; synth:bool; name:string }
  val color_id: t -> int
  val field: name:string -> t -> t
  val field_ext: Types.row_field_cell ->  t
  val split: params -> Types.type_expr -> t * color option * Types.type_desc
 end = struct

  type t =
    | Main of int
    | Synthetic of int
    | Field of { id:int; synth:bool; name:string }

  type name_map = {
    last: int ref;
    tbl: (int,int) Hashtbl.t;
  }
  let id_map = { last = ref 0; tbl = Hashtbl.create 20 }

  let color_id = function
    | Main id | Synthetic id | Field {id;_} -> id


  let pretty_id params id =
    if not params.short_ids then Main id else
      match Hashtbl.find_opt id_map.tbl id with
      | Some x -> Main x
      | None ->
          incr id_map.last;
          let last = !(id_map.last) in
          Hashtbl.replace id_map.tbl id last;
          Main last

  let split params x =
    let x = repr params x in
    let color = colorize params x.id in
    pretty_id params x.id, color, x.desc

  let _index params ty = pretty_id params (repr params ty).id
  let field ~name x = match x with
    | Main id -> Field {id;name;synth=false}
    | Field r -> Field {r with name}
    | Synthetic id -> Field {id;name;synth=true}

  let either = ref []
  let synth = ref 0
  let field_ext r =
    match List.assq_opt r !either with
    | Some n -> Synthetic n
    | None ->
        let n = !synth in
        incr synth;
        either := (r,n) :: !either;
        Synthetic n

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

  let shape ppf = function
    | Circle -> fprintf ppf "circle"
    | Diamond -> fprintf ppf "diamond"
    | Ellipse -> fprintf ppf "ellipse"

  let modal ppf = function
    | Color c -> fprintf ppf {|color="%a"|} color c
    | Font_color c -> fprintf ppf {|fontcolor="%a"|} color c
    | Style s ->
        fprintf ppf {|style="%a"|} style s;
        begin match s with
        | Filled (Some c) -> fprintf ppf {|;@ fillcolor="%a"|} color c;
        | _ -> ()
        end;
    | Shape s -> fprintf ppf {|shape="%a"|} shape s
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


  let index ppf = function
    | Index.Main id -> fprintf ppf "i%d" id
    | Index.Synthetic id -> fprintf ppf "s%d" id
    | Index.Field r ->
        fprintf ppf "%s%dRF%s" (if r.synth then "s" else "i") r.id r.name

  let prettier_index ppf = function
    | Index.Main id -> fprintf ppf "%d" id
    | Index.Synthetic id -> fprintf ppf "[%d]" id
    | Index.Field r -> fprintf ppf "%d(%s)" r.id r.name


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

  let cluster_counter = ref 0
  let pp_cluster ppf =
    incr cluster_counter;
    fprintf ppf "cluster_%d" !cluster_counter

  let rec subgraph graph ppf (lbl,sg) =
    fprintf ppf
      "@[<v 2>subgraph %t {@,\
       %a;@ \
       %a%a%a%a}@]@."
      pp_cluster
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

  let add ?(user=false) lbl entry (g,s) =
    match Entity_map.find_opt entry g with
    | Some lbl' ->
        let lbl = if user then merge lbl lbl' else merge lbl' lbl in
        Entity_map.add entry lbl g, s
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

  let labelk k fmt = kasprintf (fun s -> k  [Label [s]]) fmt
  let labelf fmt = labelk Fun.id fmt
  let labelfr fmt = labelk make fmt

  let add_node label color id tynode gh =
    let lbl = labelf "<SUB>%a</SUB>" Pp.prettier_index id in
    let lbl = match color with
    | None -> make lbl
    | Some _ as x -> make (Style (Filled x) :: lbl)
    in
    let label = merge label lbl in
    add label tynode gh

  let field_node color lbl rf =
    let color = match color with
      | None -> []
      | Some c -> [Color c]
    in
    let pr_lbl ppf = match lbl with
      | None -> ()
      | Some lbl -> fprintf ppf "`%s" lbl
    in
    let txt =
      Types.match_row_field
        ~absent:(fun _ -> labelf "`-%t" pr_lbl)
        ~present:(fun _ -> labelf "&gt;%t" pr_lbl)
        ~either:(fun c _tl m _e ->
            labelf "%s%t%s"
              (if m then "?" else "")
              pr_lbl
              (if c then "(∅)" else "")
          )
        rf
    in
    make (Shape Diamond::color@txt)

  let group ty id0 lbl l (g,sgs as gh) =
    match l with
    | [] -> gh
    | first :: l ->
      let gh = g,empty_subgraph in
      let id, gh = ty first gh in
      let g, nested = List.fold_left (fun gh t -> snd (ty t gh)) gh l in
      let gh = (g, add_subgraph (lbl,nested) sgs) in
      gh |> add none (Edge(id0,id))

  let rec inject_typ ?(follow_expansions=true) params ty0 (g,_ as gh) =
    let (id, color, desc) = Index.split params ty0 in
    let tynode = Node id in
    if Entity_map.mem tynode g then id, gh
    else id, node params color id tynode ~follow_expansions desc gh
  and edge ~follow_expansions params id0 lbl ty gh =
    let id, gh = inject_typ ~follow_expansions params ty gh in
    add lbl (Edge(id0,id)) gh
  and poly_edge ~follow_expansions ~color params id0 gh ty =
    let id, gh = inject_typ ~follow_expansions params ty gh in
    match color with
    | None -> add (labelfr "bind") (Edge (id0,id)) gh
    | Some c ->
        let gh = add (make [Label ["bind"]; Color c]) (Edge (id0,id)) gh in
        add ~user:true (make [filled c]) (Node id) gh
  and numbered_edge ~follow_expansions params id0 (i,gh) ty =
    let l = labelfr "%d" i in
    i + 1, edge ~follow_expansions params id0 l ty gh
  and numbered_edges ~follow_expansions params id0 l gh =
    snd @@ List.fold_left
      (numbered_edge ~follow_expansions params id0)
      (0,gh) l
  and node ~follow_expansions params color id tynode desc gh =
    let add_tynode l = add_node l color id tynode gh in
    let mk fmt = labelk (fun l -> add_tynode (make l)) fmt in
    let numbered = numbered_edges ~follow_expansions params id in
    let edge = edge ~follow_expansions params id in
    let group = group (inject_typ ~follow_expansions params) in
    let std_edge = edge std in
    match desc with
    | Types.Tvar name -> mk "%a" pretty_var name
    | Types.Tarrow(l,t1,t2,_) ->
       let gh = mk "→%a" exponent_of_label l in numbered [t1; t2] gh
    | Types.Ttuple tl ->
        mk "," |> numbered tl
    | Types.Tconstr (p,tl,abbrevs) ->
        let constr = mk "%a" Path.print p |> numbered tl in
        if not follow_expansions then
          constr
        else
          expansions
            (inject_typ ~follow_expansions:true)
            params id !abbrevs constr
    | Types.Tobject (t, name) ->
        let gh =
          begin match !name with
          | None -> mk "obj"
          | Some (p,[]) -> (* invalid format *)
              mk "obj(%a)" Path.print p
          | Some (p, (rv_or_nil :: tl)) ->
              match Types.get_desc rv_or_nil with
              | Tnil ->
                  mk "obj(%a)" Path.print p |> std_edge t |> numbered tl
              | _ ->
                  mk "obj(#%a)" Path.print p
                  |> edge (labelfr "row variable") rv_or_nil
                  |> numbered tl
          end
        in
        group id (labelfr "Fields") [t] gh
    | Types.Tfield (f, k, t1, t2) ->
        mk "%s<SUP>%s</SUP>" f (string_of_field_kind k)
        |> std_edge t1
        |> edge (make [Style Dotted]) t2
    | Types.Tnil -> mk "∅"
    | Types.Tlink t -> add_tynode (make [Style Dash]) |> std_edge t
    | Types.Tsubst (t, o) ->
        add_tynode (make [Label ["Subst"]; Style Dotted])
        |> numbered (t :: Option.to_list o)
    | Types.Tunivar name ->
        mk "%a<SUP>∀</SUP>" pretty_var name
    | Types.Tpoly (t, tl) ->
        let gh = mk "∀" |> std_edge t in
        List.fold_left (poly_edge ~follow_expansions ~color params id) gh tl
    | Types.Tvariant row ->
        let Row {fields; more; name; fixed; closed} = Types.row_repr row in
        let closed = if closed then "<SUP>closed</SUP>" else "" in
        let gh = match name with
          | None -> mk "[Row%s]" closed
          | Some (p,tl) ->
              mk "[Row %a%s]" Path.print p closed
              |> numbered tl
        in
        let more_lbl = labelfr "%a row variable" Pp.row_fixed fixed in
        let gh = gh |> edge more_lbl more in
        List.fold_left (field ~follow_expansions params id) gh fields
    | Types.Tpackage (p, fl) ->
        let types = List.map snd fl in
        mk "mod %a[%a]"
          Path.print p
          Pp.(list ~sep:semi longident) (List.map fst fl)
        |> numbered types
  and field ~follow_expansions params id0 gh (name,rf)  =
    let id = Index.field ~name id0 in
    let fnode = Node id in
    let color = colorize params (Index.color_id id) in
    let gh = add (field_node color (Some name) rf) fnode gh in
    let gh = add none (Edge(id0,id)) gh in
    field_inside ~follow_expansions params id rf gh
  and field_inside ~follow_expansions params id rf gh =
    Types.match_row_field
      ~absent:(fun () -> gh)
      ~present:(function
          | None -> gh
          | Some arg -> numbered_edges ~follow_expansions params id [arg] gh
        )
      ~either:(fun _ tl _ (cell,e) ->
          let gh = match tl with
            | [] -> gh
            | [x] -> edge ~follow_expansions params id none x gh
            | _ :: _ as tls ->
                let label = make [Label ["⋀"]; filled lightgrey] in
                group (inject_typ ~follow_expansions params) id label tls gh
          in
          match e with
          | None -> gh
          | Some f ->
              let id_ext = Index.field_ext cell in
              let color = colorize params (Index.color_id id_ext) in
              let gh = add (field_node color None f) (Node id_ext) gh in
              let gh = add none (Edge(id,id_ext)) gh in
              field_inside ~follow_expansions params id_ext f gh
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
  Digraph.add ~user:true label node gh

let translate_entries params ts =
  List.fold_left (translate params) (Entity_map.empty, empty_subgraph) ts

let group_nodes sg (label,set) =
  let set = Node_set.inter set sg.nodes in
  if Node_set.cardinal set <= 1 then
    sg
  else
    let nodes = Node_set.diff sg.nodes set in
    let g = { empty_subgraph with nodes = set} in
    let subgraphes =  (label,g) :: sg.subgraphes in
    { sg with nodes; subgraphes }

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

let subgraph_register = ref []
let register_subgraph params ?(label=[filled lightgrey]) tys =
  let add_ty gh ty =
    let _id, gh = Digraph.inject_typ ~follow_expansions:false params ty gh in
    gh
  in
  let gh = Entity_map.empty, empty_subgraph in
  let _g, sg = List.fold_left add_ty gh tys in
  subgraph_register := (make label, sg.nodes) :: !subgraph_register

let forget () =
  node_register := [];
  subgraph_register := []

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
      let g, sg = translate_entries params (ts @ !node_register) in
      let sg = List.fold_left group_nodes sg !subgraph_register in
      Pp.graph ppf (g,sg)
    )

let types ~title params ts =
  nodes ~title params (List.map (fun (lbl,ty) -> lbl, Node ty) ts)
