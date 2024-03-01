
(* Print a raw type expression, with sharing *)

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

let (.%()) map id =
  Option.value ~default:none @@ Int_map.find_opt id map

let string_of_field_kind v =
  match Types.field_kind_repr v with
  | Fpublic -> "public"
  | Fabsent -> "absent"
  | Fprivate -> "private"

type entity =
  | Node of Types.type_expr
  | Edge of Types.type_expr * Types.type_expr
  | Hyperedge of Types.type_expr list

let ty_id ppf x =
  let x = Types.Transient_expr.coerce x in
  fprintf ppf "%d" x.id

let pp_id ppf = function
  | Node x -> fprintf ppf "n%a" ty_id  x
  | Edge (x,y) -> fprintf ppf "e%ae%a" ty_id x ty_id y
  | Hyperedge l ->
      let sep ppf () = fprintf ppf "h" in
      fprintf ppf "h%a" Pp.(list ~sep ty_id) l

let rec hyperedges_of_memo ty = function
  | Types.Mnil -> []
  | Types.Mcons (_priv, _p, t1, t2, rem) ->
      (memo, Hyperedge [ty;t1;t2]) :: hyperedges_of_memo ty rem
  | Types.Mlink rem -> hyperedges_of_memo ty !rem


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


let rec typ map visited ppf ty0 =
  let ty = Types.Transient_expr.coerce ty0 in
  if List.memq ty !visited then ()
  else begin
    visited := ty :: !visited;
    node map visited ty0 ty.id ppf ty.desc
  end
and node map visited ty0 id ppf desc =
  let user_label = map.%(id) in
  let node_label, node_meta , sub_entities = split_node ty0 desc in
  let label = asprintf "%t<SUB>%d</SUB>" node_label id in
  let node_label = update (make node_meta) (Label label) in
  let label = merge user_label node_label in
  fprintf ppf "%a%a;@," pp_id (Node ty0) Pp.decorate label;
  List.iter (entity map visited ppf) sub_entities
and split_node ty0 = function
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
    numbered ty0 tl @ hyperedges_of_memo ty0 !abbrevs
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
and edge ppf (lbl,x,y) =
  fprintf ppf "%a->%a%a;@ " pp_id x pp_id y Pp.decorate lbl;
and edges ppf tys=
  List.iter (edge ppf) tys
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
and entity map visited ppf (lbl,entry) = match entry with
  | Node ty -> typ map visited ppf ty
  | Edge (x,y) ->
      typ map visited ppf x;
      typ map visited ppf y;
      edges ppf [lbl, Node x, Node y]
  | Hyperedge l as h->
      hyper ppf h;
      List.iter (typ map visited ppf) l;
      List.iter (fun e ->
          edges ppf [lbl, h, Node e]
        ) l
and hyper ppf h =
  fprintf ppf "%a[shape=\"triangle\"; label=\"\"];@," pp_id h


let add_info map (info,entry) =
  match entry with
  | Edge _ | Hyperedge _ -> map
  | Node ty ->
      let ty = Types.Transient_expr.coerce ty in
      let info = match Int_map.find_opt ty.id map with
        | None -> info
        | Some i -> merge i info
      in
      Int_map.add ty.id info map



let entries ppf ts =
  let visited = ref [] in
  let map = List.fold_left add_info Int_map.empty ts in
  fprintf ppf "@[<v>digraph G {@,";
  List.iter (entity map visited ppf) ts;
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


let global = ref None, pp_print_string
let loc = ref None, compact_loc
let context = [pp global; pp loc]
let dash ppf () = fprintf ppf "-"

let node_register = ref []
let register_node (label,ty) =
  node_register := (make label,Node ty) :: !node_register
let forget () = node_register := []

let dump_to suffix ts =
  incr file_counter;
  let filename =
    match !Clflags.dump_dir with
    | None -> asprintf "%04d-%s.dot"  !file_counter suffix
    | Some d ->
        asprintf "%s%s%04d-%s-%a.dot"
          d Filename.dir_sep
          !file_counter
          suffix
          Pp.(list ~sep:dash (fun ppf pr -> pr ppf)) context
  in
  Out_channel.with_open_bin filename (fun ch ->
      let ppf = Format.formatter_of_out_channel ch in
      let ts = List.map (fun (l,t) -> make l, t) ts in
      entries ppf (ts @ !node_register)
    )

let types suffix ts = dump_to suffix (List.map (fun (lbl,ty) -> lbl, Node ty) ts)
