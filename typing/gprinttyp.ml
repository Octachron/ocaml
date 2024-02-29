
(* Print a raw type expression, with sharing *)

open Format

type edge_label =
  | Arg of int
  | Std
  | Memo_expansion


type node_label =
| Left
| Right
| Important
| Normal

let decorate ppf = function
  | Left -> fprintf ppf {|color="green"|}
  | Right -> fprintf ppf {|color="blue"|}
  | Important -> fprintf ppf "color=\"red\"; "
  | Normal -> ()

module Int_map = Map.Make(Int)

let (.%()) map id =
  Option.value ~default:Normal @@ Int_map.find_opt id map

let add_info map (info,ty) =
  let ty = Types.Transient_expr.coerce ty in
  Int_map.add ty.id info map

let string_of_field_kind v =
  match Types.field_kind_repr v with
  | Fpublic -> "public"
  | Fabsent -> "absent"
  | Fprivate -> "private"

let rec edges_of_memo = function
  | Types.Mnil -> []
  | Types.Mcons (_priv, _p, t1, t2, rem) ->
      (Memo_expansion,t1,t2) :: edges_of_memo rem
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

let semi ppf () = fprintf ppf ";@ "
let list ~sep = pp_print_list ~pp_sep:sep
let rec longident ppf = function
  | Longident.Lident s -> fprintf ppf "%s" s
  | Longident.Ldot (l,s) -> fprintf ppf "%a.%s"  longident l s
  | Longident.Lapply(f,x) -> fprintf ppf "%a(%a)" longident f  longident x

let numbered ty tl = List.mapi (fun i x -> Arg i, ty, x) tl

let rec typ map visited ppf ty0 =
  let ty = Types.Transient_expr.coerce ty0 in
  if List.memq ty !visited then ()
  else begin
    visited := ty :: !visited;
    node map visited ty0 ty.id ppf ty.desc
  end
and node map visited ty0 id ppf desc =
   fprintf ppf "%d[%alabel=<" id decorate map.%(id);
   let sub_edges = node_desc_and_edges ty0 ppf desc in
   fprintf ppf "<SUB>%d</SUB> >];@ " id;
   edges map visited ppf sub_edges
and node_desc_and_edges ty0 ppf = function
| Types.Tvar name -> pretty_var ppf name; []
| Types.Tarrow(l,t1,t2,_) ->
    fprintf ppf "→%a" exponent_of_label l;
    numbered ty0 [t1; t2]
| Types.Ttuple tl -> fprintf ppf ","; numbered ty0 tl
| Types.Tconstr (p,tl,abbrevs) ->
    fprintf ppf "%a" Path.print p;
    numbered ty0 tl @ edges_of_memo !abbrevs
| Types.Tobject (t, name) ->
    fprintf ppf "obj";
     begin match !name with
      | None -> [Std, ty0, t]
      | Some (_p,tl) -> (Std, ty0, t)::numbered ty0 tl
      end
| Types.Tfield (f, k, t1, t2) ->
    fprintf ppf "%s<SUP>%s</SUP>"
      f (string_of_field_kind k);
    numbered ty0 [t1;t2]
| Types.Tnil -> fprintf ppf "∅"; []
| Types.Tlink t -> fprintf ppf "⇒"; [Std, ty0, t]
| Types.Tsubst (t, o) ->
    fprintf ppf "Subst";
    numbered ty0 (t :: Option.to_list o)
| Types.Tunivar name ->
    fprintf ppf "%a<SUP>∀</SUP>" pretty_var name; []
| Types.Tpoly (t, tl) ->
    fprintf ppf "∀";
    (Std, ty0, t) :: (numbered ty0 tl)
| Types.Tvariant row ->
    let Row {fields; more; name; fixed; closed} = Types.row_repr row in
    let args = match name with
    | None ->
        fprintf ppf "[Row(%B,%a,[%a])]" closed row_fixed fixed
          (list ~sep:semi field_node) fields; []
    | Some (p,tl) ->
        fprintf ppf "[Row %a(%B,%a,%a)]"
          Path.print p closed row_fixed fixed
          (list ~sep:semi field_node) fields;
        tl
    in
    numbered ty0 @@ args @ more :: List.concat_map field_edge (List.map snd fields)
| Types.Tpackage (p, fl) ->
    fprintf ppf "mod %a[%a]"
      Path.print p
      (list ~sep:semi longident) (List.map fst fl);
    numbered ty0 (List.map snd fl)
and edge ppf (lbl,x,y) =
  let x = Types.Transient_expr.coerce x in
  let y = Types.Transient_expr.coerce y in
  fprintf ppf "%d->%d" x.id y.id;
  begin match lbl with
  | Std -> ()
  | Arg n -> fprintf ppf "[label=\"%d\"]" n
  | Memo_expansion -> fprintf ppf "[label=\"expand\"; style=\"dashed\"]"
  end;
  fprintf ppf ";@ "
and edges map visited ppf tys=
  List.iter (fun (_,x,y) ->
      typ map visited ppf x;
      typ map visited ppf y)
  tys;
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

let types ppf ts =
  let visited = ref [] in
  let map = List.fold_left add_info Int_map.empty ts in
  let ts = List.map snd ts in
  fprintf ppf "@[<v>digraph G {@,";
  List.iter (typ map visited ppf) ts;
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

let types suffix ts =
  incr file_counter;
  let filename =
    match !Clflags.dump_dir with
    | None -> asprintf "%s-%04d.dot" suffix !file_counter
    | Some d ->
        asprintf "%s%s%s-%04d-%a.dot"
          d Filename.dir_sep suffix !file_counter
          (list ~sep:dash (fun ppf pr -> pr ppf)) context
  in
  Out_channel.with_open_bin filename (fun ch ->
      let ppf = Format.formatter_of_out_channel ch in
      types ppf ts
    )
