
type sync = Seq | Par

type path = int list

type node = { path:path; sync:sync; children: forest; parents:path array }
and forest = node array

let () = Random.init 10

module Pos: sig
  type t
  val exn: int -> t
  val opt: int -> t option
  val rand: t -> int
  val (+): t -> t -> t
  val (-): t -> int -> t option
  val int: t -> int
end = struct
  type t = int
  let opt n = if n > 0 then Some n else None
  let exn n = if n > 0 then n else failwith "Non-positive argument"
  let rand n = if n <= 0 then failwith (Printf.sprintf "positive int error: %d" n) else Random.int n
  let ( + ) = ( + )
  let ( - ) x y = opt (x - y)
  let int x = x
end

let rec drop n l = if n = 0 then l else
    match l with
    | [] -> []
    | a :: q -> drop (n-1) q

let rand_parent d path =
  drop (1 + Pos.rand d) path


let id ppf path =
  Format.fprintf ppf "@[<h>%a@]"
    (Format.pp_print_list ~pp_sep:(fun _ _ -> ()) (fun ppf -> Format.fprintf ppf "%d")) path


let name ppf path =
  Format.fprintf ppf "Unit%a" id path

let rand_parents ~ncalls ~depth path =
  Array.init (1 + Pos.rand ncalls) (fun _ -> rand_parent depth path)

let leaf ~ncalls ~depth ~domains path  =
  let sync = if domains >= 1 then Par else Seq in
  let parents = rand_parents ~ncalls ~depth path in
  { path = 0::path; sync; children = [||]; parents = parents }


let split bins n =
  match Pos.opt n with
  | None -> Array.init bins (fun _ -> 0)
  | Some p ->
    let seps = Array.init (1+bins) (fun _ -> Pos.(rand (p+ exn 1))) in
    let () = Array.sort Stdlib.compare seps in
    seps.(0) <- 0;
    seps.(bins) <- n;
    Array.init bins (fun i -> seps.(i+1) - seps.(i))

let spawn_or_not ~current_depth ~domains ~depth = match Pos.int current_depth with
  | 1 -> domains - 1, Par
  | _ ->
      if domains = 0 then 0, Seq
      else if domains >= Pos.int depth || Pos.rand depth = 0 then
        domains - 1, Par
      else domains, Seq


let rec rand path ~ncalls ~current_depth ~domains ~width ~depth =
  match depth, width <= 0 with
  | None, _ | _, true ->
    leaf ~depth:current_depth ~domains ~ncalls path
  | Some depth, false ->
      let bins =
        match Pos.opt (min width 4) with
        | None -> 1
        | Some p -> 1 + Pos.rand p
      in
    branch path
      ~ncalls
      ~current_depth
      ~width
      ~domains
      ~depth
      ~bins
and branch path ~ncalls ~current_depth ~domains ~width ~depth ~bins =
  let number_of_grandchilds = split bins width in
  let domains, sync = spawn_or_not ~current_depth ~depth ~domains in
  let number_of_domains = split bins domains in
  let children =
    Array.init bins (fun n ->
        rand (n::path)
          ~ncalls
          ~domains:(number_of_domains.(n))
          ~current_depth:Pos.(current_depth+ exn 1)
          ~width:(number_of_grandchilds.(n))
          ~depth:Pos.(depth-1)
      )
  in
  let parents = rand_parents ~ncalls ~depth path in
  { path; children; sync; parents }


let start  ~ncalls ~domains ~width ~depth ~childs =
  let number_of_grandchilds = split childs width in
  let children =
    Array.init childs (fun n ->
        rand [n]
          ~ncalls
          ~domains:2
          ~current_depth:(Pos.exn 1)
          ~width:(number_of_grandchilds.(n))
          ~depth:Pos.(depth-1)
      )
  in
  let parents = [||] in
  { path=[]; children; sync=Seq; parents }



let seq ppf target =
  Format.fprintf ppf
    {|@[<h>let () = Dynlink.loadfile @@@@ Dynlink.adapt_filename "%a.cmo"@]@.|}
    name target

let par_create ppf target =
  Format.fprintf ppf
    {|@[<h>let d%a = Domain.spawn (fun () -> Dynlink.loadfile @@@@ Dynlink.adapt_filename "%a.cmo")@]@.|}
    id target name target

let add ppf parent = match parent with
  | [] -> Format.fprintf ppf "Store.add"
  | p -> Format.fprintf ppf "%a.add" name p

let register ppf parent current =
  Format.fprintf ppf
    {|@[<h>let () = %a "%a->%a"|}
    add parent
    id current
    id parent

let new_add ppf node =
  let path =
    let n = Array.length node.parents in
    match Pos.opt n with
    | None -> []
    | Some p -> node.parents.(Pos.rand p)
  in
  Format.fprintf ppf
    "let add x = %a x@." add path

let par_join ppf target =
    Format.fprintf ppf
    {|@[<h>let () = Domain.join d%a@]@.|}
    id target

type action = Parent of path | Child of node


let shuffle a =
  let a = Array.copy a in
  for i = Array.length a - 1 downto 1 do
    let pos = Random.int i in
    let tmp = a.(i) in
    a.(i) <- a.(pos);
    a.(pos) <- tmp
  done;
  a

let schedule node =
  let children =
    Array.map (fun i -> Child i) node.children
  in
  let parents = Array.map (fun i -> Parent i) node.parents in
  shuffle @@ Array.concat [children;parents]


let write_action self ppf = function
  | Parent p ->
      register ppf p self.path
  | Child n ->
      match n.sync with
      | Seq -> seq ppf n.path
      | Par -> par_create ppf n.path

let join_action ppf ch = match ch.sync with
  | Seq -> ()
  | Par -> par_join ppf ch.path

let write_node ppf node =
  let schedule = schedule node in
  Array.iter (write_action node ppf) schedule;
  Array.iter (join_action ppf) (shuffle node.children);
  new_add ppf node;
  Format.pp_print_flush ppf ()


let to_file name f x =
  let ch = open_out name in
  let ppf = Format.formatter_of_out_channel ch in
  f ppf x;
  close_out ch


let gen_node node =
  to_file (Format.asprintf "@[<h>%a.ml@]" name node.path)
  write_node node


let rec fold f acc node =
  let acc = f acc node in
  fold_forest f acc node.children
and fold_forest f acc nodes = Array.fold_left (fold f) acc nodes

let rec iter f node =
  f node;
  iter_forest f node.children
and iter_forest f nodes = Array.iter (iter f) nodes

let gen_files node = iter gen_node node

let files ppf node =
  iter (fun node -> Format.fprintf ppf " @[<h>%a.ml@]" name node.path) node


let registred ppf node =
  iter (fun node ->
      Array.iter (fun p ->
          Format.fprintf ppf {|"%a->%a";|} id node.path id p
        )
        node.parents
    )
    node


let repeat n ppf fmt =
  for i = 1 to n do
    Format.fprintf ppf fmt
  done

let task ppf n = repeat n ppf "*"

let main_header ppf node =
  Format.fprintf ppf {|(* TEST

include dynlink
libraries = ""
readonly_files = "@[<h>store.ml main.ml%a@]"

* shared-libraries
** setup-ocamlc.byte-build-env
*** ocamlc.byte
module = "store.ml"@.|}
    files node;
  let bytecode_compilation i node =
    Format.fprintf ppf
      {|@[<v>%a ocamlc.byte@,module = "%a.ml"@,@]|}
      task i name node.path;
    i + 1
  in
  let stars = fold bytecode_compilation 4 node in
  Format.fprintf ppf
    "@[<v>%a ocamlc.byte@ \
     module = \"main.ml\"@ \
     %a ocamlc.byte@ \
     program = \"./run.byte.exe\"@ \
     libraries= \"dynlink\"@ \
     all_modules = \"store.cmo main.cmo\"@ \
     module = \"\" @ \
     %a run@ \
     %a check-program-output@ \
     @ \
@]"
    task stars
    task (1 + stars)
    task (2 + stars)
    task (3 + stars)
  ;
  let native_compilation i node =
    Format.fprintf ppf
      "@[<v>%a ocamlopt.byte@ \
       flags = \"-shared\"@ \
       program= \"%a.cmxs\"@ \
       modules = \"\"@ \
       all_modules = \"%a.ml\"@ \
      @]"
      task i
      name node.path
      name node.path;
    i + 1
  in
  Format.fprintf ppf
    "@[<v>** native-dynlink@ \
     *** setup-ocamlopt.byte-build-env@ \
     **** ocamlopt.byte@ \
     flags = \"\"@ \
     module = \"store.ml\"@ \
     @]";
  let stars = fold native_compilation 5 node in
  Format.fprintf ppf
    "@[<v>\
     %a ocamlopt.byte@ \
     flags = \"\"@ \
     module = \"main.ml\"@ \
     %a ocamlopt.byte@ \
     program = \"./main.exe\"@ \
     libraries=\"dynlink\"@ \
     all_modules = \"store.cmx main.cmx\"@ \
     module = \"\"@ \
     %a run@ \
     %a check-program-output@ \
    *)@ \
     @]"
    task stars
    task (1 + stars)
    task (2 + stars)
    task (3 + stars)

let expected ppf nodes =
  Format.fprintf ppf
    "@[<v>[%a]@]"
    registred nodes


let check ppf node =
  Format.fprintf ppf
    "@[<v>@ \
     module String_set = Set.Make(String)@ \
     let stored = Atomic.get Store.store@ \
     let stored_set = String_set.off_list stored@ \
     %a@ \
     let expected = String_set.of_list %a@ \
     let () =@ \
       let () = List.iter (Printf.printf \"%s\n\") (String_set.elements stored_set) in@ \
       assert (String_set.equal stored_set expected)
@]"
    expected node

let main_file ppf node =
  main_header ppf node;
  write_node ppf node;
  check ppf node

let () =
  let domains = 4 in
  let tree = start ~ncalls:(Pos.exn 5) ~depth:(Pos.exn 1) ~domains ~width:10 ~childs:(domains/2) in
  to_file "main.ml" main_file tree;
  gen_files tree
