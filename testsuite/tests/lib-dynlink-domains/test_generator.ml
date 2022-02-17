type path = int list
type topdown_path = Topdown of int list
let rev p = Topdown (List.rev p)

module Pp = struct
  let int ppf d = Format.fprintf ppf "%d" d
  let list ~sep p ppf x = Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf sep) p ppf x
end


let id ppf path =
  Format.fprintf ppf "@[<h>%a@]" Pp.(list ~sep:"_" int) path

let name ppf path =
  Format.fprintf ppf "Plugin_%a" id (List.rev path)

type schedule_atom =
  | Seq_action
  | Register_action
  | Intron_action
  | Define_action
  | Domain_action of int

type 'a normalized_schedule_atom =
  | Seq_point of int
  | Register_point of 'a
  | Intron_point of int * 'a option
  | Define_point of 'a
  | Domain_open of int
  | Domain_close of int

type 'a schedule = 'a normalized_schedule_atom array

type 'a node = { path:path; children: 'a forest; schedule:'a schedule }
and 'a forest = 'a node array


module Int_map = Map.Make(Int)
module Int_set = Set.Make(Int)
module String_set = Set.Make(String)


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


module Rand = struct
  let shuffle_in_place a =
    for i = Array.length a - 1 downto 1 do
      let pos = Random.int i in
      let tmp = a.(i) in
      a.(i) <- a.(pos);
      a.(pos) <- tmp
    done

  let shuffle a =
    let a = Array.copy a in
    shuffle_in_place a;
    a

  let schedule ~calls ~seq ~par ~introns =
    let total = calls + seq + 2 * par + introns + 1 in
    let a = Array.make total Seq_action in
    for i = seq to seq + par - 1 do
      a.(i) <- Domain_action i;
      a.(par + i) <- Domain_action i
    done;
    for i = seq + 2 * par to seq + 2 * par + calls - 1 do
      a.(i) <- Register_action;
    done;
    for i = seq + 2 * par + calls to seq + 2 * par + calls + introns - 1 do
      a.(i) <- Intron_action
    done;
    a.(calls + seq + 2 * par + introns) <- Define_action;
    shuffle_in_place a;
    a

  let split bins n =
    match Pos.opt n with
    | None -> Array.init bins (fun _ -> 0)
    | Some p ->
        let seps = Array.init (1+bins) (fun _ -> Pos.(rand (p+ exn 1))) in
        let () = Array.sort Stdlib.compare seps in
        seps.(0) <- 0;
        seps.(bins) <- n;
        Array.init bins (fun i -> seps.(i+1) - seps.(i))

  let spawn_or_not ~current_depth ~depth domains =
    if domains = 0 then 0, 0
    else if domains >= Pos.int depth || Pos.rand depth = 0 then
      1, domains - 1
    else 0, domains

  let rec number_of_domains ~current_depth ~depth ~spawned ~domains k =
    if k = 0 then domains, spawned
    else
      let s, domains = spawn_or_not ~current_depth ~depth domains in
      number_of_domains ~current_depth ~depth ~spawned:(spawned +s) ~domains (k-1)

  let normalized_schedule ~calls ~seq ~par ~introns =
    let raw_schedule = schedule ~calls ~seq ~par ~introns in
    let update (res, creation_pos, domain_pos) = function
      | Intron_action ->
          Intron_point (Random.int 2,None) :: res,
          creation_pos,
          domain_pos
      | Define_action ->
          Define_point () :: res,
          creation_pos,
          domain_pos
      | Register_action ->
          Register_point () :: res, creation_pos, domain_pos
      | Seq_action ->
          Seq_point creation_pos::res,
          1 + creation_pos,
          domain_pos
      | Domain_action n ->
          match Int_map.find n domain_pos with
          | exception Not_found ->
              Domain_open creation_pos :: res,
              1 + creation_pos,
              Int_map.add n creation_pos domain_pos
          | domain_creation_pos ->
              Domain_close domain_creation_pos :: res,
              creation_pos, domain_pos
    in
    let l, cp, _ = Array.fold_left update ([],0,Int_map.empty) raw_schedule in
    Array.of_list @@ List.rev l

  let rec node_at_path tree = function
    | Topdown [] -> tree
    | Topdown (x :: path) ->
        node_at_path tree.children.(x) (Topdown path)

  module Immutable_array = struct
    type 'a t = { cardinal:int; get: int -> 'a }
    let (.![]) a n = a.get n

    let rand x = x.![Random.int x.cardinal]

    let concat a =
      let n = Array.length a in
      let cardinals =
        let cs = Array.make (n+1) 0 in
        Array.iteri (fun i x -> if i < n then cs.(i+1) <- cs.(i) + x.cardinal) a;
        cs
      in
      let cardinal = cardinals.(n) in
      let rec pick a ~c_low ~pos n =
        let c = cardinals.(pos+1) in
        if n < c  then
          a.(pos).![n - c_low]
        else pick a ~c_low:c ~pos:(pos+1) n
      in
      { cardinal; get = pick a ~c_low:0 ~pos:0}

    let append x y =
      let cardinal = x.cardinal + y.cardinal in
      let get n = if n >= x.cardinal then y.![n-x.cardinal] else x.![n] in
      { cardinal; get }

    let singleton x = { cardinal = 1; get = fun _ -> x }
  end

  let rec path from node =
    let children = Array.mapi (fun i -> path (i::from)) node.children in
    Immutable_array.(concat (Array.append [|singleton from|] children))

  type path_filter = { strictly_older: int array; pos: int; sub_filter: path_filter option }

  let rec filtred_path filter from node =
    let strictly_older = Array.init (Array.length filter.strictly_older) (fun i -> path (i::from) node.children.(i)) in
    let lower_paths = match filter.sub_filter with
      | None -> [||]
      | Some sub_filter ->
          [|filtred_path sub_filter (filter.pos::from) node.children.(filter.pos)|]
    in
    Immutable_array.concat (Array.append lower_paths strictly_older )

  let older_siblings schedule pos =
    let l = Array.length schedule in
    let rec find sched_pos child_pos loaded_plugins =
      if 1 + child_pos >= pos || sched_pos >= l  then loaded_plugins else
        match schedule.(sched_pos) with
        | Register_point _ | Intron_point _ | Define_point _ ->
            find (1+sched_pos) child_pos loaded_plugins
        | Seq_point p ->
            find (1+sched_pos) (1+child_pos) (Int_set.add p loaded_plugins)
        | Domain_open i ->
            find (1+sched_pos) (1+child_pos) loaded_plugins
        | Domain_close i ->
            find (1+ sched_pos) child_pos (Int_set.add i loaded_plugins)
    in
    find 0 0 Int_set.empty

let rec path_filter (Topdown remaining_path) node = match remaining_path with
  | [] -> assert false
  | pos :: rpath ->
      let strictly_older = Array.of_list @@ Int_set.elements @@ older_siblings node.schedule pos in
      let sub_filter =  match rpath with
        | [] -> None
        | _ :: _  as rpath -> Some (path_filter (Topdown rpath) node.children.(pos))
      in
      { strictly_older; pos; sub_filter }

  let anterior_plugin tree = function
    | [] -> Immutable_array.singleton []
    | path ->
        let path_filter = path_filter (rev path) tree in
        Immutable_array.append
          (Immutable_array.singleton [])
          (filtred_path path_filter [] tree)

  let rec map = fun f node ->
    let schedule = f node.path node.schedule in
    { node with schedule; children = map_forest f node.children }
  and map_forest f nodes = Array.map (map f) nodes

  let links tree =
    let edit_schedule path = function
      | Register_point () -> Register_point (Immutable_array.rand @@ anterior_plugin tree path)
      | Define_point () -> Define_point (Immutable_array.rand @@ anterior_plugin tree path)
      | Intron_point (variant,_) ->
          let plugins = anterior_plugin tree path in
          let rec sample ntry =
            if ntry = 0 then Intron_point (Random.int 2, None) else
            let plugin = Immutable_array.rand plugins in
            if plugin = [] then sample (ntry - 1) else
            let linked = node_at_path tree (rev plugin) in
            let same_variant = function Intron_point (n, _ ) -> n = variant | _ -> false in
            if Array.exists same_variant linked.schedule then
              Intron_point (variant, Some plugin)
            else sample (ntry - 1)
          in
          sample 5
      | Domain_close _ | Domain_open _ | Seq_point _ as x -> x
    in
    let edit path schedule = Array.map (edit_schedule path) schedule in
    map edit tree

  let leaf path =
    { path = path; children = [||]; schedule = [|Register_point (); Define_point ()|] }


  let rec node path ~introns ~ncalls ~current_depth ~domains ~width ~depth =
    match depth, width <= 0 with
    | None, _ | _, true ->
        leaf path
    | Some depth, false ->
        let bins =
          match Pos.opt (min width 4) with
          | None -> 1
          | Some p -> 1 + Pos.rand p
        in
        nodes path
          ~ncalls
          ~introns
          ~current_depth
          ~width
          ~domains
          ~depth
          ~bins
  and nodes path ~introns ~ncalls ~current_depth ~domains ~width ~depth ~bins =
    let number_of_grandchilds = split bins width in
    let available_domains = min domains bins in
    let par, _ = number_of_domains ~current_depth ~depth ~domains:available_domains ~spawned:0 2 in
    let remaining_domains = domains - par in
    let number_of_domains = split bins remaining_domains in
    let children =
      Array.init bins (fun n ->
          node (n::path)
            ~ncalls
            ~introns
            ~domains:(number_of_domains.(n))
            ~current_depth:Pos.(current_depth + exn 1)
            ~width:(number_of_grandchilds.(n))
            ~depth:Pos.(depth-1)
        )
    in
    let schedule = normalized_schedule ~calls:(Pos.int ncalls) ~par ~seq:(bins-par) ~introns in
    { path; children; schedule }


end

let start  ~ncalls ~domains ~width ~depth ~introns ~childs =
  let number_of_grandchilds = Rand.split childs width in
  let children =
    Array.init childs (fun n ->
        Rand.node [n]
          ~ncalls
          ~domains:(domains/childs - 1)
          ~current_depth:(Pos.exn 1)
          ~width:(number_of_grandchilds.(n))
          ~depth:Pos.(depth-1)
          ~introns
      )
  in
  let schedule = Rand.normalized_schedule ~par:childs ~seq:0 ~calls:0 ~introns in
  Rand.links { path=[]; children; schedule }

module Synthesis = struct
  let seq ppf target =
    Format.fprintf ppf
      {|@[<h>let () = Dynlink.loadfile @@@@ Dynlink.adapt_filename "%a.cmo"@]@,|}
      name target

  let par_create ppf target =
    Format.fprintf ppf
      {|@[<h>let d%a = Domain.spawn (fun () -> Dynlink.loadfile @@@@ Dynlink.adapt_filename "%a.cmo")@]@,|}
      id target name target

  let add ppf parent = match parent with
    | [] -> Format.fprintf ppf "Store.add"
    | p -> Format.fprintf ppf "%a.add" name p

  let register ppf ~parent ~current =
    Format.fprintf ppf
      {|@[<h>let () = %a "[%a]->[%a]"@]@,|}
      add parent
      id current
      id parent

  let new_add ppf path =
    Format.fprintf ppf
      "let add x = %a x@," add path

  let par_join ppf target =
    Format.fprintf ppf
      {|@[<h>let () = Domain.join d%a@]@,|}
      id target

  let intron ppf variant plugin_opt path  =
    let maybe_wordy ppf = function
      | None -> Format.fprintf ppf {|"That's all"|}
      | Some plugin ->
          Format.fprintf ppf "@[<h>%a.wordy@]" name plugin
    in
    let maybe_start ppf = function
      | None -> Format.fprintf ppf "%h"  (Random.float 5.)
      | Some plugin ->
        Format.fprintf ppf "@[<h>%a.sqrt2@]" name plugin
    in
    let code = match variant with
      | 0 ->
          Format.asprintf
            {|let wordy = "This" ^ "is" ^ "a" ^ "very" ^ "useful" ^ "code" ^ "fragment: %d." ^ %a |}
            (Random.int 1000) maybe_wordy plugin_opt;
      | _ ->
          Format.asprintf
{|let sqrt2 =
  let rec find c =
    if Float.abs (c *. c -. 2.) < 1e-3 then c
    else find ((c *. c +. 2.) /. (2. *. c))
  in find %a|}
maybe_start plugin_opt
    in
    Format.fprintf ppf "%s@ " code

  let write_action node ppf = function
    | Register_point parent ->
        register ppf ~parent ~current:node.path
    | Domain_open n ->
        par_create ppf node.children.(n).path
    | Domain_close n ->
        par_join ppf node.children.(n).path
    | Define_point p ->
        new_add ppf p
    | Seq_point n ->
        seq ppf node.children.(n).path
    | Intron_point (n,x) ->
        intron ppf n x node.path

  let write_node ppf node =
    Format.fprintf ppf "@[<v>";
    Array.iter (write_action node ppf) node.schedule;
    Format.fprintf ppf "@]"


  let to_file name f x =
    let ch = open_out name in
    let ppf = Format.formatter_of_out_channel ch in
    f ppf x;
    Format.pp_print_flush ppf ();
    close_out ch


  let gen_node node =
    if node.path = [] then ()
    else
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



  let plugins node = iter gen_node node

  let files ppf node =
    iter (fun node -> if node.path = [] then () else Format.fprintf ppf " @[<h>%a.ml@]" name node.path) node


  let registred ~quote ~sep ppf node =
    let pairs =
      fold (fun set node ->
          Array.fold_left (fun set -> function
              | Seq_point _ | Domain_open _ | Domain_close _ | Intron_point _ | Define_point _  -> set
              | Register_point p  ->
                  let link = Format.asprintf "[%a]->[%a]" id node.path id p  in
                  String_set.add link set
            )
            set node.schedule
        ) String_set.empty node
    in
    String_set.iter (fun link ->
        Format.fprintf ppf {|@[<h>%s%s%s@]%(%)|}  quote link quote sep
      ) pairs


  let repeat n ppf fmt =
    for i = 1 to n do
      Format.fprintf ppf fmt
    done

  let task ppf n = repeat n ppf "*"

  let main_header ppf node =
    Format.fprintf ppf {|@[<v>(* TEST

include dynlink
libraries = ""
readonly_files = "@[<h>store.ml main.ml%a@]"

* shared-libraries
** setup-ocamlc.byte-build-env
*** ocamlc.byte
module = "store.ml"@ @]|}
      files node;
    let bytecode_compilation i node =
      if node.path = [] then i else begin
      Format.fprintf ppf
        {|@[<v>%a ocamlc.byte@,module = "%a.ml"@,@]|}
        task i name node.path;
      i + 1
      end
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
      if node.path = [] then i else begin
      Format.fprintf ppf
        "@[<v>%a ocamlopt.byte@ \
         flags = \"-shared\"@ \
         program= \"%a.cmxs\"@ \
         module = \"\"@ \
         all_modules = \"%a.ml\"@ \
         @]"
        task i
        name node.path
        name node.path;
      i + 1
      end
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
      (registred ~quote:{|"|} ~sep:";@ ") nodes

  let reference file nodes =
    to_file file (fun ppf ->
        Format.fprintf ppf "@[<v>%a@]@?" (registred ~quote:"" ~sep:"@,")
      ) nodes

  let full_check ppf node =
    Format.fprintf ppf
      "@[<v>@ \
       module String_set = Set.Make(String)@ \
       let stored = Atomic.get Store.store@ \
       let stored_set = String_set.of_list stored@ \
       @[<b>let expected =@ String_set.of_list@ %a@]@ \
       @[<v 2>let () =@ \
       let () = @[<h>List.iter (Printf.printf \"%%s\\n\") (String_set.elements stored_set) in@]@ \
       assert (String_set.equal stored_set expected)\
       @]\
       @]"
      expected node

  let print_result ppf =
    Format.fprintf ppf
      "@[<v>@ \
       module String_set = Set.Make(String)@ \
       let stored = Atomic.get Store.store@ \
       let stored_set = String_set.of_list stored@ \
       @[<v 2>let () =@ \
       @[<h>List.iter (Printf.printf \"%%s\\n\") (String_set.elements stored_set)@]@ \
       @]\
       @]"


  let main ~seed ~width ~depth ~nlinks ~introns ~childs ~domains ppf node =
    Format.fprintf ppf
      "@[<v>%a@ \
      (*  This module and all plugin modules are generated by a call to test_generator.ml with parameters:@ \
      seed=%d, width=%d, depth=%d, nlinks=%d, introns=%d, childs=%d, domains=%d.@ *)@ \
       (* Link plugins *)@ \
       %a@ \
       (* Print result *)@ \
       %t@]@."
      main_header node
      seed width depth nlinks introns childs domains
      write_node node
      print_result
end

let seed = ref 10
let nlinks  =ref 2
let depth = ref 4
let domains = ref 16
let childs: int option ref = ref None
let width = ref 10
let introns = ref 8

let args =
  [ "-nlinks", Arg.Int ((:=) nlinks), "<int> number of calls to older plugins";
    "-depth", Arg.Int ((:=) depth), "<int> number of plugin layers";
    "-domains", Arg.Int ((:=) domains), "<int> number of domain spawned (in total)";
    "-childs", Arg.Int (fun n -> childs := Some n), "<int> number of domain spawned in main program";
    "-width", Arg.Int ((:=) width), "<int> maximum number of unrelated plugins active at the same time";
    "-introns", Arg.Int ((:=) introns), "<int> number of random unrelated code fragments inserted";
    "-seed", Arg.Int ((:=) seed), "<int> PRNG seed"
  ]

let () =
  let () = Arg.parse (Arg.align args) ignore
      "generator -width=<w> -depth=<d> -domains=<dn> -nlinks=<l> generate a test for a random plugin tree \
       with depth <d>, width <w>, with <dn> domain spawned, and where each plugin calls <l> functions from older plugins"
  in
  let seed = !seed in
  Random.init seed;
  let nlinks = !nlinks and depth = !depth and width = !width and domains = !domains and introns = !introns in
  let childs = match !childs with
    | None -> domains/2
    | Some c -> c
  in
  let tree = start
      ~ncalls:(Pos.exn nlinks)
      ~depth:(Pos.exn depth)
      ~domains
      ~width
      ~childs
      ~introns
  in
  Synthesis.to_file "main.ml"
    (Synthesis.main ~nlinks ~width ~introns ~depth ~domains ~childs ~seed)
       tree;
  Synthesis.reference "main.reference" tree;
  Synthesis.plugins tree
