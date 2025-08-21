
type ty =
  | Char
  | Int
  | Float
  | String

type mutation =
  | Duplicate
  | Delete
  | Single
  | Mixture
  | Change_type

type _ Effect.t +=
  | Random_name: string Effect.t
  | Str_mutation: mutation option Effect.t
  | Letter_mutation: bool Effect.t
  | Letter: char Effect.t
  | Type: ty Effect.t


let name () = Effect.perform Random_name
let ty () = Effect.perform Type

module Name_set = Set.Make(String)

let init n = Array.init n (fun _ -> name (), ty())


let mutation () = Effect.perform Str_mutation
let letter () = Effect.perform Letter
let change_letter () = Effect.perform Letter_mutation


let one_letter (s,ty) =
  String.map (fun c ->
      if change_letter () then letter () else c
    ) s, ty

let mixture_name aux l =
  let b = Buffer.create (String.length l) in
  for i = 0 to (max (String.length l) (String.length aux) - 1) do
    if change_letter () && i < String.length aux then
      Buffer.add_char b aux.[i]
    else if i < String.length l then
      Buffer.add_char b l.[i]
  done;
  Buffer.contents b

let mixture (y,yt) (x,xt) =
  (mixture_name x y, xt)

let change_type (n,_) = (n, ty ())

let mutate prev current elt =
  match mutation () with
  | None -> Dynarray.add_last current elt
  | Some m -> match m with
  | Duplicate ->
      Dynarray.add_last current elt;
      Dynarray.add_last current (one_letter elt)
  | Delete -> ()
  | Single -> Dynarray.add_last current @@ one_letter elt
  | Mixture ->
      let aux = Random.int (Array.length prev) in
      Dynarray.add_last current @@ mixture prev.(aux) elt
  | Change_type ->
      Dynarray.add_last current  @@ change_type elt

let one_generation_mutation str =
  let d = Dynarray.create () in
  Array.iter (mutate str d) str;
  Dynarray.to_array d

let rec many n str = if n <= 1 then str else
    many (n-1) @@ one_generation_mutation str

let keywords = Name_set.of_list
    [ "if"; "true"; "false"; "for"; "of"; "to"; "in"; "or"; "done";
    "asr"; "lsl"; "lsr"; "mod"; "asl"; "do"; "as"; "let"; "val"; "end"; "begin";"struct"; "sig";
     "object"; "lazy"; "else"; "then" ;"rec"; "with"; "when"; "try"; "match"; "lor"; "land"; "and";
    "fun"; "function"; "new"; "lxor"; "type"; "module"; "open"; "include"; "exception"; "effect"
    ]

let deduplicate a =
  let rec aux d set pos =
    if pos < 0 then Dynarray.to_array d
    else
      let elt = a.(pos) in
      if Name_set.mem (fst elt) set then
        aux d set (pos-1)
      else
        let () = Dynarray.add_last d elt in
        let set = Name_set.add (fst elt) set in
        aux d set (pos-1)
  in
  aux (Dynarray.create ()) keywords (Array.length a -1)

let gen ~mut ~size =
  let core = init size in
  let str = deduplicate @@ many mut core in
  let sg = deduplicate @@ many mut core in
  str, sg

let value = function
  | Int -> Format.dprintf "0"
  | Float -> Format.dprintf "0."
  | Char -> Format.dprintf "'a'"
  | String -> Format.dprintf {|"str"|}

let pp_ty = function
  | Int -> Format.dprintf "int"
  | Float -> Format.dprintf "float"
  | Char -> Format.dprintf "char"
  | String -> Format.dprintf "string"


let gen_def ppf (name, ty) =
  Format.fprintf ppf "let %s = %t@," name
    (value ty)

let gen_decl ppf (name,ty) =
  Format.fprintf ppf "val %s: %t@," name (pp_ty ty)


let str elt l ppf =
  Format.fprintf ppf "@[<v>%a@]@."
    (fun ppf -> Array.iter (elt ppf)) l

let gen_impl = str gen_def
let gen_intf = str gen_decl

let with_file filename f = Out_channel.with_open_bin filename (fun ch ->
    let ppf = Format.formatter_of_out_channel ch in
    f ppf
  )

let print prefix (impl,sg) =
  with_file (prefix ^".mli") (gen_intf sg);
  with_file (prefix ^".ml") (gen_impl impl)


let uletter () =
  Char.chr @@ Char.code 'a' + Random.int 26

let exp scale =
  2 + Float.to_int (~-. scale *. log (Random.float 1.))

let name () =
  let len = exp 10. in
  String.init len (fun _ -> uletter ())

let continue = Effect.Deep.continue

let mutation rate =
  if Random.float 1. > rate then None else
  match Random.int 10 with
  | 0 -> Some Duplicate
  | 1 -> Some Delete
  | 2 -> Some Mixture
  | 3 -> Some Change_type
  | 4|5|6|7|8|9| _ -> Some Single

let uty () = match Random.int 4 with
  | 0 -> Int
  | 1 -> Char
  | 2 -> Float
  | 3 | _ -> String


let small_change () = Random.float 1. > 0.9

let handler rate f = match f () with
  | effect Random_name, k -> continue k @@ name ()
  | effect Str_mutation, k ->  continue k @@ mutation rate
  | effect Letter_mutation, k -> continue k @@ small_change ()
  | effect Letter, k -> continue k (uletter ())
  | effect Type, k -> continue k (uty ())
  | x -> x

let size = ref 1_000
let mut = ref 10
let name = ref "/tmp/perf_test"
let rate = ref 0.1
let samples = ref 10
let () = Random.self_init ()
let process ~rate ~mut ~size ~name =
  let n_rate = rate /. float size in
  let x = handler n_rate (fun () -> gen ~mut ~size) in
  print name x

let args =
  [ "-size", Arg.Set_int size, "size of the initial module";
    "-samples", Arg.Set_int samples, "number of sample with increaded sizes";
    "-mutate", Arg.Set_int mut, "number of mutation";
    "-name", Arg.Set_string name, "prefix of the generated name";
    "-rate", Arg.Set_float rate, "mutation rate by round";
  ]

let main () =
  Arg.parse args ignore "gen [-name test_name]";
  for sf = 1 to !samples do
    for mf = 1 to !samples do
      let size = sf * !size in
      let mut = mf *  !mut in
      let name = Format.asprintf "%s_mut=%03d_size=%06d" !name mut size in
      process ~mut ~name ~size ~rate:!rate
    done
  done

let () = main ()
