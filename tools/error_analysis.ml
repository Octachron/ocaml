let report_file = Sys.argv.(1)

module S = Set.Make(String)
module M = Map.Make(String)

let read_file file =
  let f =
    try open_in_gen [Open_creat;Open_rdonly] 1 file with _ ->
      Format.eprintf "Report: opening file failure@."; exit 2 in
  let rec loop prev map =
    match input_line f with
    | exception End_of_file -> begin
        match prev with
        | None -> map
        | Some (key,s) -> M.add key s map
      end
    | "" -> loop prev map
    | s ->
        let kind, loc = s.[0], String.sub s 1 (String.length s - 1) in
        match kind, prev with
        | '+', None -> loop (Some(loc,S.empty)) map
        | '+', Some(ploc,pset) ->
            loop (Some(loc,S.empty)) (M.add ploc pset map)
        | ' ', Some(ploc,pset) ->
            loop (Some(ploc,S.add loc pset)) map
        | _ -> loop prev map in
  let r = loop None M.empty in
  close_in f;
  r

let pp_map ppf map  =
  M.iter (fun key s ->
      Format.fprintf ppf "@[<v 2>[%s]@." key;
      S.iter (Format.fprintf ppf "%s@ ") s;
      Format.fprintf ppf "@]@."
    ) map

let pp_seq ppf set  =
  Format.fprintf ppf "@[<v>";
  Seq.iter (Format.fprintf ppf "%s@ ") set;
  Format.fprintf ppf "@]"

let section s =
  M.fold (fun key set mmap ->
      match String.split_on_char '/' key with
      | x :: q ->
          let map = try M.find x mmap with Not_found -> M.empty in
          M.add x (M.add (String.concat "/" q) set map) mmap
      | _ -> mmap
    ) s M.empty

let coverage map =
  M.fold (fun _ set -> (+) (if S.cardinal set > 0 then 1 else 0)) map 0,
  M.fold (fun _ set -> (+) (S.cardinal set)) map 0,
  M.cardinal map

let diff mmap key =
  let map = M.find key mmap in
  let tested, tests, all = coverage map in
  let seq =
    let untested (k,m) = if S.cardinal m = 0 then Some k else None in
    Seq.filter_map untested (M.to_seq map) in
  Format.printf "[%s]@.[Coverage %.3g%% %d/%d Average %f]@."key
    (100. *. float tested /. float all)
    tested all
    (float tests /. float tested);
  Format.printf "%a@." pp_seq seq

let () =
  let map = read_file report_file in
  let smap = section map in
  M.fold (fun key _ () -> diff smap key) smap ()
