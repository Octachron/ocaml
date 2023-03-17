
type version = { major:int; minor:int }
type version_range = { introduction: version; deprecation: version option }
let first_version = { major = 0; minor = 0 }

type doc = Format.formatter -> unit

module Keys = Map.Make(struct type t = string list let compare = compare end)

type _ extension = ..

type 'a printer =
  | Int: int printer
  | String: string printer
  | Doc: doc printer
  | List: 'a printer -> 'a list printer
  | Pair: 'a printer * 'b printer -> ('a * 'b) printer
  | Triple: 'a printer * 'b printer * 'c printer -> ('a * 'b * 'c) printer
  | Quadruple: 'a printer * 'b printer * 'c printer * 'd printer ->
      ('a * 'b * 'c * 'd) printer
  | Custom: { id :'b extension; pull: ('b -> 'a); default: 'a printer} ->
      'b printer
  | Sublog: 'id log_scheme -> 'id log printer

and ('a,'b) key = { path: string list; typ: 'a printer }
and key_metadata =
    Key_metadata:
      { typ: 'a printer;
        version:version;
        infinitesimal_version:int;
        deprecation: version option } ->
      key_metadata
and 'a log_scheme = {
  mutable version: version;
  mutable infinitesimal_version:int;
  mutable keys: key_metadata Keys.t
}
and 'a log = {
  device: device;
  version: version;
  scheme: 'a log_scheme;
}
and device = {
  print: 'a. key:string list -> 'a printer -> 'a -> unit;
  sub: key:string list -> device;
  flush: unit -> unit
}


module New_scheme() = struct
  type id
  let init () =
    { version = first_version;
      infinitesimal_version = 0;
      keys = Keys.empty
    }
end

let flush log = log.device.flush ()

type error =
  | Duplicate_key of string list
  | Deprecation_non_existing_key of string list
  | Time_travel of version * version
exception Error of error
let error e = raise (Error e)


let (.!()<-) scheme key metadata =
  scheme.keys <- Keys.add key.path metadata scheme.keys

let new_key ~path scheme typ =
  if Keys.mem path scheme.keys then error (Duplicate_key path);
  scheme.infinitesimal_version <- succ scheme.infinitesimal_version;
  let metadata = Key_metadata {
    version=scheme.version;
    infinitesimal_version = scheme.infinitesimal_version;
    deprecation=None;
    typ
  }
  in
  let key = { path; typ } in
  scheme.!(key) <- metadata;
  key

let (.!()) scheme key =
  match Keys.find_opt key.path scheme.keys with
  | Some x -> x
  | None -> assert false

let deprecate_key key scheme =
  let Key_metadata r = scheme.!(key) in
  scheme.!(key) <- Key_metadata { r with deprecation = Some scheme.version }

(** {1:log_scheme_versionning  Current version of the log } *)

let version scheme = scheme.version

let name_version scheme version =
  if version <= scheme.version then error (Time_travel (version, scheme.version))
let version_range key scheme =
  let Key_metadata r =  scheme.!(key) in
  { introduction = r.version; deprecation = r.deprecation }

(** {1:log_creation }*)

let create device version scheme = { device; version; scheme }
let detach key log = log.device.sub ~key

type extension_printer =
  { extension: 'b. 'b extension -> (Format.formatter -> 'b -> unit) option}

let rec fmt_print : type a. extension_printer -> key:string list -> a printer -> Format.formatter -> a -> unit =
  fun {extension} ~key printer ppf x ->
  match printer with
  | Int -> Format.pp_print_int ppf x
  | String -> Format.pp_print_string ppf x
  | Doc -> x ppf
  | Pair (a,b) ->
      let x, y = x in
      Format.fprintf ppf "(%a,@ %a)"
        (fmt_print {extension} ~key a) x
        (fmt_print {extension} ~key b) y
  | Triple (a,b,c) ->
      let x, y, z = x in
      Format.fprintf ppf "(%a,@ %a,@ %a)"
        (fmt_print {extension} ~key a) x
        (fmt_print {extension} ~key b) y
        (fmt_print {extension} ~key c) z
  | Quadruple (a,b,c,d) ->
      let x, y, z ,w = x in
      Format.fprintf ppf "(%a,@ %a,@ %a,@ %a)"
        (fmt_print {extension} ~key a) x
        (fmt_print {extension} ~key b) y
        (fmt_print {extension} ~key c) z
        (fmt_print {extension} ~key d) w
  | Custom {pull; default; id } -> begin
      match extension id with
      | Some pr -> pr ppf x
      | None -> fmt_print {extension} ~key default ppf (pull x)
      end
  |  List elt ->
      Format.pp_print_list (fmt_print {extension} ~key elt) ppf x
  | Sublog _ -> ()


let rec fmt ext ppf = {
  flush = Format.pp_print_newline ppf;
  sub = (fun ~key:_ -> fmt ext ppf);
  print = (fun ~key ty x -> fmt_print ext ~key ty ppf x)
}


let set key x log =
  log.device.print ~key:key.path key.typ x

let (.%[]<-) log key x = set key x log

let fmt key log fmt =
  Format.kasprintf (fun s -> log.%[key] <- s ) fmt
