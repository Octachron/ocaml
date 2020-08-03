(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Florian Angeletti, Inria                             *)
(*                                                                        *)
(*   Copyright 2020 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

let failf x =
  let flush_and_exit ppf =  Format.fprintf ppf "@."; exit 2 in
  Format.(kfprintf flush_and_exit err_formatter) x

type release_kind =
  | Alpha
  | Beta
  | RC
  | Production

type variant_flag =
  | Flambda
  | Afl
  | Fp

let pp_release_codename ppf = function
  | Alpha -> Format.fprintf ppf "+alpha"
  | Beta -> Format.fprintf ppf "+beta"
  | RC -> Format.fprintf ppf "+rc"
  | Production -> ()


let pp_variant_flag_intro ppf = function
  | Flambda -> Format.fprintf ppf "flambda activated"
  | Afl -> Format.fprintf ppf "afl-fuzz instrumentation"
  | Fp -> Format.fprintf ppf "frame-pointers"


let pp_variant_intro ppf = function
  | [] -> ()
  | l ->
      let and' ppf () = Format.fprintf ppf " and " in
      Format.fprintf ppf ", with %a"
        Format.(pp_print_list ~pp_sep:and' pp_variant_flag_intro) l

let variant_flag_codename ppf = function
  | Flambda -> Format.fprintf ppf "+flambda"
  | Afl -> Format.fprintf ppf "+afl"
  | Fp -> Format.fprintf ppf "+fp"

let variant_codename ppf = List.iter (variant_flag_codename ppf)

let variant_flag_configure_option ppf = function
  | Flambda -> Format.fprintf ppf {|@ "--enable-flambda"|}
  | Fp -> Format.fprintf ppf {|@ "--enable-frame-pointers"|}
  | Afl -> Format.fprintf ppf {|@ "--with-afl"@ "--disable-debug-runtime"|}

let variant_configure_option ppf = List.iter (variant_flag_configure_option ppf)

let pp_release_name ppf = function
  | Alpha -> Format.fprintf ppf "alpha"
  | Beta -> Format.fprintf ppf "beta"
  | RC -> Format.fprintf ppf "release candidate"
  | Production -> ()
type release = { kind:release_kind; iteration:int }



type core_version = { major:int; minor:int; patchlevel:int }

type full_version =
  { core:core_version; release: release; variant: variant_flag list }

let pp_core_version ppf v =
  Format.fprintf ppf "%d.%02d.%d" v.major v.minor v.patchlevel


let pp_version ppf fv =
  Format.fprintf ppf "%a%a%d"
    pp_core_version fv.core
    pp_release_codename fv.release.kind
    fv.release.iteration

let pp_release_codename ppf fv =
  Format.fprintf ppf "%a%a"
    pp_version fv
    variant_codename fv.variant


let pp_intro ppf release =
  let ordinal ppf iteration = match iteration with
  | 0 -> Format.fprintf ppf "Zeroth"
  | 1 -> Format.fprintf ppf "First"
  | 2 -> Format.fprintf ppf "Second"
  | 3 -> Format.fprintf ppf "Third"
  | 4 -> Format.fprintf ppf "Fourth"
  | 5 -> Format.fprintf ppf "Fifth"
  | 6 -> Format.fprintf ppf "Sixth"
  | 7 -> Format.fprintf ppf "Seventh"
  | 8 -> Format.fprintf ppf "Eigth"
  | 9 -> Format.fprintf ppf "Nineth"
  | n -> Format.fprintf ppf "%d-th release" n in
  Format.fprintf ppf "%a %a for"
    ordinal release.iteration
    pp_release_name release.kind



type checksum =
  | Sha256 of string

let pp_checksum ppf = function
  | Sha256 s -> Format.fprintf ppf "sha256=%s" s


let template version checksum ppf = Format.fprintf ppf (
   {|opam-version: "2.0"@,|}
^^ {|synopsis: "%a %a%a"@,|}
^^ {|maintainer: "platform@lists.ocaml.org"@,|}
^^ {|authors: "Xavier Leroy and many contributors"@,|}
^^ {|homepage: "https://ocaml.org"@,|}
^^ {|bug-reports: "https://github.com/ocaml/ocaml/issues"@,|}
^^ {|dev-repo: "git://github.com/ocaml/ocaml"@,|}
^^ {|depends: [@,|}
^^ {|  "ocaml" {= "%a" & post}@,|}
^^ {|  "base-unix" {post}@,|}
^^ {|  "base-bigarray" {post}@,|}
^^ {|  "base-threads" {post}@,|}
^^ {|  "ocaml-beta"@,|}
^^ {|]@,|}
^^ {|conflict-class: "ocaml-core-compiler"@,|}
^^ {|flags: compiler@,|}
^^ {|setenv: CAML_LD_LIBRARY_PATH = "%%{lib}%%/stublibs"@,|}
^^ {|build: [@,|}
^^ {|  @[<hv 2>[@,"./configure"@ "--prefix=%%{prefix}%%"%a@,|}
^^ {|"CC=cc" {os = "openbsd" | os = "macos"}@,|}
^^ {|"ASPP=cc -c" {os = "openbsd" | os = "macos"}|}
^^ {|@;<0 -2>]@]@,|}
^^ {|  [make "-j%%{jobs}%%" {os != "cygwin"} "world"]@,|}
^^ {|  [make "-j%%{jobs}%%" {os != "cygwin"} "world.opt"]@,|}
^^ {|]@,|}
^^ {|install: [make "install"]@,|}
^^ {|url {@,|}
^^ {|  src: "https://github.com/ocaml/ocaml/archive/%a.tar.gz"@,|}
^^ {|  checksum: "%a"@,|}
^^ {|}@,|}
^^ {|post-messages: [@,|}
^^ {|  "A failure in the middle of the build may be caused|}
^^ {| by build parallelism@,|}
^^ {|   (enabled by default).@,|}
^^ {|   Please file a bug report at https://github.com/ocaml/ocaml/issues"@,|}
^^ {|  {failure & jobs > 1 & os != "cygwin"}@,|}
^^ {|  "You can try installing again including --jobs=1@,|}
^^ {|   to force a sequential build instead."@,|}
^^ {|  {failure & jobs > 1 & os != "cygwin" & opam-version >= "2.0.5"}@,|}
^^ {|]|})
  pp_intro version.release
  pp_core_version version.core
  pp_variant_intro version.variant
  pp_core_version version.core
  variant_configure_option version.variant
  pp_version version
  pp_checksum checksum


let version: core_version option ref = ref None
let kind: release option ref = ref None
let checksum: checksum option ref = ref None

let parse_version s = Scanf.sscanf s "%d.%d.%d"
    (fun major minor patchlevel -> {major; minor; patchlevel})
let parse_kind = function
  | "beta" -> Beta
  | "RC" -> RC
  | "alpha" -> Alpha
  | x -> failf "Unknown release kind:%s" x


let parse_checksum s = Scanf.sscanf s "%s@=%s" (fun pre check ->
    match pre with
    | "sha256" -> Sha256 check
    | s -> failf "Unsupported checksum %s" s
  )

let kind = ref None
let iteration = ref None
let version = ref None
let id = fun x -> x
let checksum = ref None
let dir = ref None
let variants = ref [[]]


let ( ^= ) r f x = r := Some (f x)


let ( =:: ) r f x = r := f x :: !r



let parse_variant_flag = function
  | "afl" -> Afl
  | "flambda" -> Flambda
  | "fp" -> Fp
  | x -> failf "Unknown variant %s" x

let parse_variant x =
  List.map parse_variant_flag @@ String.split_on_char '+' x


let args =
  Arg.[
    "-kind", String (kind ^= parse_kind), "alpha, beta, or rc";
    "-n", Int (iteration ^= id ), "iteration of the release";
    "-v", String (version ^= parse_version), "version number";
    "-sha256", String (checksum ^= parse_checksum ),
    "checksum for the github archive";
    "-dir" , String (dir ^= id), "ocaml-variants directory";
    "-add", String (variants =:: parse_variant), "which variant to generate"
  ]


let geometry ppf =
      Format.pp_set_geometry ppf ~max_indent:65 ~margin:72

let out version fmt  = match !dir with
  | None ->
      let ppf = Format.std_formatter in
      geometry ppf; Format.fprintf ppf fmt
  | Some x ->
      let package =
        Format.asprintf "ocaml-variants.%a" pp_release_codename version in
      let dir = String.concat Filename.dir_sep [x; package ] in
      let () = if not (Sys.file_exists dir) then Unix.mkdir dir 0o777 in
      let f = open_out @@ String.concat Filename.dir_sep [dir ; "opam"] in
      let ppf = Format.formatter_of_out_channel f in
      geometry ppf; Format.kfprintf (fun _ -> close_out f) ppf fmt

let checksum () = match !checksum with
  | None ->
      Sha256 (Printf.sprintf "%064d" 0)
  | Some s -> s

let by_variant core release variant =
  let full_version = { core; release; variant } in
  out full_version "@[<v>%t@]@." (template full_version @@ checksum ())

let () =
  Arg.parse args (fun _ -> failf "No anonymous argument expected")
    "release-opam-script -version [%d.%02d.%d] -release [alpha|beta|rc] \
     -n [int] -checksum [int]";
  match !version, !kind, !iteration with
  | Some version, Some kind, Some iteration ->
      List.iter (by_variant version {kind;iteration}) !variants
  | _ ->
      failf "Missing arguments"
