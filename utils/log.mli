(** The log module provides an unified interface for logging in
    a structured log
*)

type 'a log_scheme

module New_scheme(): sig
  type id
  val scheme: id log_scheme
end



type doc = Format.formatter -> unit

type !'a log


type _ extension = ..


type 'a printer =
  | Int: int printer
  | String: string printer
  | Doc: doc printer
  | Option: 'a printer -> 'a option printer
  | List: 'a printer -> 'a list printer
  | Pair: 'a printer * 'b printer -> ('a * 'b) printer
  | Triple: 'a printer * 'b printer * 'c printer -> ('a * 'b * 'c) printer
  | Quadruple: 'a printer * 'b printer * 'c printer * 'd printer -> ('a * 'b * 'c * 'd) printer
  | Custom: { id :'b extension; pull: ('b -> 'a); default: 'a printer} ->
      'b printer
  | Sublog: 'id log_scheme -> 'id log printer

type ('a,'b) key


type device = {
  print: 'a. key:string list -> 'a printer -> 'a -> unit;
  sub: key:string list -> device;
  flush: unit -> unit
}


val new_key: path:string list ->
  'id log_scheme -> 'a printer -> ('a,'id) key

val deprecate_key: ('a,'id) key -> 'id log_scheme -> unit



(** {1:log_scheme_versionning  Current version of the log } *)
type version = { major:int; minor:int }

type version_range = { introduction: version; deprecation: version option }

val version: _ log_scheme -> version
val name_version: _ log_scheme -> version -> unit
val version_range: (_,'id) key -> 'id log_scheme -> version_range


(** {1:log_creation Log } *)


val flush: 'id log -> unit

val create: device -> version -> 'a log_scheme -> 'a log
val detach: ('b log, 'a) key -> 'a log ->  'b log



type format_extension_printer =
  { extension: 'b. 'b extension -> (Format.formatter -> 'b -> unit) option}

val make_fmt: format_extension_printer -> Format.formatter -> device

val set: ('a,'b) key  -> 'a -> 'b log -> unit
val (.%[]<-): 'b log -> ('a,'b) key -> 'a -> unit


val fmt : (string,'a) key -> 'a log -> ('b, Format.formatter, unit) format -> 'b
  (** [logf key log fmt] records the output of [fmt] as
      a string at key [key] in [log].
  *)
