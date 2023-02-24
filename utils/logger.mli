(** The log module provides an unified interface for logging in *)
    a structured log
*)

type 'a log_scheme

module New_scheme(): sig
  type id
  val init: unit -> id log_scheme
end

val raw: version -> 'id log_scheme -> 'id log
val flush: 'id log -> unit

type 'a printer
val int: int printer
val string: string printer
val list: 'a printer -> 'a list printer

val custom: ('b -> 'a) -> 'a printer -> 'b printer

val pair: 'a printer * 'b printer -> ('a*'b) printer
val triple: 'a printer * 'b printer * 'c printer -> ('a*'b*'c) printer
val quadruple: 'a printer * 'b printer * 'c printer * 'd printer -> ('a*'b*'c*'d) printer

val sublog: 'id log printer

type ('a,'b) key
val new_key: path:string list ->
  'id log_scheme -> 'a printer -> ('a,'id) key

val deprecate_key: ('a,'id) key -> 'id log_scheme -> unit



(** {1:log_scheme_versionning  Current version of the log } *)
type version = { major:int; minor:int }
type version_range = { introduction: version; deprecation: version option }

val version: _ log_scheme -> version
val version_range: (_,_) key -> version_range


(** {1:log_creation }*)

type 'a log

val fmt: Format.formatter -> version -> 'a log_scheme -> 'a log

val set: ('a,'b) key  -> 'a -> 'b log -> unit
val (.![]<-): 'b log -> ('a,'b) log -> 'a -> unit


val fmt : (string,'a) key -> 'a log -> ('b, Format.formatter, unit) format -> 'b
  (** [logf key log fmt] records the output of [fmt] as
      a string at key [key] in [log].
  *)

val itemf : (string list,'id) key ->
  'id log -> ('a, Format.formatter, unit) format -> 'a
  (** [log_itemf key log fmt] appends the output of [fmt]
      to the list at key [key] in [log].
  *)
