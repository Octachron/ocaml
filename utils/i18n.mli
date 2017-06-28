(** Internationalization and localization plugin hooks *)

(**
   This module provides an interface for plugins implementing
   the internationalization and localization of OCaml user-facing messages,
   and a set of abstract type for localized string and formats.

*)

(** The type of translated string *)
type s

(** I18n implementation *)

type implementation =
  {


    kfprintf:
      'a 'r. (Format.formatter -> 'r) -> Format.formatter
      -> ('a,Format.formatter,unit,'r) format4-> 'a;
    (** Singular form printing function *)

    knfprintf:
      'a 'r. (Format.formatter -> 'r) -> Format.formatter
      -> int -> (('a,Format.formatter,unit,'r) format4 as 'f)
      -> 'f -> 'a;
    (** Plural form printing function *)

  }

(** The default implementation only convert type *)
val default:implementation

(** Plugin hook for i18n implementation *)
val hook : implementation ref


(** These functions use the current implementation *)

val s: string -> s
val i18n: string -> s
  (** Translate a string *)

val sn: int -> string -> string -> s
  (** Translate a plural string *)

val kfprintf: (Format.formatter -> 'r) -> Format.formatter
  -> ('a,Format.formatter,unit,'r) format4  -> 'a
val fprintf: Format.formatter -> ('a,Format.formatter,unit) format  -> 'a
val eprintf: ('a,Format.formatter,unit) format  -> 'a
val printf: ('a,Format.formatter,unit) format  -> 'a
  (** Translate a singular format *)

val pp: Format.formatter -> s -> unit


val fnprintf:
  Format.formatter
  -> int
  -> ( ('a,Format.formatter,unit) format as 'f)
  -> 'f
  -> 'a
  (** Translate a plural format. *)



val sprintf: ('a,Format.formatter,unit,s) format4 -> 'a
val snprintf:
  int -> ( ('a,Format.formatter,unit,s) format4 as 'f) -> 'f -> 'a
      (** String versions *)



(** Convert back to standard types *)
val to_string: s -> string
val raw: string -> s
