(** Internationalization and localization plugin hooks *)

(**
   This module provides an interface for plugins implementing
   the internationalization and localization of OCaml user-facing messages,
   and a set of abstract type for localized string and formats.

*)

(** The type of translated string *)
type s

(** The type of translated format *)
type ('a,'b,'c,'d) f4
type ('a,'b,'c) f = ('a, 'b, 'c, 'c) f4

(** The type of delayed printer *)
type 'a delayed
type d = unit delayed

(** I18n implementation *)
type implementation ={

  s_: string -> s;
  (** Translate a string *)

  sn_: string -> string -> int -> s;
  (** Translate a plural string *)

  f_: 'a 'b 'c 'd. ('a,'b,'c,'d) format4 -> ('a,'b,'c,'d) f4;
  (** Translate a format singular argument *)

  fn_: 'a 'b 'c 'd. (('a,'b,'c,'d) format4 as 't) ->'t -> int -> ('a,'b,'c,'d) f4
    (** Translate a [Printf] plural argument. *)

}

(** The default implementation only convert type *)
val default:implementation

(** Plugin hook for i18n implementation *)
val hook : implementation ref


(** The following function uses the current hook under the hood *)
module I18n_core: sig
  val s_ : string -> s
  val sn_ : string -> string -> int -> s
  val f_: ('a,'b,'c,'d) format4 -> ('a,'b,'c,'d) f4
  val fn_: (('a,'b,'c,'d) format4 as 't) -> 't -> int -> ('a,'b,'c,'d) f4
end

(** Printing function imported from format that are useful to ensure
  that the format or string used as entry are localized
*)

val pp: Format.formatter -> s -> unit
val i18n: Format.formatter -> s -> unit

val printf : ('a, Format.formatter, unit) f -> 'a
val eprintf : ('a, Format.formatter, unit) f -> 'a

val sprintf : ('a, unit, s) f -> 'a
val fprintf : (Format.formatter as 'fmt) -> ('a, 'fmt, unit) f -> 'a

val asprintf : ('a, Format.formatter, unit, s) f4 -> 'a

val kfprintf :
  ((Format.formatter as 'fmt) -> 'a) -> 'fmt -> ('b, 'fmt, unit, 'a) f4 -> 'b

(** Localized delayed printing *)
val dprintf: ('a,Format.formatter,unit) f -> 'a delayed

val (<$>): ('a -> 'b) delayed -> 'a -> 'b delayed
(** Application for delayed printer:

{[
      (fun ppf -> Format.fprintf ppf "The %s message %d should be translated"
                  "warning" 32
      )
]}

can be translated to

    {[
      (dprintf (f_"The %a message %d should be translated")
                  <$> i18n <$> (s_"warning") <$> 32
      )
    ]}

*)

(** Convert back to standard types *)
val to_string: s -> string
val to_format: ('a,'b,'c,'d) f4 -> ('a,'b,'c,'d) format4
val raw: string -> s
val rawf: ('a,'b,'c,'d) format4 -> ('a,'b,'c,'d) f4
val rawd: (Format.formatter -> 'a) -> 'a delayed
val t: 'a delayed -> Format.formatter -> 'a
