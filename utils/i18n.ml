(** Internationalization and localization plugin hooks *)

(** The type of translated string *)
type s = string

(** The type of translated format *)
type ('a,'b,'c,'d) f4 = ('a,'b,'c,'d) format4
type ('a,'b,'c) f = ('a,'b,'c) format

type 'a delayed = Format.formatter -> 'a
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

let id x = x
let choice x y n =
  (* folloxing gettext convention, the default behavior is assumed to be the
     germanic language with a singular(=1) and non-singular form
     (i.e. {0} union [2,+infinity]) *)
  if n = 1 then x else y

(** The default implementation only convert type *)
let default = {
  s_ = id;
  sn_ = choice;
  f_ = id;
  fn_ = choice
}

let hook = ref default
(** Plugin hook for i18n implementation *)


module I18n_core = struct
  (** The following function uses the current hook under the hood *)

  let s_ x = !hook.s_ x
  let sn_  x y n = !hook.sn_ x y n
  let f_ x = !hook.f_ x
  let fn_ x y n = !hook.fn_ x y n
end
open I18n_core

open Format
let printf fmt = printf (f_ fmt)
let eprintf fmt = printf (f_ fmt)

let fprintf ppf fmt = fprintf ppf (f_ fmt)
let sprintf fmt = sprintf (f_ fmt)

let kfprintf k ppf fmt = kfprintf k ppf (f_ fmt)
let asprintf fmt = asprintf (f_ fmt)

let dprintf fmt ppf =
  kfprintf ignore ppf fmt

let (<$>) f x ppf = f ppf x

let pp = pp_print_string
let i18n = pp
let raw = id
let rawf =id
let rawd = id
let to_string = id
let to_format = id
let t = id
