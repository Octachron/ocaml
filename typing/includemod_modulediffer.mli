(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                Malo Monin, projet Cambium, Inria Paris                 *)
(*                                                                        *)
(*   Copyright 2024 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Suggestion : sig
  type alteration =
    | Add_item
    | Rename_item of Ident.t
    | Change_type_of_value of Types.type_expr
    | Change_type of Types.type_declaration

  type t = {
    subject : Types.signature_item;
    alteration : alteration;
  }
end

val suggest :
  Includemod.Error.signature_symptom -> int -> Suggestion.t list
