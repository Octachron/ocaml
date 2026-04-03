(* TEST
 flags = "-dshape";
 expect;
*)

(* Make sure that shapes of compilation units are never eagerly loaded,
   regardless of the context. *)

module Mdirect = Stdlib__Unit
[%%expect{|
{
 "Mdirect"[module] -> Alias(<.0>
                            CU Stdlib__Unit);
 }
module Mdirect = Unit
|}, Principal{|
{
 "Mdirect"[module] -> Alias(<.1>
                            CU Stdlib__Unit);
 }
module Mdirect = Unit
|}, Rectypes{|
{
 "Mdirect"[module] -> Alias(<.2>
                            CU Stdlib__Unit);
 }
module Mdirect = Unit
|}]

module Mproj = Stdlib.Unit
[%%expect{|
{
 "Mproj"[module] -> Alias(<.3>
                          CU Stdlib . "Unit"[module]);
 }
module Mproj = Unit
|}, Principal{|
{
 "Mproj"[module] -> Alias(<.4>
                          CU Stdlib . "Unit"[module]);
 }
module Mproj = Unit
|}, Rectypes{|
{
 "Mproj"[module] -> Alias(<.5>
                          CU Stdlib . "Unit"[module]);
 }
module Mproj = Unit
|}]

module F (X : sig type t end) = X
[%%expect{|
{
 "F"[module] -> Abs<.8>(X, X<.7>);
 }
module F : (X : sig type t end) -> sig type t = X.t end
|}, Principal{|
{
 "F"[module] -> Abs<.11>(X, X<.10>);
 }
module F : (X : sig type t end) -> sig type t = X.t end
|}, Rectypes{|
{
 "F"[module] -> Abs<.14>(X, X<.13>);
 }
module F : (X : sig type t end) -> sig type t = X.t end
|}]

module App_direct = F (Stdlib__Unit)
[%%expect{|
{
 "App_direct"[module] -> CU Stdlib__Unit;
 }
module App_direct : sig type t = Unit.t end
|}]

module App_proj = F (Stdlib.Unit)
[%%expect{|
{
 "App_proj"[module] -> (CU Stdlib . "Unit"[module])<.18>;
 }
module App_proj : sig type t = Unit.t end
|}, Principal{|
{
 "App_proj"[module] -> (CU Stdlib . "Unit"[module])<.19>;
 }
module App_proj : sig type t = Unit.t end
|}, Rectypes{|
{
 "App_proj"[module] -> (CU Stdlib . "Unit"[module])<.20>;
 }
module App_proj : sig type t = Unit.t end
|}]

module App_direct_indir = F (Mdirect)
[%%expect{|
{
 "App_direct_indir"[module] -> Alias(<.21>
                                     CU Stdlib__Unit);
 }
module App_direct_indir : sig type t = Mdirect.t end
|}, Principal{|
{
 "App_direct_indir"[module] -> Alias(<.22>
                                     CU Stdlib__Unit);
 }
module App_direct_indir : sig type t = Mdirect.t end
|}, Rectypes{|
{
 "App_direct_indir"[module] -> Alias(<.23>
                                     CU Stdlib__Unit);
 }
module App_direct_indir : sig type t = Mdirect.t end
|}]

module App_proj_indir = F (Mproj)
[%%expect{|
{
 "App_proj_indir"[module] -> Alias(<.24>
                                   CU Stdlib . "Unit"[module]);
 }
module App_proj_indir : sig type t = Mproj.t end
|}, Principal{|
{
 "App_proj_indir"[module] -> Alias(<.25>
                                   CU Stdlib . "Unit"[module]);
 }
module App_proj_indir : sig type t = Mproj.t end
|}, Rectypes{|
{
 "App_proj_indir"[module] -> Alias(<.26>
                                   CU Stdlib . "Unit"[module]);
 }
module App_proj_indir : sig type t = Mproj.t end
|}]

(* In the following the shape are not loaded, we just know what the signature
   are and build shapes from them. *)

include Stdlib__Unit
[%%expect{|
{
 "compare"[value] -> CU Stdlib__Unit . "compare"[value];
 "equal"[value] -> CU Stdlib__Unit . "equal"[value];
 "t"[type] -> CU Stdlib__Unit . "t"[type];
 "to_string"[value] -> CU Stdlib__Unit . "to_string"[value];
 }
type t = unit = ()
val equal : t -> t -> bool = <fun>
val compare : t -> t -> int = <fun>
val to_string : t -> string = <fun>
|}]

include Stdlib.Unit
[%%expect{|
{
 "compare"[value] -> CU Stdlib . "Unit"[module] . "compare"[value];
 "equal"[value] -> CU Stdlib . "Unit"[module] . "equal"[value];
 "t"[type] -> CU Stdlib . "Unit"[module] . "t"[type];
 "to_string"[value] -> CU Stdlib . "Unit"[module] . "to_string"[value];
 }
type t = unit = ()
val equal : t -> t -> bool = <fun>
val compare : t -> t -> int = <fun>
val to_string : t -> string = <fun>
|}]

module Without_constraint = Set.Make(Int)
[%%expect{|
{
 "Without_constraint"[module] ->
   CU Stdlib . "Set"[module] . "Make"[module](CU Stdlib . "Int"[module])<.27>;
 }
module Without_constraint :
  sig
    type elt = Int.t
    type t = Set.Make(Int).t
    val empty : t
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val disjoint : t -> t -> bool
    val diff : t -> t -> t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val min_elt_opt : t -> elt option
    val max_elt : t -> elt
    val max_elt_opt : t -> elt option
    val choose : t -> elt
    val choose_opt : t -> elt option
    val find : elt -> t -> elt
    val find_opt : elt -> t -> elt option
    val find_first : (elt -> bool) -> t -> elt
    val find_first_opt : (elt -> bool) -> t -> elt option
    val find_last : (elt -> bool) -> t -> elt
    val find_last_opt : (elt -> bool) -> t -> elt option
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
    val map : (elt -> elt) -> t -> t
    val filter : (elt -> bool) -> t -> t
    val filter_map : (elt -> elt option) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val split : elt -> t -> t * bool * t
    val is_empty : t -> bool
    val is_singleton : t -> bool
    val singleton_to_elt : t -> elt option
    val mem : elt -> t -> bool
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val subset : t -> t -> bool
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val to_list : t -> elt list
    val of_list : elt list -> t
    val to_seq_from : elt -> t -> elt Seq.t
    val to_seq : t -> elt Seq.t
    val to_rev_seq : t -> elt Seq.t
    val add_seq : elt Seq.t -> t -> t
    val of_seq : elt Seq.t -> t
  end
|}, Principal{|
{
 "Without_constraint"[module] ->
   CU Stdlib . "Set"[module] . "Make"[module](CU Stdlib . "Int"[module])<.28>;
 }
module Without_constraint :
  sig
    type elt = Int.t
    type t = Set.Make(Int).t
    val empty : t
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val disjoint : t -> t -> bool
    val diff : t -> t -> t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val min_elt_opt : t -> elt option
    val max_elt : t -> elt
    val max_elt_opt : t -> elt option
    val choose : t -> elt
    val choose_opt : t -> elt option
    val find : elt -> t -> elt
    val find_opt : elt -> t -> elt option
    val find_first : (elt -> bool) -> t -> elt
    val find_first_opt : (elt -> bool) -> t -> elt option
    val find_last : (elt -> bool) -> t -> elt
    val find_last_opt : (elt -> bool) -> t -> elt option
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
    val map : (elt -> elt) -> t -> t
    val filter : (elt -> bool) -> t -> t
    val filter_map : (elt -> elt option) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val split : elt -> t -> t * bool * t
    val is_empty : t -> bool
    val is_singleton : t -> bool
    val singleton_to_elt : t -> elt option
    val mem : elt -> t -> bool
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val subset : t -> t -> bool
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val to_list : t -> elt list
    val of_list : elt list -> t
    val to_seq_from : elt -> t -> elt Seq.t
    val to_seq : t -> elt Seq.t
    val to_rev_seq : t -> elt Seq.t
    val add_seq : elt Seq.t -> t -> t
    val of_seq : elt Seq.t -> t
  end
|}, Rectypes{|
{
 "Without_constraint"[module] ->
   CU Stdlib . "Set"[module] . "Make"[module](CU Stdlib . "Int"[module])<.29>;
 }
module Without_constraint :
  sig
    type elt = Int.t
    type t = Set.Make(Int).t
    val empty : t
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val disjoint : t -> t -> bool
    val diff : t -> t -> t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val min_elt_opt : t -> elt option
    val max_elt : t -> elt
    val max_elt_opt : t -> elt option
    val choose : t -> elt
    val choose_opt : t -> elt option
    val find : elt -> t -> elt
    val find_opt : elt -> t -> elt option
    val find_first : (elt -> bool) -> t -> elt
    val find_first_opt : (elt -> bool) -> t -> elt option
    val find_last : (elt -> bool) -> t -> elt
    val find_last_opt : (elt -> bool) -> t -> elt option
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
    val map : (elt -> elt) -> t -> t
    val filter : (elt -> bool) -> t -> t
    val filter_map : (elt -> elt option) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val split : elt -> t -> t * bool * t
    val is_empty : t -> bool
    val is_singleton : t -> bool
    val singleton_to_elt : t -> elt option
    val mem : elt -> t -> bool
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val subset : t -> t -> bool
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val to_list : t -> elt list
    val of_list : elt list -> t
    val to_seq_from : elt -> t -> elt Seq.t
    val to_seq : t -> elt Seq.t
    val to_rev_seq : t -> elt Seq.t
    val add_seq : elt Seq.t -> t -> t
    val of_seq : elt Seq.t -> t
  end
|}]

module With_identity_constraint : sig
  module M : Set.S
end = struct
  module M = Set.Make(Int)
end
[%%expect{|
{
 "With_identity_constraint"[module] ->
   {<.32>
    "M"[module] ->
      CU Stdlib . "Set"[module] . "Make"[module](
      CU Stdlib . "Int"[module])<.30>;
    };
 }
module With_identity_constraint : sig module M : Set.S end
|}, Principal{|
{
 "With_identity_constraint"[module] ->
   {<.35>
    "M"[module] ->
      CU Stdlib . "Set"[module] . "Make"[module](
      CU Stdlib . "Int"[module])<.33>;
    };
 }
module With_identity_constraint : sig module M : Set.S end
|}, Rectypes{|
{
 "With_identity_constraint"[module] ->
   {<.38>
    "M"[module] ->
      CU Stdlib . "Set"[module] . "Make"[module](
      CU Stdlib . "Int"[module])<.36>;
    };
 }
module With_identity_constraint : sig module M : Set.S end
|}]

module With_constraining_constraint : sig
  module M : sig type t end
end = struct
  module M = Set.Make(Int)
end
[%%expect{|
{
 "With_constraining_constraint"[module] ->
   {<.42>
    "M"[module] ->
      {<.39>
       "t"[type] ->
         CU Stdlib . "Set"[module] . "Make"[module](
         CU Stdlib . "Int"[module])<.39> . "t"[type];
       };
    };
 }
module With_constraining_constraint : sig module M : sig type t end end
|}, Principal{|
{
 "With_constraining_constraint"[module] ->
   {<.46>
    "M"[module] ->
      {<.43>
       "t"[type] ->
         CU Stdlib . "Set"[module] . "Make"[module](
         CU Stdlib . "Int"[module])<.43> . "t"[type];
       };
    };
 }
module With_constraining_constraint : sig module M : sig type t end end
|}, Rectypes{|
{
 "With_constraining_constraint"[module] ->
   {<.50>
    "M"[module] ->
      {<.47>
       "t"[type] ->
         CU Stdlib . "Set"[module] . "Make"[module](
         CU Stdlib . "Int"[module])<.47> . "t"[type];
       };
    };
 }
module With_constraining_constraint : sig module M : sig type t end end
|}]
