module M : sig module F: functor (X:sig end) -> sig end end =
  struct
    module F(X:sig type t end) = struct end
  end
