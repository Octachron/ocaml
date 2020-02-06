module M: sig
  module F: functor
      (X:
         functor(A: sig type xa end)(B:sig type xz end) -> sig end
      )
      (Y:
         functor(A: sig type ya end)(B:sig type yb end) -> sig end
      )
      (Z:
         functor(A: sig type za end)(B:sig type zb end) -> sig end
      ) -> sig end
end = struct
  module F
      (X:
         functor (A: sig type xaa end)(B:sig type xz end) -> sig end
      )
      (Y:
         functor (A: sig type ya end)(B:sig type ybb end) -> sig end
      )
      (Z:
         functor (A: sig type za end)(B:sig type zbb end) -> sig end
      )
  = struct end
end
