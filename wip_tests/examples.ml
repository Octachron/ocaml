module type empty = sig end
module type a
module type b
module type c

module Empty = struct end;;

module Example1 = struct

module type f = functor (X:empty)(Y:empty) -> empty
module F: f =
  functor(X:empty)(Y:empty)(Z:empty) -> Empty

end;;


module Example_2 = struct
  module type f = functor (X:a)(Y:b) -> c
  module F:f = functor (X:a)(Y:b)(Z:c) -> Empty
end
