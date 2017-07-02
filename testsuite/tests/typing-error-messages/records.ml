module M: sig
  type t = { alpha: unit; beta: int; delta: unit}
end =
struct
  type t = { alpha: unit; beta:unit; gamma: unit}
end;;
