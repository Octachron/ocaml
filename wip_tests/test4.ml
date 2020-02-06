module type first = sig
  type arma
  type virumque
  type cano
  type troiae
  type qui
  type primus
  type ab
  type oris
end

module F (X: first) = struct end

module type s =
sig
  type iram
  type arma
  type virumque
  type cano
  type troiae
  type qui
  type primus
  type ab
end

module rec X: s = X

module M = F(X)
