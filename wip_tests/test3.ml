module F
(X:sig
  type arma
  type virumque
  type cano
  type troiae
  type qui
  type primus
  type ab
  type oris
end)
(Y:sig
  type italiam
  type fato
  type profugus
  type laviniaque
  type venit
end)
(Z:sig
   type litorra
   type multum
   type ille
   type et
   type terris
   type iactatis
   type et'
   type alto
end)
(W:sig
   type vi
   type superum
   type saevae
   type memoriam
   type iunonis
   type ob
   type iram
end)
 = struct end

module M =
F(struct
  type iram
  type arma
  type virumque
  type cano
  type troiae
  type qui
  type primus
  type ab
end)
(struct
  type oris
  type italiam
  type fato
  type profugus
  type laviniaque
end)
(struct
  type venit
   type litorra
   type multum
   type ille
   type et
   type terris
   type iactatis
   type et'
end)
(struct
   type alto
   type vi
   type superum
   type saevae
   type memoriam
   type iunonis
   type ob
end)
