module type s = functor
  (X: sig type ai type laurie end)
  (Y:sig type lantar type lassi type surinem end)
  (Z:sig type yeni type unotime end)
  (W:sig type ve type ramar type aldararon end)
  (S: sig type yuldar type avanie end)
  (R: sig type nu type uromanie type lissemiruvoreva end)

-> sig end

module F: s = functor
  (X:sig type ai type laurie end)
  (Y:sig type silivren  type penna type miriel end)
  (Z:sig type yeni type unotime end)
  (W:sig type ve type ramar type aldararon end)
  (T:sig type yeni type ve type linte end)
  (S: sig type yuldar type avanie end)
  -> struct end
