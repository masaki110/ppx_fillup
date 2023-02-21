(lang dune 2.8)

(name ppx_fillup)

(package
 (name ppx_fillup)
 (depends
  (ppxlib
   (and
    (>= 0.19.0)
    (< 0.27.0)))
  ocaml-compiler-libs
  str))
