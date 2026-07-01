(lang dune 3.22)

(name ocaml_dspy)

(package
 (name ocaml_dspy)
 (depends
  ocaml
  dune
  yojson
  eio
  eio_main
  cohttp-eio
  tls-eio
  ca-certs
  mirage-crypto-rng
  hmap
  uri
  domain-name
  ppxlib
  alcotest))

(package
 (name ppx_ocaml_dspy)
 (depends
  ocaml
  dune
  ppxlib))
