(lang dune 3.22)

(generate_opam_files true)

(name ocaml_dspy)

(package
 (name ocaml_dspy)
 (synopsis "A small OCaml port of the core DSPy programming model")
 (maintainers "Arjun Guha <a.guha@northeastern.edu>")
 (authors "Arjun Guha")
 (homepage "https://github.com/arjunguha/ocaml_dspy")
 (bug_reports "https://github.com/arjunguha/ocaml_dspy/issues")
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
 (synopsis "PPX rewriter for OCaml DSPy signatures and predictors")
 (maintainers "Arjun Guha <a.guha@northeastern.edu>")
 (authors "Arjun Guha")
 (homepage "https://github.com/arjunguha/ocaml_dspy")
 (bug_reports "https://github.com/arjunguha/ocaml_dspy/issues")
 (depends
  ocaml
  dune
  ppxlib))
