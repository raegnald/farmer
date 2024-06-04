(lang dune 3.15)
(using menhir 3.0)

(name farmer)

(generate_opam_files false)

(package
 (name farmer)
 (depends ocaml dune))
