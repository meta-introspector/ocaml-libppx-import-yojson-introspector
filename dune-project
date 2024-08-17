(lang dune 3.3)
(name ppx_mina_introspector)
(generate_opam_files true)
(source
 (github meta-introspector/ocaml-libppx-import-yojson-introspector))

(authors "James Michael DuPont")

(maintainers "James Michael DuPont")

(license AGPL-3.0)

(documentation https://github.com/meta-introspector/ocaml-libppx-import-yojson-introspector)

(package
 (name ppx_mina_introspector)
 (synopsis "PPX Introspector for targeting mina")
 (description "Dumping out asts in json")
 (depends ocaml dune ppx_import ppx_yojson_conv base ppx_sexp_conv)
 (tags
  (topics introspection ast json reflection compiler)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
