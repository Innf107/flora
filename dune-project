(lang dune 3.6)

(name flora)

(generate_opam_files true)

(source
 (github Innf107/flora))

(authors "Finn Schneider")

(maintainers "Finn Schneider")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name flora)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune menhir)
 (tags
  (topics "to describe" your project)))

(using menhir 2.1)

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
