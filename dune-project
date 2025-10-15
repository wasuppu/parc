(lang dune 3.15)

(name parc)

(generate_opam_files true)

(source
 (github wasuppu/parc))

(authors "lundrore")

(maintainers "lundrore")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name parc)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune)
 (tags
  (topics "parser combinator" "wyas")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
