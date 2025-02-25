(lang dune 3.16)

(name nia_tty)

(generate_opam_files true)

(source
 (github bhlieberman/nia_tty))

(authors "Ben Lieberman")

(maintainers "Ben Lieberman")

(license BSD-3)

(documentation https://url/to/documentation)

(package
 (name nia_tty)
 (synopsis "A poem in a terminal")
 (description "A reactive TUI for reading and interacting with Nouvelles Impressions d'Afrique,
 by Raymond Roussel")
 (depends ocaml dune dolog nottui nottui-pretty lwd re ppx_let)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
