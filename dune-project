(lang dune 3.17)

(name textsearch)

(generate_opam_files true)

(source
 (github Debajyati/textsearch))

(authors "Debajyati Dey <ddebajyati@gmail.com>")

(maintainers "Debajyati Dey <ddebajyati@gmail.com>")

(license MIT)

(documentation https://github.com/Debajyati/textsearch#)

(package
 (name textsearch)
 (synopsis "A simple text search CLI tool")
 (description "CLI tool to search terms in files")
 (depends  
  (ocaml (>= 5.2))  
  (cmdliner (>= 1.3.0))  
  (dune (>= 3.17)))
 (tags
  ("cmdline" "text" "search")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
