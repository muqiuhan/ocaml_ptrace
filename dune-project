(lang dune 3.6)

(name ocaml_ptrace)

(generate_opam_files true)

(source
 (github muqiuhan/ocaml_ptrace))

(authors "Muqiu Han")

(maintainers "Muqiu Han")

(license MIT)

(documentation https://github.com/muqiuhan/ocaml_ptrace)

(package
 (name ocaml_ptrace)
 (allow_empty)
 (synopsis "OCaml ptrace function binding")
 (description "OCaml ptrace function binding")
 (depends ocaml dune)
 (tags
  (ptrace binding)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
