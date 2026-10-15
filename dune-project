(lang dune 3.4)

(name Language)

(generate_opam_files true)

(package
 (name Language)
 (synopsis
  "Forge: a statically typed functional language with an interpreter and a native compiler")
 (description
  "Hindley-Milner type inference, algebraic data types, traits, macros and modules; a tree-walking interpreter, a REPL and an LLVM-based native compiler.")
 (depends
  (ocaml
   (>= 5.1))
  (ounit2 :with-test)))
