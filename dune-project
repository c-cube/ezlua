(lang dune 3.0)

(name lua-ppx)

(version 0.1)

(generate_opam_files true)

(source
 (github c-cube/lua-ppx))

(authors "Simon Cruanes")
(maintainers "Simon Cruanes")
(license MIT)

;(documentation https://url/to/documentation)

(package
 (name ezlua)
 (synopsis "OCaml-Lua codec runtime and high-level interpreter API")
 (depends
  (ocaml (>= 4.14))
  dune
  ocaml-lua))

(package
 (name ppx_ezlua)
 (synopsis "PPX deriving for Lua (ezlua)")
 (depends
  (ocaml (>= 4.14))
  dune
  ppxlib
  ezlua
  (ppx_deriving :with-test)
  (alcotest :with-test)))
