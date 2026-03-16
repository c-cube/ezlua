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
 (name lua-ppx)
 (synopsis "Ppx for lua")
 (depends
  (ocaml
   (>= 4.14))
  dune
  ppxlib
  (odoc :with-doc)))
