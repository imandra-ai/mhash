(lang dune 2.0)
(generate_opam_files true)
(version 0.1)
(license MIT)
(name mhash)

(package
  (name mhash)
  (synopsis "Modular Hashing library"))

(package
  (name ppx_deriving_mhash)
  (synopsis "Ppx deriver for mhash")
  (depends ppxlib))
