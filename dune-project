(lang dune 2.0)
(generate_opam_files true)
(version 0.1)
(license MIT)
(name mhash)

(package
  (name mhash)
  (depends
    (odoc :with-doc))
  (synopsis "Modular Hashing library"))

(package
  (name mhash-sha)
  (depends
    mhash
    sha
    (odoc :with-doc))
  (synopsis "Mhash implementation for Sha"))

(package
  (name ppx_deriving_mhash)
  (synopsis "Ppx deriver for mhash")
  (depends mhash ppxlib))
