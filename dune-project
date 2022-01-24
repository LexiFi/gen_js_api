(lang dune 2.7)
(name gen_js_api)
(version 1.1.0)

(maintainers "Alain Frisch <alain.frisch@lexifi.com>")
(authors
 "Alain Frisch <alain.frisch@lexifi.com>"
 "Sebastien Briais <sebastien.briais@lexifi.com>")

(source (github LexiFi/gen_js_api))

(generate_opam_files true)

(license MIT)

(package
 (name ojs)
 (synopsis "Runtime Library for gen_js_api generated libraries")
 (description "To be used in conjunction with gen_js_api")
 (depends
   (ocaml (>= 4.08))
   (js_of_ocaml-compiler (>= 4.0.0)))
 )

(package
 (name gen_js_api)
 (synopsis "Easy OCaml bindings for JavaScript libraries")
 (description "
gen_js_api aims at simplifying the creation of OCaml bindings for
JavaScript libraries.  Authors of bindings write OCaml signatures for
JavaScript libraries and the tool generates the actual binding code
with a combination of implicit conventions and explicit annotations.

gen_js_api is to be used with the js_of_ocaml compiler.
 ")
 (conflicts (js_of_ocaml-compiler (< 4.0.0)))
 (depends
  (ocaml (>= 4.08))
  (ppxlib (>= 0.22))
  (js_of_ocaml-compiler :with-test)
  (conf-npm :with-test)
  ojs))
