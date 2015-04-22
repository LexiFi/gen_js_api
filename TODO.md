TODO list for gen_js_api
========================



- Support sum types / polymorphic variants with non constant constructors
  (mapped to objects with a discriminator field).


- Support really abstract types (treated as `Ojs.t` in the implementation).

- Optimize generated code (for instance, lift calls to string_of_js on
  literals).

- Compile properly qualified globals [@@js.global "foo.bar"] to avoid
  js_of_ocaml warning.

- Allow to put a final "unit" argument (ignored, but allowing to have
  optional argument in final position).  This also generalize the case
  of 0-ary functions.
