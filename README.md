gen_js_api: generate OCaml bindings for Javascript libraries
============================================================

Overview
--------

gen_js_api aims at simplifying the creation of OCaml bindings for
Javascript libraries, to be used with the [js_of_ocaml
compiler](https://github.com/ocsigen/js_of_ocaml).  It is based on the
following ideas:

 - Authors of bindings write OCaml signatures for Javascript libraries
   and the tool generates the actual binding code with a combination
   of implicit conventions and explicit annotations.

 - The generated binding code takes care of translating values between
   OCaml and Javascript.

 - All syntactic processing is done by authors of bindings: the client
   code is normal OCaml code and does not depend on custom syntax nor
   on JS-specific types.


gen_js_api can be used in two complementary ways:

  - [Generating .ml implementations from annotated .mli interfaces](IMPLGEN.md),
    in order to create the code for stub libraries.

  - As a [ppx preprocessor on implementations](PPX.md) to define local
    bindings.


Examples
--------

The repository contains some examples of OCaml bindings to Javascript
libraries created with gen_js_api:

 - Very partial [ bindings to jQuery](examples/jquery.mli), with
   some [example client code](examples/test_jquery.ml).

 - Partial [bindings to Javascript strings and
   regexps](examples/js_str.mli), with some [example client
   code](examples/test_js_str.ml).

 - Partial [bindings to Javascript dates](examples/js_date.mli), with
   some [example client code](examples/test_js_date.ml).

Documentation
-------------

  - [Install and use](INSTALL_AND_USE.md)
  - [Low-level binding to Javascript](LOW_LEVEL_BINDING.md)
  - [Using gen_js_api to generate .ml from .mli](IMPLGEN.md)
  - [Using gen_js_api as a ppx processor](PPX.md)
  - [Default naming convention](NAMING.md)
  - [JS-able types and type declarations](TYPES.md)
  - [Value bindings](VALUES.md)
  - [Class-wrapping bindings](CLASSES.md)
  - [TODO list] (TODO.md)


Related projects
----------------

  - [js_of_ocaml](https://github.com/ocsigen/js_of_ocaml): The compiler
    and runtime system on which gen_js_api relies. (Note: gen_js_api
    doesn't depend on js_of_ocaml's OCaml library, nor on its language
    extension.)

  - [goji](https://github.com/klakplok/goji): A DSL to describe OCaml
    bindings for JavaScript libraries.

  - [DefinitelyMaybeTyped](https://github.com/andrewray/DefinitelyMaybeTyped):
    A project to parse
    [DefinitelyTyped](https://github.com/borisyankov/DefinitelyTyped)
    interfaces and produce OCaml interfaces.


About
-----

This package is licensed by LexiFi under the terms of the MIT license.

Contact: alain.frisch@lexifi.com

Contributors:

 - Alain Frisch
 - Sebastien Briais
