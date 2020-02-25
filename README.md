gen_js_api: easy OCaml bindings for Javascript libraries
========================================================

[![Build Status](https://travis-ci.com/LexiFi/gen_js_api.svg?branch=master)](https://travis-ci.com/LexiFi/gen_js_api)

Overview
--------

gen_js_api aims at simplifying the creation of OCaml bindings for
Javascript libraries.  It must currently be used with the [js_of_ocaml
compiler](https://github.com/ocsigen/js_of_ocaml), although other ways
to run OCaml code "against" Javascript might be supported later with
the same binding definitions (for instance,
[Bucklescript](https://github.com/bloomberg/bucklescript),
or direct embedding of a JS engine in a native OCaml application).

gen_js_api is based on the following ideas:

 - Authors of bindings write OCaml signatures for Javascript libraries
   and the tool generates the actual binding code with a combination
   of implicit conventions and explicit annotations.

 - The generated binding code takes care of translating values between
   OCaml and Javascript and of dealing with Javascript calling
   conventions.

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

 - Very partial [bindings to jQuery](examples/misc/jquery.mli), with
   some [example client code](examples/misc/test_jquery.ml).

 - Partial bindings to Javascript [strings and
   regexps](examples/misc/js_str.mli) and Javascript
   [dates](examples/js_date.mli).

 - Some [ad hoc test](examples/test) to exercise various features.

 - An example of a self-contained program, a [simple
   calculator](examples/calc/calc.ml), implementing local .bindings

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
  - [TODO list](TODO.md)


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

  - [Bucklescript](https://github.com/bloomberg/bucklescript):
    Another compiler from OCaml to Javascript.


About
-----

gen_js_api has been created by LexiFi for porting a web application
from Javascript to OCaml.  The tool has been used in production since
2015.

This gen_js_api package is licensed by LexiFi under the terms of the
MIT license.

See see [Changelog](CHANGES.md)

Contact: alain.frisch@lexifi.com

Contributors:

 - Alain Frisch
 - Sebastien Briais
