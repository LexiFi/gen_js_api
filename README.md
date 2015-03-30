gen_js_api: generate OCaml bindings for Javascript libraries
============================================================

gen_js_api aims at simplifying the creation of OCaml bindings for
Javascript libraries, for OCaml code compiled to Javascript with
js_of_ocaml.  It is based on the following ideas:

 - Authors of bindings write OCaml signatures for Javascript libraries
   and the tool generates automatically the actual binding code.

 - The generated binding code takes care of translating values between
   OCaml and Javascript.

 - The client code looks like normal OCaml code and does not depend on
   custom syntax nor on JS-specific types (unless desired).


Concretely, gen_js_api is implemented as a stand-alone processor which
takes an `.mli` file as input and produces the corresponding `.ml`
implementation.  The input signature must have a certain shape and its
declarations are annotated to specify to which JS element they
correspond.

The generated binding code is built on top of a `Ojs.t` type, which
corresponds roughly to js_of_ocaml's `Js.Unsafe.any` type -- that is,
an opaque type that represents arbitrary JS values -- but with
associated operations that abstract away from specific properties of
how js_of_ocaml represents OCaml values (for instance the fact that
OCaml integers are encoded directly as JS numbers).  This would make
it easy to change the way OCaml and JS are connected (either because
of changes in js_of_ocaml's encoding of OCaml values, or because an
entirely different technology is used, such as an OCaml bytecode
interpreter written in Javascript or a Javascript engine linked with
native OCaml code).


Interfaces processed by gen_js_api can currently contain:

  - Definition of "abstract subtypes" of `Ojs.t`:

    ````
    type t = private Ojs.t
    ````

    Values of this type are "opaque" Javascript values of a certain
    "type".  Such a definition produces in the implementation a similar
    type definition, together with a pair of upcast/downcast functions:

    ````
    val t_of_js: Ojs.t -> t
    val t_to_js: t -> Ojs.t
    ````

    These functions will be used when binding JS functions and methods to
    translate their arguments or results.  It is possible to mention
    these functions in the interface, and this is required if bindings
    defined in other compilation unit need to apply them.


  - Sub-modules:

    ````
    module M : sig
      ...
    end
    ````

    This naturally produces in the implementation a corresponding sub-module:

    ````
    module M = struct
      ...
    end
    ````


  - Value declarations:

    ````
    val f: tyexpr
    ````

    This produce in the implementation a definition for such a value,
    whose content depends on three elements: the name of the value
    (`f` in the example), its declared types (`tyexpr`), and possible
    `[@@js.xxx]` attributes attached to the declaration in the interface.

    TODO: document the convention.
