gen_js_api: generate OCaml bindings for Javascript libraries
============================================================

Overview
--------

gen_js_api aims at simplifying the creation of OCaml bindings for
Javascript libraries, to be used with the js_of_ocaml compiler.  It is
based on the following ideas:

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

Low-level binding
-----------------

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


gen_js_api interfaces
---------------------


Interfaces processed by gen_js_api can currently contain:

  - [Type declarations](TYPES.md):

    ````
    type t = ...
    ````

    See [this page](TYPES.md) for a description of supported types.
    Such a type declaration produces in the implementation an identical
    defininition, and associated `*_to_js` and `*_of_js` functions
    (which can be manually exported if needed).


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


  - [Value declarations](VALUES.md):

    ````
    val f: tyexpr
    ````

    This produces in the implementation a definition for such a value,
    whose content depends on three elements: the name of the value
    (`f` in the example), its declared type (`tyexpr`), and possible
    `[@@js.xxx]` attributes attached to the declaration in the interface.

    See [this page] (VALUES.md) for supported forms of value declarations.


  - Class declarations:

    ````
    class my_class: Ojs.t ->
      object
        inherit Ojs.obj
        ....
      end
    ````

    The class must inherit from `Ojs.obj` directly or indirectly.

    This declaration produces an OCaml class to wrap JS objects.

    TODO:

       - specify how method types + attributes are interpreted
       - document class rebinding



Value bindings
--------------

Class declarations
------------------

TODO


Name conversion
---------------

Unless explicitly provided, the derived Javascript name is obtained
from Caml name by (1) removing every underscore and (2) uppercasing
every character following an underscore.


PPX on implementations
----------------------

While the primary mode of operation for gen_js_api is to generate an
.ml file from an annotated .mli file, it is also possible to use it as
a ppx preprocessor on .ml file directly to insert local JS bindings.

The `-ppx` command-line option must be the first argument passed
to gen_js_api to enable the ppx mode.

Several forms are supported:

 - `[%js]` extension as a module expression, to be used directly under
   a module-level type constraint.  Examples:

   ````
     include ([%js] : sig ... end)

     module M : sig ... end = [%js]
   ````

   The signature is processed as it it were found in an .mli file, and
   the resulting structure is inserted in place of the `[%js]`
   extension.

 - `[@@js]` attributes on type declarations.

   Example:

   ````
     type t = { x : int; y : int } [@@js]
   ````

   This generates the corresponding `*_of_js` and `*_to_js` functions.
   In case of a multi-type declaration, each type must be annotated
   with `[@@js]` (if needed).

 - `[@@js.*]` attributes on val declarations.

   Example:

   ````
     val alert_bool: bool -> unit
       [@@js.global "alert"]

     val myGlobalValue: int array
       [@@js]
   ````

   In implementations, such `val` declarations are recognized only
   if they have some `[@@js.*]` attribute.  To enable the default
   heuristics, one can use `[@@js]`.

 - `[%js.to: ty]` and `[%js.of: ty]` extensions an expressions.

   Example:

   ````
     let x : Ojs.t = [%js.of: int list]  [ 10; 20; 30 ]
   ````

   This form generates the mapping function associated to a JS-able type.


About
-----

This package is licensed by LexiFi under the terms of the MIT license.

Contact: alain.frisch@lexifi.com

Contributors:

 - Alain Frisch
 - Sebastien Briais


Currenlty, this package works only on OCaml trunk, not on any released
version.  The package is itself not released.

See our [TODO list] (TODO.md).
