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

  - [Type declarations] (TYPES.md)

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

    This produces in the implementation a definition for such a value,
    whose content depends on three elements: the name of the value
    (`f` in the example), its declared type (`tyexpr`), and possible
    `[@@js.xxx]` attributes attached to the declaration in the interface.

    The next section documents possible forms for value declarations.


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

- Method call:

  ````
  val myMethod: t -> T1 -> ... -> Tn -> T
  [@@js.call]
  ````

  Calling the function on a first argument `o` of type `t` corresponds
  to calling the method `myMethod` on the underlying JS object, with
  other arguments passed to it.

  By default, the name of the method on the JS side is derived from
  the name of the OCaml value (`myMethod` above).  It is possible to
  specify a custom name explicitly, for instance for cases where the
  JS name is not a valid OCaml (lowercase-)identifier, or to support
  overloading (exposing multiple OCaml functions that correspond to
  different types given to the same JS method):


  ````
  val myMethod: t -> T1 -> ... -> Tn -> T
  [@@js.call "JavascriptMethodName"]
  ````

  A special case is when there is a single argument (in addition to
  the object itself) of type `unit`.  This is interpreted as a JS
  method with no argument at all.

- Object creation:

  ````
  val new_myClass: T1 -> ... -> Tn -> t
  [@@js.new]
  ````

  Corresponds to creating an object of class `myClass` with other
  arguments passed to it.

  By default, the name of the class on the JS side is derived from the
  name of the OCaml value (`myClass` above): in this case, the value
  name must start with the `new_` prefix which is dropped to obtain
  the class name.  It is also possible to specify a custom name
  explicitly.

  ````
  val new_myClass: T1 -> ... -> Tn -> t
  [@@js.new "JavascriptClassName"]
  ````

- Global value or function:

  ````
  val x: t
  [@@js.global]
  ```

  This creates an OCaml value that corresponds to a globally accessible
  Javascript value.  This is used to access both global objects (e.g.
  the `window` object) and global functions (e.g. `alert`).  It is also
  possible to specify a custom name for the Javascript variable:

  ````
  val x: t
  [@@js.global "JavascriptValueName"]
  ````

  Example:
  ````
  val alert: string -> unit
  [@@js.global]
  ````

- Property getter

  ````
  val prop: t -> T
  [@@js.get]
  ````

  Calling the function on a first argument `o` of type `t` corresponds
  to getting the `prop` property of the underlying JS object. A custom
  name for the JS property can be specified:

  ````
  val get_property: t -> T
  [@@js.get "MypProp"]
  ````


- Property setter

  ````
  val set_prop: t -> T -> unit
  [@@js.set]
  ````

  Calling the function on a first argument `o` of type `t` corresponds
  to setting the `prop` property of the underlying JS object.  Note that
  the value name must start with the `set_` prefix, which is dropped to
  obtain the property name.

  A custom name for the JS property can also be specified (in which
  case the name of the value can be arbitrary):

  ````
  val modify_prop: t -> T -> unit
  [@@js.set "prop"]
  ````

- Cast

  ```
  val cast: t1 -> t2
  [@@js.cast]
  ````

  Calling this function performs an unchecked cast from type `t1` to
  type `t2`, going through the Javascript representation (i.e.
  applying mapper from `t1` to the underlying JS object, and back
  using the mapper for `t2`).

- Custom expressions

  TODO


Class declarations
------------------

TODO


Name conversion
---------------

Unless explicitly provided, the derived Javascript name is obtained
from Caml name by (1) removing every underscore and (2) uppercasing
every character following an underscore.

Automatic binding
-----------------

Some conventions, based on the declared value names and their types,
allow to get rid of the explicit `[@@js.xxx]` attributes on value
declarations in most cases.  Here are the rules, applied in order:

- If the type has the form `t -> Ojs.t` (for a local named type `t`) and
  the value name is `t_to_js` (i.e. the type name followed by `_to_js`),
  then the function is assumed to be a `[@@js.cast]`.  This is used
  to exposed the `_to_js` function generated automatically by the tool
  for a type declaration.

- Similarly, iff the type has the form `Ojs.t -> t` (for a local named
  type `t`) and the value name is `t_to_js` (i.e. the type name
  followed by `_to_js`), then the function is assumed to be a
  `[@@js.cast]`.

- If the value is a function with a single argument (named type)  `t -> t2`,
  then the declaration is assumed to be a `[@@js.get]` property getter.

- If the value is a function with two arguments `t1 -> t2 -> unit` and
  its name starts with `set_`, then the declaration is assumed to be a
  `[@@js.set]` property setter (on the property whose name is obtained
  by dropping the `set_` prefix).

- If the value is a function whose result is a named type `... -> t`
  and its name starts with `new_`, then the declaration is assumed to
  be a `[@@js.new]` object creation (on the class whose name is
  obtained by dropping the `new_`prefix).

- If the value is a function whose first argument is a named type `t -> ...`,
  then the definition is assumed to be a `[@@js.call]` method call.

- Otherwise, the declaration is assumed to be a `[@@js.global]` value.
  This applies in particular for any non-functional type.


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
