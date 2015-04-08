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
    defined in other compilation units need to call them.


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


JS-able types
-------------

A JS-able type is an OCaml type whose values can be mapped to and from
Javascript objects.  Technically, a non-parametrized type with path
`M.t` is JS-able if the following two values are available in module
`M`:

 ````
 val t_to_js: t -> Ojs.t
 val t_of_js: Ojs.t -> t
 ````

The name of these values is obtained by appending `_of_js` or `_to_js`
to the local name of the type.

Parametrized types can also be JS-able.  It is currently assumed that
such types are covariant in each of their parameter.  Mapping
functions take extra arguments corresponding to the mapper for each
parameter.  For instance, a type `'a t` would need to come with the following
functions:

 ````
 val t_to_js: ('a -> Ojs.t) -> 'a t -> Ojs.t
 val t_of_js: (Ojs.t -> 'a) -> Ojs.t -> 'a t
 ````

Some built-in types are treated in a special way to make them JS-able:
`string`, `int`, `bool`, `float`, `array`, `list`, `option`.  Arrays
and lists are mapped to JS arrays (which are assumed to be indexed by
integers 0..length-1).  Options are mapped to the same type as their
parameter: `None` is mapped to JS `null` value, and both `null` and
`undefined` are mapped back to `None`.  This encoding doesn't support
nested options in a faithful way.

JS-able type can be defined manually by defining `*_to_js` and
`*_of_js` functions.  They can also be created by gen_js_api
automatically when it processed type declarations.

The `Ojs.t` type itself is JS-able.

Arrow can also be used in contexts that expect JS-able types. The JS
function's arity is obtained by counting arrows.  A special case is
when the OCaml arity is 1, with a single argument of type unit, in
which case the JS function is assumed to have no argument.  In order
to define functions that return functions, one can put an arbitrary
attribute on the resulting type:

   ```t1 -> (t2 -> t3 [@foo])```

Without the attribute, such a type would be parsed as a function of
arity 2 (returning type `t3`).


The `unit` type can only be used in specific contexts: as the return
type of functions or methods, or as the unique argument.


Type declarations
-----------------

All type declarations processed by gen_js_api create JS-able types,
i.e.  associated `*_to_js` and `*_to_js` mapping functions.  A
optional "private" modifier is allowed on the type declaration (in the
interface) and dropped from the generated definition (in the
implementation).  Mutually recursive type declarations are supported.


- "Abstract" subtype of `Ojs.t`:

    ````
    type t = private Ojs.t
    ````

  This is used to bind to JS "opaque" objects, with no runtime mapping
  involved when moving between OCaml and JS (mapping functions are the
  identity).

- Type abbreviation:

    ````
    type t = tyexp
    ````

  (formally, abstract types with a manifest).  This assumes that the
  abbreviated type expression is itself JS-able.  Note that the first
  kind of type declaration above (abstract subtypes of `Ojs.t`) are
  a special kind of such declaration, since `abstract` is always dropped
  and `Ojs.t` is JS-able.

- Record declaration:

    ````
    type t = { .... }
    ````

  This assumes that the type for all fields are JS-able.  Fields can
  be mutabled, but polymorphic fields are not yet supported.

  OCaml record values of this type are mapped to JS objects (one
  property per field).  By default, property names are equal to OCaml
  labels, but this can be changed manually with a `[@js]` attribute.

  ````
  type myType = { x : int; y : int [@js "Y"]}
  ````


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
  obtain the propery name.

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



TODOs
-----

- Support sum types, with different possible mappings (configured through
  attributes): int or strings for enums (only constant constructors),
  objects with a discriminator field, etc.

- Support OCaml object types, to wrap JS values (less efficient than
  opaque binding, but sometimes more idiomatic).

- Support really abstract types (treated as `Ojs.t` in the implementation).

- In ppx mode, support typed-index mapping betwen values
  [%%to_js: tyexpr], [%of_js: tyexpr] (returning functions).



About
-----

This package is licensed by LexiFi under the terms of the MIT license.

Contact: alain.frisch@lexifi.com


Currenlty, this package works only on OCaml trunk, not on any released
version.
