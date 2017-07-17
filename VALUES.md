Value bindings in gen_js_api
============================


Supported forms
---------------

- Method call:

  ```ocaml
  val my_method: t -> T1 -> ... -> Tn -> T
  [@@js.call]
  ```

  Calling the function on a first argument `o` of type `t` corresponds
  to calling the method `myMethod` on the underlying JS object, with
  other arguments passed to it.

  By default, the name of the method on the JS side is derived from
  the name of the OCaml value (`myMethod` above).  It is possible to
  specify a custom name explicitly, for instance for cases where the
  JS name is not a valid OCaml (lowercase-)identifier, or to support
  overloading (exposing multiple OCaml functions that correspond to
  different types given to the same JS method):


  ```ocaml
  val my_method: t -> T1 -> ... -> Tn -> T
  [@@js.call "JavascriptMethodName"]
  ```


- Object constructor:

  ```ocaml
  val new_my_class: T1 -> ... -> Tn -> t
  [@@js.new]
  ```

  Corresponds to calling a JS constructor with arguments
  passed to it.

  By default, the name of the class on the JS side is derived from the
  name of the OCaml value (`myClass` above): in this case, the value
  name must start with the `new_` prefix which is dropped to obtain
  the class name.  It is also possible to specify a custom name
  explicitly.

  ```ocaml
  val f: T1 -> ... -> Tn -> t
  [@@js.new "JavascriptClassName"]
  ```
  As for global values, it is possible to indicate the access path by
  using `[@js.scope]` attributes on englobing modules (see below).

- Global value or function:

  ```ocaml
  val x: t
  [@@js.global]
  ```

  This creates an OCaml value that corresponds to a globally accessible
  Javascript value.  This is used to access both global objects (e.g.
  the `window` object) and global functions (e.g. `alert`).  It is also
  possible to specify a custom name for the Javascript variable:

  ```ocaml
  val x: t
  [@@js.global "JavascriptValueName"]
  ```

  Example:
  ```ocaml
  val alert: string -> unit
  [@@js.global]
  ```

  By default, a global value or function is taken from the global
  object. However, it is possible to specify an access path by using
  `[@js.scope]` attribute on englobing modules. The access path is
  then composed by concatenation of all the names indicated by
  `[@js.scope]` attribute, separated by a '.'.

  For instance,

  ```ocaml
  module Console: sig
    val log: string -> unit [@@js.global]
  end [@js.scope "console"]
  ```

  is equivalent to

  ```ocaml
  module Console: sig
    val log: string -> unit [@@js.global "console.log"]
  end
  ```

- Property getter

  ```ocaml
  val prop: t -> T
  [@@js.get]
  ```

  Calling the function on a first argument `o` of type `t` corresponds
  to getting the `prop` property of the underlying JS object. A custom
  name for the JS property can be specified:

  ```ocaml
  val get_property: t -> T
  [@@js.get "MyProp"]
  ```


- Property setter

  ```ocaml
  val set_prop: t -> T -> unit
  [@@js.set]
  ```

  Calling the function on a first argument `o` of type `t` corresponds
  to setting the `prop` property of the underlying JS object.  Note that
  the value name must start with the `set_` prefix, which is dropped to
  obtain the property name.

  A custom name for the JS property can also be specified (in which
  case the name of the value can be arbitrary):

  ```ocaml
  val modify_prop: t -> T -> unit
  [@@js.set "prop"]
  ```

- Global getter

  ```ocaml
  val get_x: unit -> T
  [@@js.get "x"]

  val get_sub_x: unit -> T
  [@@js.get "MyObject.x"]
  ```

  This creates a function which returns the current value of a
  global variable or of a (possibly nested) inner field of a global variable.

  As for global values, it is possible to indicate the access path by
  using `[@js.scope]` attributes on englobing modules.

- Global setter

  ```ocaml
  val set_x: T -> unit
  [@@js.set "x"]

  val set_sub_x: T -> unit
  [@@js.set "MyObject.x"]
  ```

  This creates a function which sets the value of a
  global variable or of a (possibly nested) inner field of a global variable.

  As for global values, it is possible to indicate the access path by
  using `[@js.scope]` attributes on englobing modules.

- Cast

  ```ocaml
  val cast: t1 -> t2
  [@@js.cast]
  ```

  Calling this function performs an unchecked cast from type `t1` to
  type `t2`, going through the Javascript representation (i.e.
  applying mapper from `t1` to the underlying JS object, and back
  using the mapper for `t2`).


- Literal object builder:

  ```ocaml
  val make: l1:T1 -> ... -> ln:tn -> t
  [@@js.builder]
  ```

  Corresponds to creating a JS plain object with fields initialized
  with the provided values.  The name of the function (`make` in the
  example) does not correspond to any concept in JS.  By default, the
  JS field names are derived from OCaml labels, but it is also
  possible to override that with a `[@js]` attribute on the argument's
  type.  All fields must be labeled or optional, or come with such an
  attribute.

  Optional arguments (but not non-optional argument with optional
  type) are treated in a special way: no field is created in the JS
  object if the parameter is not provided on the call site (without
  this special behavior, the treatment would be to set the field to
  `null`, which is the encoding of `None`).

  Example:

  ```ocaml
  type t  = private Ojs.t

  val mk: ?children:t list -> age:int -> (string[@js "name"]) -> t
  [@@js.builder]
  ```


- Custom binding:

  ```ocaml
  val f: ...
    [@@js.custom
          let f = ...
    ]
  ```

  The val declaration itself doesn't produce anything in the
  implementation.  Instead, custom OCaml code that goes into the
  implementation must be provided explicitly.

  See [Verbatim section](IMPLGEN.md) for more details and examples.


Automatic binding
-----------------

Some conventions, based on the declared value names and their types,
allow to get rid of the explicit `[@@js.xxx]` attributes on value
declarations in most cases.  Here are the rules, applied in order:

- If the type has the form `τ -> Ojs.t` (for a local named type `τ`) and
  the value name is `τ_to_js` (i.e. the type name followed by `_to_js`),
  then the function is assumed to be a `[@@js.cast]`.  This is used
  to expose the `_to_js` function generated automatically by the tool
  for a type declaration.

- Similarly, if the type has the form `Ojs.t -> τ` (for a local named
  type `τ`) and the value name is `τ_of_js` (i.e. the type name
  followed by `_of_js`), then the function is assumed to be a
  `[@@js.cast]`.

- If the value is a function with a single argument `τ1 -> unit` and
  its name starts with `set_`, then the declaration is assumed to be a
  `[@@js.set]` global setter (whose name is obtained by dropping the
  `set_` prefix).

- If the value is a function with a single argument (named type) `τ ->
  unit`, then the declaration is assumed to be a `[@@js.call]` method
  call, unless a `[@js.scope]` attribute has been defined in an
  englobing module. In this latter case, the value is assumed to be a
  `[@@js.global]` value.

- If the value is a function with a single argument (named type) `τ ->
  τ2` (and `τ2` is not `unit`), then the declaration is assumed to be
  a `[@@js.get]` property getter.

- If the value is a function with a single argument `unit -> τ2`, then
  the declaration is assumed to be a `[@@js.get]` global getter.

- If the value is a function with two arguments `τ1 -> τ2 -> unit` and
  its name starts with `set_`, then the declaration is assumed to be a
  `[@@js.set]` property setter (on the property whose name is obtained
  by dropping the `set_` prefix).

- If the value is a function whose result is a named type `... -> τ`
  and its name starts with `new_`, then the declaration is assumed to
  be a `[@@js.new]` object creation (on the class whose name is
  obtained by dropping the `new_`prefix).

- If the value is a function whose first argument is a named type `τ
  -> ...`, then the definition is assumed to be a `[@@js.call]` method
  call, unless a `[@js.scope]` attribute has been defined in an
  englobing module. In this latter case, the value is assumed to be a
  `[@@js.global]` value.

- Otherwise, the declaration is assumed to be a `[@@js.global]` value.
  This applies in particular for any non-functional type.
