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
  name of the OCaml value (`MyClass` above): in this case, the value
  name must start with the `new_` prefix which is dropped and the
  remaining name is capitalize to obtain the class name.  It is
  also possible to specify a custom name explicitly.

  ```ocaml
  val f: T1 -> ... -> Tn -> t
  [@@js.new "JavascriptClassName"]
  ```
  As for global values, it is possible to indicate the access path by
  using `[@js.scope]` attributes on englobing modules (see below).

  When the global object is itself an object constructor, the `[@@js.create]`
  attribute may be used to instantiate it.

  For instance,
  ```ocaml
    module[@js.scope "JavascriptClassName"] C : sig
      val create: T1 -> ... -> Tn -> t [@js.create]
    end
  ```
  is the same as
  ```ocaml
    module C : sig
      val create: T1 -> ... -> Tn -> t [@js.new "JavascriptClassName"]
    end
  ```

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
  `[@js.scope]` attribute on englobing modules (see the Scope section).

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

Scope
-----

The signature attribute `[@@@js.scope "property"]` change the reference to the current global
object by following the property provided as payload. They can be nested.

The simple use case is to binds values packed in singleton objects or classes.

For instance,

```ocaml
  module[@js.scope "console"] Console: sig
    val log: string -> unit [@@js.global]
  end
```

is equivalent to

```ocaml
  module Console: sig
    val log: string -> unit [@@js.global "console.log"]
  end
```

When attached directly to a module, the payload of `[@@js.scope]`
may be omitted, it will be implicitly filled with the module name
(preserving the capitalization !).

Before version 1.0.7, the presence of `[@@js.scope]` change
the behavior of automatic bindings. It is no longer the case.

An experimental feature also allows to pass an expression of type `Ojs.t` as
a payload to replace the global object. The intended use case is to allow
dynamic loading of modules.

Automatic binding
-----------------

Some conventions, based on the declared value names and their types,
allow to inter implicitly the `[@@js.xxx]` attributes on value
declarations in most cases.

There are three modes that controls the heuristic:
  1. The *default* mode,
  2. The *static* mode,
  3. And the *prototype* mode.

The signature attributes [@@@js.auto], [@@@js.prototype] and
[@@@js.prototype] are used to switch mode inside a module.
All toplevel modules start in the *default* mode.

Note that in all modes the declaration of conversion functions generated
from types are ignored in order to expose the generated functions.

This means all value of declarations of the form:
```ocaml
val τ_to_js: ... -> Ojs.t
```
or the form
```ocaml
val τ_of_js: ... -> τ
```

### Rules for the *default* mode

The rules are applied in order:

- If the value is a function whose result is a named type `... -> τ`
  and the name is `create`, then the declaration is assumed to
  be a `[@@js.create]` object creation.

- If the value is a function whose result is a named type `... -> τ`
  and its name starts with `new_`, then the declaration is assumed to
  be a `[@@js.new]` object creation (on the class whose name is
  obtained by dropping the `new_`prefix).

- If the value is a function with a single argument `τ1 -> unit` and
  its name starts with `set_`, then the declaration is assumed to be a
  `[@@js.set]` global setter (whose name is obtained by dropping the
  `set_` prefix).

- If the value is a function with two arguments `τ1 -> τ2 -> unit` and
  its name starts with `set_`, then the declaration is assumed to be a
  `[@@js.set]` property setter (on the property whose name is obtained
  by dropping the `set_` prefix).

- If the value is a function with a single argument (named type) `τ ->
  unit`, then the declaration is assumed to be a `[@@js.call]` method
  call.

- If the value is a function with a single argument (named type) `τ ->
  τ2` (and `τ2` is not `unit`), then the declaration is assumed to be
  a `[@@js.get]` property getter.

- If the value is a function with a single argument `unit -> τ2`, then
  the declaration is assumed to be a `[@@js.get]` global getter.

- If the value is a function whose first argument is a named type `τ
  -> ...`, then the definition is assumed to be a `[@@js.call]` method
  call.

- Otherwise, the declaration is assumed to be a `[@@js.global]` value.
  This applies in particular for any non-functional type.

### Rules for the *static* mode

This mode is used to generate bindings depending on the "current"
global object (see the Scope section).

- If the value is a function whose result is a named type `... -> τ`
  and the name is `create`, then the declaration is assumed to
  be a `[@@js.create]` object creation.

- If the value is a function whose result is a named type `... -> τ`
  and its name starts with `new_`, then the declaration is assumed to
  be a `[@@js.new]` object creation (on the class whose name is
  obtained by dropping the `new_`prefix).

- If the value is a function with a single argument `τ1 -> unit` and
  its name starts with `set_`, then the declaration is assumed to be a
  `[@@js.set]` global setter (whose name is obtained by dropping the
  `set_` prefix).

- Otherwise, the declaration is assumed to be a `[@@js.global]` value.

### Rules for the *prototype* mode

In a *prototype* section, the generated code is not allowed to depend
on the current global object (or an error will be emitted). Therefore
such sections are guaranteed not to depend on the current scope.

The rule in this mode are:

- If the value is a function whose result is a named type `... -> τ`
  and its name starts with `new_`, then the declaration is assumed to
  be a `[@@js.new]` object creation (on the class whose name is
  obtained by dropping the `new_`prefix).

- If the value is a function with two arguments `τ1 -> τ2 -> unit` and
  its name starts with `set_`, then the declaration is assumed to be a
  `[@@js.set]` property setter (on the property whose name is obtained
  by dropping the `set_` prefix).

- If the value is a function with a single argument (named type) `τ ->
  unit`, then the declaration is assumed to be a `[@@js.call]` method
  call.

- If the value is a function with a single argument (named type) `τ ->
  τ2` (and `τ2` is not `unit`), then the declaration is assumed to be
  a `[@@js.get]` property getter.

- If the value is a function whose first argument is a named type `τ
  -> ...`, then the definition is assumed to be a `[@@js.call]` method
  call.

- Otherwise, an error will be emitted.
