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
  [@@js.call "JavaScriptMethodName"]
  ```

- Function Application

  ```ocaml
  val apply: t -> T1 -> ... -> Tn
  [@@js.apply]
  ```

  Calling the function on a first argument `f` of type `t` corresponds
  to calling the underlying JS function object directly, with
  other arguments passed to it.

  This is particularly useful when binding to a "callable" JS object (an object that is also a function), or [a function type in a TypeScript interface](https://www.typescriptlang.org/docs/handbook/interfaces.html#function-types).

  The name of the function need not necessarily be `apply` as long as the `[@@js.apply]` attribute is present.

  When the function you want to bind is a "newable" one (a function that must be called with a prefix `new`, e.g. constructors), use `[@@js.apply_newable]` instead. This is especially useful to bind to constructor interfaces in TypeScript.

  ```ocaml
  module FooConstructor: sig
    type t
    val new_: t -> Foo.t [@@js.apply_newable]
  end
  val fooConstructor: FooConstructor.t [@@js.global "Foo"]
  ```

  When the "callable" object you want to bind to is a global object, the `[@@js.invoke]`
  attribute along with the `[@js.scope]` attribute (see below) may be used to call it.

  For instance, you can write
  ```ocaml
    module[@js.scope "JavaScriptClassName"] C : sig
      val invoke: T1 -> ... -> Tn -> t [@@js.invoke]
    end

    (* usage *)
    let x = C.invoke arg1 ... argn
  ```
  instead of
  ```ocaml
    module C : sig
      type t
      val apply: t -> T1 -> ... -> Tn -> t [@@js.apply]
    end
    val c: C.t [@@js.global "JavaScriptClassName"]

    (* usage *)
    let x = C.apply c arg1 ... argn
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
  [@@js.new "JavaScriptClassName"]
  ```
  As for global values, it is possible to indicate the access path by
  using `[@js.scope]` attributes on englobing modules (see below).

  When the global object is itself an object constructor, the `[@@js.create]`
  attribute may be used to instantiate it.

  For instance,
  ```ocaml
    module[@js.scope "JavaScriptClassName"] C : sig
      val create: T1 -> ... -> Tn -> t [@@js.create]
    end
  ```
  is the same as
  ```ocaml
    module C : sig
      val create: T1 -> ... -> Tn -> t [@@js.new "JavaScriptClassName"]
    end
  ```

- Global value or function:

  ```ocaml
  val x: t
  [@@js.global]
  ```

  This creates an OCaml value that corresponds to a globally accessible
  JavaScript value.  This is used to access both global objects (e.g.
  the `window` object) and global functions (e.g. `alert`).  It is also
  possible to specify a custom name for the JavaScript variable:

  ```ocaml
  val x: t
  [@@js.global "JavaScriptValueName"]
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

- Index getter

  ```ocaml
  val get: t -> index -> T option
  [@@js.index_get]
  ```

  Corresponds to getting from an index accessor or [an index signature in a TypeScript interface](https://www.typescriptlang.org/docs/handbook/interfaces.html#indexable-types).

  The return type may be `T` or `T option`, depending on whether the property is optional or not.

  The name of the function need not necessarily be `get` as long as the `[@@js.index_get]` attribute is present.

  `index` must be `int`, `string`, or abstract types holding a JavaScript `number` or `string` value.

- Index setter

  ```ocaml
  val set: t -> index -> T -> unit
  [@@js.index_set]
  ```
  Corresponds to setting to an index accessor or [an index signature in a TypeScript interface](https://www.typescriptlang.org/docs/handbook/interfaces.html#indexable-types).

  The name of the function need not necessarily be `set` as long as the `[@@js.index_set]` attribute is present.

  `index` must be `int`, `string`, or abstract types holding a JavaScript `number` or `string` value.


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
  type `t2`, going through the JavaScript representation (i.e.
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

Calling a function/constructor by different means
-------------------------------------------------

|                                     | as a function          | as a constructor       |
|-------------------------------------|------------------------|------------------------|
| call the first argument             | `[@@js.apply]`         | `[@@js.apply_newable]` |
| call the global object              | `[@@js.invoke]`        | `[@@js.create]`        |
| call a member of the first argument | `[@@js.call "methodName"]`   | N/A                    |
| call a member of the global object  | `[@@js.global "funcName"]` | `[@@js.new "ClassName"]`    |

Scope
-----

The signature attribute `[@@@js.scope "property"]` changes the reference to the current global
object by following the property provided as payload. Nested scopes work as if the
access path were composed by concatenation of all the names indicated by [@js.scope]
attribute, separated by a '.'.

A simple use case is to bind to JavaScript values packed in singleton objects or classes.

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

Before version 1.0.7, the presence of `[@@js.scope]` used to change
the behavior of automatic bindings. It is no longer the case.

An experimental feature also allows to pass an expression of type `Ojs.t` as
a payload to replace the global object. The intended use case is to allow
dynamic loading of modules.

Automatic binding (Deprecated since 1.0.7)
------------------------------------------

Some conventions, based on the declared value names and their types,
allow to infer implicitly the `[@@js.xxx]` attributes on value
declarations in most cases.

*This feature has been deprecated starting from version 1.0.7*. All values
declaration should be annotated with an explicit attribute. Otherwise
a preprocessor warning will be emitted.

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

- If the value is a function returning `unit` with three arguments
  whose first argument is a named type `τ -> τ2 -> τ3 -> unit` and the
  name is `set`, then the declaration is assumed to be a
  `[@@js.set_index]` index setter.

- If the value is a function with two arguments `τ1 -> τ2 -> unit` and
  its name starts with `set_`, then the declaration is assumed to be a
  `[@@js.set]` property setter (on the property whose name is obtained
  by dropping the `set_` prefix).

- If the value is a function with a single argument (named type) `τ ->
  unit`, then the declaration is assumed to be a `[@@js.call]` method
  call.

- If the value is a function with two arguments whose first argument is
  a named type `τ -> τ2 -> τ3` (and `τ3` is not `unit`) and the name is
  `get`, then the declaration is assumed to be a `[@@js.index_get]`
  index getter.

- If the value is a function with a single argument (named type) `τ ->
  τ2` (and `τ2` is not `unit`), then the declaration is assumed to be
  a `[@@js.get]` property getter.

- If the value is a function with a single argument `unit -> τ2`, then
  the declaration is assumed to be a `[@@js.get]` global getter.

- If the value is a function whose first argument is a named type `τ
  -> ...` and the name is `apply`, then the definition is assumed to
  be a `[@@js.apply]` function object application.

- If the value is a function whose first argument is a named type `τ
  -> ...` (and the name is not `apply`), then the definition is assumed
  to be a `[@@js.call]` method call.

- Otherwise, the declaration is assumed to be a `[@@js.global]` value.
  This applies in particular for any non-functional type.
