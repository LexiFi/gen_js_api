Value bindings in gen_js_api
============================


Supported forms
---------------

- Method call:

  ```ocaml
  val myMethod: t -> T1 -> ... -> Tn -> T
  [@@js.meth]
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
  val myMethod: t -> T1 -> ... -> Tn -> T
  [@@js.meth "JavascriptMethodName"]
  ```

  A special case is when there is a single argument (in addition to
  the object itself) of type `unit`.  This is interpreted as a JS
  method with no argument at all.

- Object creation:

  ```ocaml
  val new_myClass: T1 -> ... -> Tn -> t
  [@@js.new]
  ```

  Corresponds to creating an object of class `myClass` with other
  arguments passed to it.

  By default, the name of the class on the JS side is derived from the
  name of the OCaml value (`myClass` above): in this case, the value
  name must start with the `new_` prefix which is dropped to obtain
  the class name.  It is also possible to specify a custom name
  explicitly.

  ```ocaml
  val new_myClass: T1 -> ... -> Tn -> t
  [@@js.new "JavascriptClassName"]
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
  [@@js.get "MypProp"]
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

- Cast

  ```ocaml
  val cast: t1 -> t2
  [@@js.cast]
  ```

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

- Similarly, if the type has the form `Ojs.t -> t` (for a local named
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
  then the definition is assumed to be a `[@@js.meth]` method call.

- Otherwise, the declaration is assumed to be a `[@@js.global]` value.
  This applies in particular for any non-functional type.


