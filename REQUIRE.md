Value bindings using required object in gen_js_api
=========================================================

It happens sometime that, when writing bindings of js library, there
is a need to require some modules before using it.

```js
const myModule = require('myModule')
/* ... */
```

There is a support to automatically write bindings and to use the
required object instead of using the global object.

The _require_ attribute
--------------------------

### Global ###

The attribute `[@@@js.require "myModule"]` indicate to subsequents
declarations to use the require module **myModule** during the
generation phase. This global attribute requires the module name, it
cannot be inferred.

### Local to a module ###

It's also permitted to attach the require attribute to modules.

```ocaml
module MyModule : sig
  ...
end [@@js.require]
```

By default, the name of the module is used as the name of the js
module to require.

### Associated to a variable ###

To use bindings rules describe in [Value section](VALUES.md), it's
possible to put the annotation to a simple variable.

```ocaml
type t = private Ojs.t

val module_t : t Lazy.t [@@js.require "myModule"]

val get_x : t -> int
```

###### Nested requires ######

Actually, it's not possible to nest requires.

How is it translated ?
-------------------------

First, it creates a definition which call lazily the requires
function:

```ocaml
let (_require : Ojs.t Lazy.t) = lazy (Ojs.require "...")
```

Then, wherever the normal binding starts the access from the global
object, the require annotation modifies by forcing the lazy value.
For example, a method: `val foo: int -> string [@@js.call]`, is
translated to:

```ocaml
let (foo : int -> string) =
  fun x13 ->
    Ojs.string_of_js
      (Ojs.call (Lazy.force _require) "foo" [|(Ojs.int_to_js x13)|])
```


Automatic binding
-------------------

A declaration under a require context also follow conventions as
explain in the [Value section](VALUES.md). The rules are quite
similar except that the explicit type  `t` is not required anymore.
Here are the rules:

- If the type has the form `t -> Ojs.t` and the value name is
  `t_to_js`, it's still assumed to be a `[@@js.cast]`.
  
- If the type has the form `Ojs.t -> t` and the value name is
  `t_of_js`, it's still assumed to be a `[@@js.cast]`.
  
- If the value is a function with a unit argument `unit -> t`
  (and `t` is not `unit`), then the declaration is assumed to be
  a `[@@js.get]` property getter.

- If the value is a function with a single argument (named typed)
  `t -> unit` and its name starts with `set_`, then the declaration is
  assumed to be a `[@@js.set]` property setter (on the property whose
  name is obtained by dropping the `set_` prefix).

- Otherwise, the declaration is assumed to be a `[@@js.call]` value.


The scope attribute
----------------------

When js static objects are nested, there is a need to write nested
modules in the ocaml code. To indicate when it is necessary to access
a sub-object, the `[@@js.scope]` attribute is required.

```
module MyObject : sig
  val my_method : a -> b -> c
  module MySubObject : sig
    val my_other_method : a -> b -> c
  end [@@js.scope "mySubObject"]
end [@@js.require]
```

Without the scope attribute, the method `my_other_method` in module
`MySubObject` would be translated as if it were
`require('MyObject').myOtherMethod(a, b);`. The scope attribute change
this behaviour, by translating the same method as if it were following
javascript call: `require('MyObject').mySubObject.myOtherMethod(a, b);`
