Class wrapping in gen_js_api
============================

gen_js_api can bind Javascript objects into OCaml abstract types with
associated functions (to get/set property and to call methods).  This
form of binding is quite efficient, since the opaque OCaml values are
just the underlying Javascript objects, with no mapping or wrapping.
In addition to that, gen_js_api provides ways to **wrap Javascript
objects into OCaml objects**.  This adds some runtime overhead, but
allows users to use standard OO syntax in OCaml and to rely on
inheritance (to mimic similar hierarchy on the JS side).

In addition to the runtime overhead, wrapping JS objects as OCaml
objects also forces to define all methods at once.  With opaque
bindings, methods of a given JS "class" can be spread over multiple
OCaml modules.  This can be especially useful to mimic the behavior of
JS library addins that extends the library's object prototype with
more methods.




Class wrapping
--------------

An interface processed by js_of_ocaml can define an OCaml class used
to wrap some Javascript objects:

    ````
    class my_class: Ojs.t ->
      object
        inherit Ojs.obj
        (* method declarations *)
        ....
      end
    ````

The class must inherit from `Ojs.obj` directly or indirectly.  This class
simply defines a `to_js` method (returning the underlying `Ojs.t` object).

Such a class declaration produces in the implementation a class
definition with the list of `inherit` clauses (passing the `Ojs.t`
handle to each of them) and a definition for all listed methods.  It
also produces a standard pair of `*_to_js`/`*_of_js` functions (the
`*_to_js` function calls the `to_js` method inherited from `Ojs.obj`,
and `*_of_js` calls the constructor of the class).


Method binding
--------------

- Property getter:

  ````
    method foo: t
       [@@js.get "foo"]
  ````


- Property setter:

  ````
    method set_foo: t -> unit
       [@@js.set "foo"]
  ````


- Method call:

  ````
    method f: t -> unit
      [@@js.call "f"]
  ````


As always, the names can be omitted if they correspond to the implicit
naming scheme.  And as for value bindings, some implicit rules apply,
so that `[@@js.*]` attributes can often be omitted (in particular, in
all the examples above).  The following rules are applied in order:

- If the method is a function with one argument `t -> unit` and its
  name starts with `set_`, then the declaration is assumed to be a
  `[@@js.set]` property setter (on the property whose name is obtained
  by dropping the `set_` prefix).

- If the method is a function, then the definition is assumed to be a
  `[@@js.call]` method call.

- Otherwise, the method is assumed to be a `[@@js.get]` property getter.


Constructors
------------

The default constructor for a class wrapper is necessarily an `Ojs.t` object
(see above).  (Note: it would be easy to allow such classes to take a
value of an arbitrary JS-able type, but this would make it more
difficult to support inheritance.)

It is possible to bind to actual JS constructors declarations such as:

 ````
  class foo: string -> my_class
 ````

Calling this constructor is then implemented by calling the Javascript
constructor of the same name, and wrapping the resulting object with
the `my_class` wrapper.  This is similar to defining:

  ````
    val foo: string -> my_class
      [@@js.new]
  ````

but allows writing `new foo(...)` instead of `foo(...)`.

A custom name can be provided with a `[@@js.new]` attribute:

 ````
  class foo: string -> my_class
    [@@js.new "MyConstr"]
 ````
