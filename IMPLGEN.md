gen_js_api: generate implementations from interfaces
====================================================

The primary operating mode for gen_js_api is to generate .ml
implementation from annotated .mli interfaces.  These interfaces must
follow a certain shape.  They describe both the Javascript components
to be imported and how they should be reflected within OCaml.

Usage
-----


```
  $ gen_js_api my_module.mli
```

or with findlib:

```
  $ ocamlfind gen_js_api/gen_js_api my_module.mli
```

This generates my_module.ml.



Supported declarations
----------------------

Interfaces processed by gen_js_api can currently contain:

  - [Type declarations](TYPES.md):

    ````
    type t = ...
    ````

    See [this page](TYPES.md) for a description of supported types.
    Such a type declaration produces in the implementation an identical
    defininition, and associated `*_to_js` and `*_of_js` functions
    (which can be manually exported if needed).


  - [Value declarations](VALUES.md):

    ````
    val f: tyexpr
    ````

    This produces in the implementation a definition for such a value,
    whose content depends on three elements: the name of the value
    (`f` in the example), its declared type (`tyexpr`), and possible
    `[@@js.xxx]` attributes attached to the declaration in the interface.

    See [this page] (VALUES.md) for supported forms of value declarations.


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

  - [Class declarations](CLASSES.md)



Verbatim sections
-----------------

A floating attribute `[@@@js.stop]` tells the tool to ignore the
remaining items until the end of the current (possibly nested)
signature.  This can be reverted with a floating attribute
`[@@@js.start]`.  This system makes it possible to specify fragments
of the interface that should not generate any code in the
implementation.

A floating `[@@@js.implem ...]` tells the tool to generate some custom
code in the implementation. The payload `...` is an OCaml structure,
which is processed in the same way as in [ppx mode](PPX.md).


Example:

```ocaml

  [@@@js.stop]
     val foo: int -> unit
  [@@@js.start]

  [@@@js.implem

       val foo_internal: string -> int -> unit
         [@@js.global "foo"]
       let foo = foo_internal ""

  ]
```


For the common case where verbatim sections are used to create custom
value bindings, a `[@@js.custom]` attribute can be applied to a `val`
declaration.  The effect is that the `val` declaration itself is ignored
(nothing is generated in the implementation), and a structure can be
provided as the payload of the attribute.  The example above is equivalent
to:

```ocaml
  val foo: int -> int
  [@@js.custom
       val foo_internal: string -> int -> unit
         [@@js.global "foo"]
       let foo = foo_internal ""
  ]
```

and to:

```ocaml
  val foo: int -> int
  [@@js.custom]

  [@@js.implem
        ...
  ]
```
