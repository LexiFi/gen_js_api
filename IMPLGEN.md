gen_js_api: generate implementations from interfaces
====================================================

The primary operating mode for gen_js_api is to generate .ml
implementation from annotated .mli interfaces.  These interfaces must
follow a certain shape.  They describe both the Javascript components
to be imported and how they should be reflected within OCaml.

Usage
-----


````
  $ gen_js_api my_module.mli > my_module.ml
````



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


