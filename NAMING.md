gen_js_api: default naming convention
=====================================

Javascript names corresponding to bound components can always be
specified explicitly (with the use of attributes).  When the naming is
left implicit, a Javascript name is automatically derived from the
OCaml name by applying the following rules:

  1. uppercasing every character following an underscore;

  2. removing every underscore;

  3. uppercasing the first character when generating object constructor names.

This automatic naming convention can be partially disabled by adding
an attribute `[@js.verbatim_names]` on outer structures. When the attribute
`[@js.verbatim_names]` is inherited from the context, the rule 1 and 2 are
disabled.

For instance,

```ocaml
type myType = { x_coord : int; y_coord : int [@js "Y"]}
```

is mapped to a JS record with two fields named "xCoord" and "Y" whereas

```ocaml
type myType = { x_coord : int; y_coord : int [@js "Y"]} [@@js.verbatim_names]
```

is mapped to a JS record with two fields named "x_coord" and "y".
