gen_js_api: ppx mode
====================

While the primary mode of operation for gen_js_api is to generate an
.ml file from an annotated .mli file, it is also possible to use it as
a ppx preprocessor on an .ml file directly to insert local JS bindings.

The `-ppx` command-line option must be the first argument passed
to gen_js_api to enable the ppx mode:

````
   $ ocamlc -c -ppx "gen_js_api -ppx" ...
````


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

 - `[%js.to: ty]` and `[%js.of: ty]` extensions on expressions.

   Example:

   ````
     let x : Ojs.t = [%js.of: int list]  [ 10; 20; 30 ]
   ````

   This form generates the mapping function associated to a JS-able type.
