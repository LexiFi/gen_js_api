gen_js_api: installation and usage instructions
===============================================



Currenlty, this package works only on OCaml trunk, not on any released
version.  The package is itself not released.


Installation (with OPAM)
------------------------

gen_js_api currently relies on the trunk version of OCaml and the development
version of js_of_ocaml (and since this development version doesn't install,
I suggest to use a fork as below):

   ```
   opam switch 4.03.0+trunk
   opam pin add js_of_ocaml git@github.com:alainfrisch/js_of_ocaml.git
   opam pin add gen_js_api git@github.com:alainfrisch/gen_js_api.git
   ```



Usage (with ocamlfind)
----------------------

 - Invoking the [standalone tool](IMPLGEN.md) (`.mli` -> `.ml` generator):

   ```
   ocamlfind gen_js_api/gen_js_api my_unit.mli
   ```

 - Compiling binding (`.mli` and generated `.ml` files) and user
   code which rely on the `Ojs` module:

   ```
   ocamlfind ocamlc -package gen_js_api my_unit.mli
   ocamlfind ocamlc -package gen_js_api my_unit.ml
   ```

 - Compiling with the [ppx processor](PPX.md):

   ```
   ocamlfind ocamlc -c -package gen_js_api.ppx my_prog.ml
   ```

 - Linking the bytecode program:

   ```
   ocamlfind ocamlc -o my_prog -no-check-prims -package gen_js_api -linkpkg ...
   ```

 - Compiling into Javascript:

   ```
   js_of_ocaml -o my_prog.js +gen_js_api/ojs_runtime.js my_prog
   ```

