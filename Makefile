# The package gen_js_api is released under the terms of an MIT-like license.
# See the attached LICENSE file.
# Copyright 2015 by LexiFi.

OCAMLFLAGS = -w +A-4-9-41-45
OCAMLC = ocamlfind ocamlc $(OCAMLFLAGS)
OCAMLRUN = ocamlrun

all: build example

build:
	$(OCAMLC) -c ojs.mli ojs.ml
	$(OCAMLC) -package compiler-libs.bytecomp -linkpkg -o gen_js_api.exe gen_js_api.ml

example: primitives.lst
	./gen_js_api.exe examples/test_js.mli > examples/test_js.ml
	$(OCAMLC) -c -I examples examples/test_js.mli examples/test_js.ml
	$(OCAMLC) -c -I examples -ppx "./gen_js_api.exe -ppx" examples/main.ml
	$(OCAMLC) -use-prims primitives.lst -linkpkg -o examples/main.exe ojs.cmo examples/test_js.cmo examples/main.cmo
	js_of_ocaml -o examples/main.js ojs_runtime.js examples/main.exe

PRIMITIVES=
PRIMITIVES+=caml_js_typeof
PRIMITIVES+=caml_js_to_string
PRIMITIVES+=caml_js_get
PRIMITIVES+=caml_js_new
PRIMITIVES+=caml_js_var
PRIMITIVES+=caml_js_object
PRIMITIVES+=caml_js_set
PRIMITIVES+=caml_js_fun_call
PRIMITIVES+=caml_js_meth_call
PRIMITIVES+=caml_js_wrap_callback
PRIMITIVES+=caml_js_from_bool
PRIMITIVES+=caml_js_to_bool
PRIMITIVES+=caml_js_from_string
PRIMITIVES+=caml_js_equals
PRIMITIVES+=caml_pure_js_expr
PRIMITIVES+=caml_ojs_wrap_fun_arguments
PRIMITIVES+=caml_ojs_iterate_properties

primitives.lst: Makefile
	$(OCAMLRUN) -p > $@
	for p in $(PRIMITIVES); do echo $$p >> $@; done

clean:
	rm -f *~ *.exe *.cm* .*~ primitives.lst
	cd examples && rm -f *~ *.exe *.cm* main.js test_js.ml
