# The package gen_js_api is released under the terms of an MIT-like license.
# See the attached LICENSE file.
# Copyright 2015 by LexiFi.

OCAMLFLAGS = -w +A-4-9-41-45
OCAMLC = ocamlc $(OCAMLFLAGS)

JSOO_PATH = ~/js_of_ocaml
JSOO = $(JSOO_PATH)/compiler/js_of_ocaml --pretty --noruntime $(JSOO_PATH)/runtime/runtime.js

all: build example

build:
	$(OCAMLC) -c ojs.mli ojs.ml
	$(OCAMLC) -I +compiler-libs -o gen_js_api.exe ocamlcommon.cma gen_js_api.ml

example:
	./gen_js_api.exe examples/test_js.mli
	$(OCAMLC) -c -I examples examples/test_js.mli examples/test_js.ml
	$(OCAMLC) -c -I examples -ppx "./gen_js_api.exe -ppx" examples/main.ml
	$(OCAMLC) -no-check-prims -o examples/main.exe ojs.cmo examples/test_js.cmo examples/main.cmo
	$(JSOO) -o examples/main.js ojs_runtime.js examples/main.exe

clean:
	rm -f *~ *.exe *.cm* .*~ primitives.lst
	cd examples && rm -f *~ *.exe *.cm* main.js test_js.ml
