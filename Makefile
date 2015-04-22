# The package gen_js_api is released under the terms of an MIT-like license.
# See the attached LICENSE file.
# Copyright 2015 by LexiFi.

OCAMLFLAGS = -w +A-4-9-41-45
OCAMLC = ocamlfind ocamlc $(OCAMLFLAGS)

all: build example

build:
	$(OCAMLC) -c -package js_of_ocaml ojs.mli ojs.ml
	$(OCAMLC) -package compiler-libs.bytecomp -linkpkg -o gen_js_api.exe gen_js_api.ml

example:
	./gen_js_api.exe examples/test_js.mli > examples/test_js.ml
	$(OCAMLC) -c -I examples examples/test_js.mli examples/test_js.ml
	$(OCAMLC) -c -I examples -ppx "./gen_js_api.exe -ppx" examples/main.ml
	$(OCAMLC) -package js_of_ocaml -linkpkg -o examples/main.exe ojs.cmo examples/test_js.cmo examples/main.cmo
	js_of_ocaml -o examples/main.js examples/main.exe

clean:
	rm -f *~ *.exe *.cm* .*~
	cd examples && rm -f *~ *.exe *.cm* main.js test_js.ml
