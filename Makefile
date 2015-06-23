# The package gen_js_api is released under the terms of an MIT-like license.
# See the attached LICENSE file.
# Copyright 2015 by LexiFi.

VERSION=0.1
# Don't forget to change META file as well

include $(shell ocamlc -where)/Makefile.config

.PHONY: all tests test test_jquery test_js_string_regexp clean


OCAMLFLAGS = -w +A-4-41-45 -warn-error +8
OCAMLC = ocamlc $(OCAMLFLAGS)

JSOO = js_of_ocaml --pretty

all:
	$(OCAMLC) -c ojs.mli ojs.ml
	$(OCAMLC) -c ojs_exn.mli ojs_exn.ml
	$(OCAMLC) -a -o gen_js_api.cma ojs.cmo ojs_exn.cmo
	$(OCAMLC) -I +compiler-libs -o gen_js_api$(EXE) ocamlcommon.cma gen_js_api.mli gen_js_api.ml

tests: all test test_jquery test_js_string_regexp test_js_str test_js_date

test:
	./gen_js_api$(EXE) examples/test_js.mli
	$(OCAMLC) -c -I examples examples/test_js.mli examples/test_js.ml
	$(OCAMLC) -c -I examples -ppx "./gen_js_api$(EXE) -ppx" examples/main.ml
	$(OCAMLC) -no-check-prims -o examples/main$(EXE) gen_js_api.cma examples/test_js.cmo examples/main.cmo
	$(JSOO) -o examples/main.js ojs_runtime.js examples/main$(EXE)

test_jquery:
	./gen_js_api$(EXE) examples/jquery.mli
	$(OCAMLC) -c -I examples examples/jquery.mli examples/jquery.ml
	$(OCAMLC) -c -I examples -ppx "./gen_js_api$(EXE) -ppx" examples/test_jquery.ml
	$(OCAMLC) -no-check-prims -o examples/test_jquery$(EXE) gen_js_api.cma examples/jquery.cmo examples/test_jquery.cmo
	$(JSOO) -o examples/test_jquery.js ojs_runtime.js examples/test_jquery$(EXE)

test_js_str:
	./gen_js_api$(EXE) examples/js_str.mli
	$(OCAMLC) -c -I examples examples/js_str.mli examples/js_str.ml
	$(OCAMLC) -c -I examples examples/test_js_str.ml
	$(OCAMLC) -no-check-prims -o examples/test_js_str$(EXE) gen_js_api.cma examples/js_str.cmo examples/test_js_str.cmo
	$(JSOO) -o examples/test_js_str.js ojs_runtime.js examples/test_js_str$(EXE)

test_js_date:
	./gen_js_api$(EXE) examples/js_date.mli
	$(OCAMLC) -c -I examples examples/js_date.mli examples/js_date.ml
	$(OCAMLC) -c -I examples examples/test_js_date.ml
	$(OCAMLC) -no-check-prims -o examples/test_js_date$(EXE) gen_js_api.cma examples/js_date.cmo examples/test_js_date.cmo
	$(JSOO) -o examples/test_js_date.js ojs_runtime.js examples/test_js_date$(EXE)


clean:
	rm -f *~ gen_js_api$(EXE) *.cm* .*~
	cd examples && rm -f *~ main$(EXE) test_jquery$(EXE) test_js_str$(EXE) test_js_date$(EXE) *.cm* main.js test_js.ml test_jquery.js jquery.ml js_str.ml js_date.ml


INSTALL=META gen_js_api$(EXE) gen_js_api.cma ojs.cmi ojs_runtime.js

install:
	ocamlfind install gen_js_api $(INSTALL)

uninstall:
	ocamlfind remove gen_js_api
