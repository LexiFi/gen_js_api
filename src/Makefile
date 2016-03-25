# The package gen_js_api is released under the terms of an MIT-like license.
# See the attached LICENSE file.
# Copyright 2015, 2016 by LexiFi.

ROOT=..
include $(ROOT)/Makefile.common

.PHONY: all clean install uninstall doc

all:
	$(OCAMLC) -c ojs.mli ojs.ml
	$(OCAMLC) -c ojs_exn.mli ojs_exn.ml
	$(OCAMLC) -a -o gen_js_api.cma ojs.cmo ojs_exn.cmo
	$(OCAMLC) -I +compiler-libs -o gen_js_api$(EXE) ocamlcommon.cma gen_js_api.mli gen_js_api.ml

clean:
	rm -f *~ gen_js_api$(EXE) *.cm* .*~

doc:
	rm -rf doc
	mkdir doc
	ocamldoc -d doc -html ojs.mli ojs_exn.mli

# ocamlfind installation

INSTALL = \
  META \
  gen_js_api$(EXE) \
  gen_js_api.cma \
  ojs.cmi ojs_exn.cmi \
  ojs_runtime.js

install:
	ocamlfind install gen_js_api $(INSTALL)

uninstall:
	ocamlfind remove gen_js_api
