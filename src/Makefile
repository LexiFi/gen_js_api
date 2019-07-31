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

	$(OCAMLC) -c -I +compiler-libs -I ppx/lib/ ppx/lib/gen_js_api_ppx.mli ppx/lib/gen_js_api_ppx.ml
	$(OCAMLC) -a -I +compiler-libs -o ppx/lib/gen_js_api_ppx.cma ppx/lib/gen_js_api_ppx.cmo
	$(OCAMLC) -c -I +compiler-libs -I ppx/lib/ -I ppx/bin/ ppx/bin/gen_js_api.mli ppx/bin/gen_js_api.ml
	$(OCAMLC)    -I +compiler-libs -I ppx/bin/ -I ppx/lib/ -o ppx/bin/gen_js_api$(EXE) ocamlcommon.cma gen_js_api_ppx.cma ppx/bin/gen_js_api.cmo



clean:
	rm -f *~ ppx/lib/*~ ppx/bin/*~
	rm -f .*~ ppx/lib/.*~ ppx/bin/.*~
	rm -f *.cm* ppx/lib/*.cm* ppx/bin/*.cm*
	rm -f ppx/bin/gen_js_api$(EXE)
	rm -rf doc

doc:
	rm -rf doc
	mkdir doc
	ocamldoc -d doc -html ojs.mli ojs_exn.mli

# ocamlfind installation

INSTALL = \
  META \
  ppx/bin/gen_js_api$(EXE) \
  ppx/lib/gen_js_api_ppx.cma \
  gen_js_api.cma \
  ojs.cmi ojs_exn.cmi \
  ojs_runtime.js

install:
	ocamlfind install gen_js_api $(INSTALL)

uninstall:
	ocamlfind remove gen_js_api
