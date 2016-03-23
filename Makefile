# The package gen_js_api is released under the terms of an MIT-like license.
# See the attached LICENSE file.
# Copyright 2015 by LexiFi.

VERSION=0.1
# Don't forget to change META file as well

include $(shell ocamlc -where)/Makefile.config

OCAMLFLAGS = -w +A-4-41-45 -warn-error +8
OCAMLC = ocamlc $(OCAMLFLAGS)

JSOO = js_of_ocaml --pretty

SUBDIRS=examples

.PHONY: subdirs $(SUBDIRS) all clean

all:
	$(OCAMLC) -c ojs.mli ojs.ml
	$(OCAMLC) -c ojs_exn.mli ojs_exn.ml
	$(OCAMLC) -a -o gen_js_api.cma ojs.cmo ojs_exn.cmo
	$(OCAMLC) -I +compiler-libs -o gen_js_api$(EXE) ocamlcommon.cma gen_js_api.mli gen_js_api.ml

subdirs: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@

clean:
	rm -f *~ gen_js_api$(EXE) *.cm* .*~
	for dir in $(SUBDIRS); do $(MAKE) clean -C $$dir; done

INSTALL=META gen_js_api$(EXE) gen_js_api.cma ojs.cmi ojs_runtime.js

install:
	ocamlfind install gen_js_api $(INSTALL)

uninstall:
	ocamlfind remove gen_js_api
