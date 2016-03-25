# The package gen_js_api is released under the terms of an MIT-like license.
# See the attached LICENSE file.
# Copyright 2015, 2016 by LexiFi.

ROOT = ../..
include $(ROOT)/Makefile.common

.PHONY: all clean

all:
	$(GENJSAPI) test_bindings.mli
	$(OCAMLC) -c -I $(OJSDIR) test_bindings.mli test_bindings.ml
	$(OCAMLC) -c -I $(OJSDIR) -ppx "$(GENJSAPI) -ppx" main.ml
	$(OCAMLC) -no-check-prims -o main.exe $(OJSDIR)/gen_js_api.cma test_bindings.cmo main.cmo
	$(JSOO) $(OJSDIR)/ojs_runtime.js main.exe

clean:
	rm -f test_bindings.ml *.cm* main.exe main.js *~
