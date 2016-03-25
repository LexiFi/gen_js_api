# The package gen_js_api is released under the terms of an MIT-like license.
# See the attached LICENSE file.
# Copyright 2015, 2016 by LexiFi.

ROOT=../..
include $(ROOT)/Makefile.common

BINDINGS=jquery.mli js_date.mli js_str.mli

.PHONY: all clean

all: $(BINDINGS:.mli=.cmi) $(BINDINGS:.mli=.cmo) test_jquery.js

test_jquery.exe: jquery.cmo test_jquery.cmo
	$(OCAMLC) -no-check-prims -o $@ $(OJSDIR)/gen_js_api.cma $^

test_jquery.js: test_jquery.exe
	$(JSOO) $(OJSDIR)/ojs_runtime.js $<

%.ml: %.mli
	$(GENJSAPI) $<

%.cmi: %.mli
	$(OCAMLC) -c -I $(OJSDIR) $<

%.cmo: %.ml
	$(OCAMLC) -ppx "$(GENJSAPI) -ppx" -c -I $(OJSDIR) $<

test_jquery.cmo: jquery.cmi

clean:
	rm -f $(BINDINGS:.mli=.ml) *.cm* test_jquery.exe test_jquery.js
