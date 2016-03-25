ROOT = ../..
include $(ROOT)/Makefile.common

.PHONY: all clean with_ocamlfind

all:
	$(OCAMLC) -c -I $(OJSDIR) -ppx "$(GENJSAPI) -ppx" calc.ml
	$(OCAMLC) -no-check-prims -o calc.exe $(OJSDIR)/gen_js_api.cma calc.cmo
	$(JSOO) $(OJSDIR)/ojs_runtime.js calc.exe


with_ocamlfind:
	ocamlfind ocamlc -package gen_js_api.ppx -c calc.ml
	ocamlfind ocamlc -package gen_js_api -o calc.exe -linkpkg -no-check-prims calc.cmo
	$(JSOO) +gen_js_api/ojs_runtime.js calc.exe

clean:
	rm -f *.cm* calc.exe calc.js *~
