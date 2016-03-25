ROOT = ../..
include $(ROOT)/Makefile.common

all:
	$(ROOT)/gen_js_api test_bindings.mli
	$(OCAMLC) -c -I $(OJSDIR) test_bindings.mli test_bindings.ml
	$(OCAMLC) -c -I $(OJSDIR) -ppx "$(GENJSAPI) -ppx" main.ml
	$(OCAMLC) -no-check-prims -o main.exe $(OJSDIR)/gen_js_api.cma test_bindings.cmo main.cmo
	$(JSOO) $(OJSDIR)/ojs_runtime.js main.exe

clean:
	rm -f test_bindings.ml *.cm* main.exe main.js *~
