OCAMLFLAGS = -w +A-4-9-41-45
OCAMLC = ocamlfind ocamlc $(OCAMLFLAGS)
OCAMLRUN = ocamlrun

GEN_JS_API_PATH:=${HOME}/gen_js_api
GEN_JS_API_EXE=$(GEN_JS_API_PATH)/gen_js_api$(EXE)
GEN_JS_API_LIB=$(GEN_JS_API_PATH)/gen_js_api.cma
GEN_JS_API_RUNTIME=$(GEN_JS_API_PATH)/ojs_runtime.js

JSOO_PATH = ~/js_of_ocaml
JSOO = $(JSOO_PATH)/compiler/js_of_ocaml --pretty --noruntime $(JSOO_PATH)/runtime/runtime.js

NODEJS=node

INCL=-I $(GEN_JS_API_PATH)
OJS_RUNTIME=$(GEN_JS_API_PATH)/ojs_runtime.js

API=
API+=stdlib.mli

TESTS=main.ml

LIBS=$(API:.mli=.cmo)

TARGET=$(LIBS) $(TESTS:.ml=.js)

all: $(TARGET)

%.ml: %.mli
	$(GEN_JS_API_EXE) $< > $@

%.cmo: %.mli %.ml
	$(OCAMLC) -c $(INCL) $^

%.exe: %.ml $(LIBS)
	$(OCAMLC) -c -ppx "$(GEN_JS_API_EXE) -ppx" $<
	$(OCAMLC) -no-check-prims -o $@ $(GEN_JS_API_LIB) $(LIBS) $(<:.ml=.cmo)

%.js: %.exe
	$(JSOO) -o $@ $(GEN_JS_API_RUNTIME) $<

%.out: %.js
	$(NODEJS) $< > $@

clean:
	rm -f *~ *.exe *.cm* .*~
	rm -f $(TARGET)
