OCAMLFLAGS = -w +A-4-9-41-45
OCAMLC = ocamlfind ocamlc $(OCAMLFLAGS)
OCAMLRUN = ocamlrun

GEN_JS_API_PATH:=${HOME}/gen_js_api
GEN_JS_API=$(GEN_JS_API_PATH)/gen_js_api.exe

INCL=-I $(GEN_JS_API_PATH)
OJS_RUNTIME=$(GEN_JS_API_PATH)/ojs_runtime.js

API=
API+=stdlib.mli

TARGET=$(API:.mli=.cmo)

all: $(TARGET)

%.ml: %.mli
	$(GEN_JS_API) $< > $@

%.cmo: %.mli %.ml
	$(OCAMLC) -c $(INCL) $^

clean:
	rm -f *~ *.exe *.cm* .*~
	rm -f $(TARGET)
