include $(shell ocamlc -where)/Makefile.config

OCAMLFLAGS = -w +A-4-41-45 -warn-error +8
OCAMLC = ocamlc $(OCAMLFLAGS)

GENJSAPI=../gen_js_api$(EXE)
OJSDIR=..
OJSRUNTIME=../ojs_runtime.js

JSOO = js_of_ocaml --pretty

TARGETS=
TARGETS+=main.js
TARGETS+=test_jquery.js
TARGETS+=test_js_str.js
TARGETS+=test_js_date.js

SUBDIRS=bindings

.PHONY: subdirs $(SUBDIRS) all clean

all: $(TARGETS)

subdirs: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@

main.exe: main.cmo subdirs
	$(OCAMLC) -no-check-prims -o $@ -I $(OJSDIR) gen_js_api.cma bindings/test_js.cmo $<

test_jquery.exe: test_jquery.cmo subdirs
	$(OCAMLC) -no-check-prims -o $@ -I $(OJSDIR) gen_js_api.cma bindings/jquery.cmo $<

test_js_str.exe: test_js_str.cmo subdirs
	$(OCAMLC) -no-check-prims -o $@ -I $(OJSDIR) gen_js_api.cma bindings/js_str.cmo $<

test_js_date.exe: test_js_date.cmo subdirs
	$(OCAMLC) -no-check-prims -o $@ -I $(OJSDIR) gen_js_api.cma bindings/js_date.cmo $<

%.cmo: %.ml subdirs
	$(OCAMLC) -c -I $(OJSDIR) -I bindings -ppx "$(GENJSAPI) -ppx" $<

%.js: %.exe
	$(JSOO) -o $@ "$(OJSRUNTIME)" $<

clean:
	rm -f *.exe *.cmi *.cmo *.js *~
	for dir in $(SUBDIRS); do $(MAKE) clean -C $$dir; done
