include $(shell ocamlc -where)/Makefile.config

OCAMLFLAGS = -w +A-4-41-45 -warn-error +8
OCAMLC = ocamlc $(OCAMLFLAGS)

GENJSAPI=../../gen_js_api$(EXE)
OJSDIR=../..

SRCS=
SRCS+=jquery.mli
SRCS+=js_date.mli
SRCS+=js_str.mli
SRCS+=test_js.mli

TARGETS=$(SRCS:.mli=.cmi) $(SRCS:.mli=.cmo)

SUBDIRS=bindings

.PHONY: all clean

all: $(TARGETS)

%.ml: %.mli
	$(GENJSAPI) $<

%.cmi: %.mli
	$(OCAMLC) -c -I $(OJSDIR) $<

%.cmo: %.ml
	$(OCAMLC) -c -I $(OJSDIR) $<

clean:
	rm -f *.exe *.cmo *.cmi *~
