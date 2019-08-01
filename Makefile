# The package gen_js_api is released under the terms of an MIT-like license.
# See the attached LICENSE file.
# Copyright 2015 by LexiFi.

.PHONY: all examples clean install uninstall doc

all:
	dune build @install @DEFAULT

examples:
	dune build @examples/DEFAULT

doc:
	dune build @doc

clean:
	dune clean

PREFIX := $$(opam config var prefix)

install:
	opam-installer --prefix $(PREFIX) gen_js_api.install

uninstall:
	opam-installer -u --prefix $(PREFIX) gen_js_api.install
