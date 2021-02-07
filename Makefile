# The package gen_js_api is released under the terms of an MIT-like license.
# See the attached LICENSE file.
# Copyright 2015 by LexiFi.

.PHONY: all examples test test-promote clean install uninstall doc reindent publish

all:
	dune build @install @DEFAULT

examples:
	dune build @examples/DEFAULT

doc:
	dune build @doc

test:
	dune build @runtest

test-promote:
	dune build @runtest --auto-promote

clean:
	dune clean

PREFIX := $$(opam config var prefix)

install:
	opam-installer --prefix $(PREFIX) gen_js_api.install

uninstall:
	opam-installer -u --prefix $(PREFIX) gen_js_api.install

reindent:
	git ls-files *.ml *.mli | grep -v expected | xargs ocp-indent -i

VERSION := $$(opam show . | grep "^version" | sort -u | sed 's/version *//')

publish: all
	echo "Publishing v$(VERSION) ..." 
	git tag -a v$(VERSION)
	git push origin v$(VERSION)
	opam publish



	
	
