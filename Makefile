# The package gen_js_api is released under the terms of an MIT-like license.
# See the attached LICENSE file.
# Copyright 2015 by LexiFi.

.PHONY: all examples clean install uninstall

all:
	$(MAKE) -C src all

examples:
	$(MAKE) -C examples all

clean:
	$(MAKE) -C src clean
	$(MAKE) -C examples clean

install:
	$(MAKE) -C src install

uninstall:
	$(MAKE) -C src uninstall
