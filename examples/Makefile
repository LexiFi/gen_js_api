# The package gen_js_api is released under the terms of an MIT-like license.
# See the attached LICENSE file.
# Copyright 2015, 2016 by LexiFi.

SUBDIRS=calc test misc

all:
	for dir in $(SUBDIRS); do $(MAKE) all -C $$dir; done

clean:
	for dir in $(SUBDIRS); do $(MAKE) clean -C $$dir; done
