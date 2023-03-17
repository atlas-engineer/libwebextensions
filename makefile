# SPDX-FileCopyrightText: Atlas Engineer LLC
# SPDX-License-Identifier: BSD-3-Clause

# REVIEW: Is there a better way to find the .pc file regardless of the version suffix?
# TODO: Find libsbcl/libecl too. eclconfig for ECL and ??? for SBCL.
PKG_MODULES := glib-2.0 gobject-2.0 $(shell pkg-config --list-all | awk '/^webkit2gtk-web-extension/ {print $$1}')

# See https://stackoverflow.com/questions/5618615/check-if-a-program-exists-from-a-makefile.
CC := $(shell command -v $(CC) 2> /dev/null || echo gcc)

LISP ?= sbcl
SBCL_FLAGS =
ifeq ($(LISP), sbcl)
	SBCL_FLAGS = --dynamic-space-size $(shell sbcl --noinform --no-userinit --non-interactive --eval '(prin1 (max 3072 (/ (sb-ext:dynamic-space-size) 1024 1024)))' --quit | tail -1)
endif
## We use --non-interactive with SBCL so that errors don't interrupt the CI.
LISP_FLAGS ?= $(SBCL_FLAGS) --no-userinit --non-interactive

CFLAGS += $(shell pkg-config $(PKG_MODULES) --cflags) -fPIC
LDLIBS += $(shell pkg-config $(PKG_MODULES) --libs)

.PHONY: all install clean
all: libwebextensions.so libwebextensions.core

libwebextensions.so: libwebextensions.c
	cp -f /gnu/store/zv59bxvia1vbg8v4c0xzcx98i22rw9mf-sbcl-2.2.11/lib/libsbcl.so ./
	$(CC) -fPIC -shared -L . -lsbcl -o $@ $^ $(LDLIBS) $(LDFLAGS) $(CFLAGS)

libwebextensions.core:
	$(LISP) $(LISP_FLAGS) --eval '(require "asdf")' --load libwebextensions.asd --eval '(asdf:load-system :libwebextensions)' --load build.lisp --eval '(dump-core)' --eval '(quit)'

clean:
	$(RM) libwebextensions.so libwebextensions.core
