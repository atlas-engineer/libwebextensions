# SPDX-FileCopyrightText: Atlas Engineer LLC
# SPDX-License-Identifier: BSD-3-Clause

# REVIEW: Is there a better way to find the .pc file regardless of the version suffix?
PKG_MODULES := glib-2.0 gobject-2.0 webkit2gtk-web-extension-4.1

# See https://stackoverflow.com/questions/5618615/check-if-a-program-exists-from-a-makefile.
CC := $(shell command -v $(CC) 2> /dev/null || echo gcc)

CFLAGS += $(shell pkg-config $(PKG_MODULES) --cflags) $(guile-config compile) -fPIC
LDLIBS += $(shell pkg-config $(PKG_MODULES) --libs) $(guile-config link)

MAKEFILE_DIR := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))

.PHONY: all install clean
all: webextensions.so

wrapper.c: wrapper.in
	m4 $^ >> $@

webextensions.so: wrapper.c
	$(CC) $^ -fPIC --shared -o $@ $(LDLIBS) $(CFLAGS)

install: webextensions.so
	$(INSTALL) webextensions.so

clean:
	$(RM) wrapper.c webextensions.so
