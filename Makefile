.POSIX:
SHELL = /bin/sh
.SUFFIXES:

PKG = rrule
COLLECT = rrule

RACO = raco

setup:
	$(RACO) setup $(RACO_SETUP_ARGS) --pkgs $(PKG)

install:
	$(RACO) pkg install --name $(PKG) --auto $(RACO_INSTALL_ARGS)

uninstall:
	$(RACO) pkg remove $(PKG)

test:
	$(RACO) test $(RACO_TEST_ARGS) --package $(PKG)

check-deps:
	$(RACO) setup $(RACO_SETUP_ARGS) --check-pkg-deps --pkgs $(PKG)

fix-deps:
	$(RACO) setup $(RACO_SETUP_ARGS) --fix-pkg-deps --pkgs $(PKG)

fix-doc-index:
	$(RACO) setup $(RACO_SETUP_ARGS) --doc-index --pkgs $(PKG)

clean:
	$(RACO) setup $(RACO_SETUP_ARGS) --fast-clean --pkgs $(PKG)

docs/$(PKG)/index.html:
	scribble +m --redirect-main http://pkg-build.racket-lang.org/doc/ --htmls --dest ./docs ./scribblings/$(PKG).scrbl
