VERSION := 20200829

.PHONY: all
all: bin

.PHONY: bin
bin:
	@dune build -p imagelib

install: bin
	@dune install

uninstall:
	@dune uninstall

.PHONY: clean
clean:
	@dune clean

distclean: clean
	@find . -name "*~" -exec rm {} \;

.PHONY: test slowtest
test:
	@dune runtest -p imagelib

slowtest:
	@dune build @slowtests -p imagelib --no-buffer
#.PHONY: release
#release: distclean
#	git push origin
#	git tag -a ocaml-imagelib_$(VERSION)
#	git push origin ocaml-imagelib_$(VERSION)
