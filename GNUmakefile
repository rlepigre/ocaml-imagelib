VERSION := 20180522

.PHONY: all
all: bin

.PHONY: bin
bin:
	@dune build

install: bin
	@dune install

uninstall:
	@dune uninstall

.PHONY: clean
clean:
	@dune clean

distclean: clean
	@find . -name "*~" -exec rm {} \;

#.PHONY: release
#release: distclean
#	git push origin
#	git tag -a ocaml-imagelib_$(VERSION)
#	git push origin ocaml-imagelib_$(VERSION)
