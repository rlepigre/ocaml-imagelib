OCAMLBUILD := ocamlbuild
FLAGS := -cflags -w,-3 -use-ocamlfind
IMPLFILES := $(wildcard *.ml)
INTFFILES := $(wildcard *.mli)
VERSION := 20171028

all: imagelib.cma imagelib.cmxa META

# Try to find ocamlfind and ocamlbuild.
OCAMLF := $(shell which ocamlfind  2> /dev/null)
OCAMLB := $(shell which ocamlbuild 2> /dev/null)

# Try to find the camldecompress library.
CAMLDECOMPRESS := $(shell ocamlfind query -format %p decompress 2> /dev/null)

# Try to find the bigarray library.
BIGARRAY := $(shell ocamlfind query -format %p bigarray 2> /dev/null)

.PHONY: depchecks
depchecks:
ifndef OCAMLB
	$(error "The ocamlbuild program is required...")
endif
ifndef OCAMLF
	$(error "The ocamlfind program is required...")
endif
ifeq ($(CAMLDECOMPRESS),)
	$(error "The decompress library is required...")
endif
ifeq ($(BIGARRAY),)
	$(error "The bigarray library is required...")
endif

_tags: depchecks GNUmakefile
	@echo "true : package($(BIGARRAY)), package($(CAMLDECOMPRESS))" > $@
	@echo "true : safe_string" >> $@

META: depchecks
	@echo "name=\"imagelib\"" > $@
	@echo "version=\"0.1\"" >> $@
	@echo "description=\"A library for reading / writing images\"" >> $@
	@echo "requires=\"$(CAMLDECOMPRESS),$(BIGARRAY)\"" >> $@
	@echo "archive(byte)=\"imagelib.cma\"" >> $@
	@echo "archive(native)=\"imagelib.cmxa\"" >> $@

imagelib.cma:  $(IMPLFILES) $(INTFFILES) GNUmakefile imagelib.mllib _tags
	$(OCAMLBUILD) $(FLAGS) $@

imagelib.cmxa: $(IMPLFILES) $(INTFFILES) GNUmakefile imagelib.mllib _tags
	$(OCAMLBUILD) $(FLAGS) $@

IMPL := $(addprefix _build/,$(IMPLFILES))
INTF := $(addprefix _build/,$(INTFFILES))
CMX  := $(IMPL:.ml=.cmx)
CMO  := $(IMPL:.ml=.cmo)
CMI  := $(IMPL:.ml=.cmi)
OBJ  := $(IMPL:.ml=.o)
LIB  := _build/imagelib.cma _build/imagelib.cmxa _build/imagelib.a META

install: all uninstall
	@ocamlfind install imagelib $(CMX) $(CMO) $(CMI) $(OBJ) $(INTF) $(LIB)

uninstall:
	@ocamlfind remove imagelib

clean:
	$(OCAMLBUILD) -clean

distclean: clean
	rm -f *~ _tags META

.PHONY: release
release: distclean
	git push origin
	git tag -a ocaml-imagelib_$(VERSION)
	git push origin ocaml-imagelib_$(VERSION)
