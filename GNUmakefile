OCAMLBUILD := ocamlbuild
FLAGS := -cflags -w,-3 -use-ocamlfind
FILES := $(wildcard *.ml)

all: imagelib.cma imagelib.cmxa

# Try to find ocamlfind and ocamlbuild.
OCAMLF := $(shell which ocamlfind 2> /dev/null)
OCAMLB := $(shell which ocamlbuild 2> /dev/null)

# Try to find the camlzip library.
CAMLZIP := $(shell ocamlfind query -format %p zip 2> /dev/null)
ifeq ($(CAMLZIP),)
	CAMLZIP := $(shell ocamlfind query -format %p camlzip 2> /dev/null)
endif

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
ifeq ($(CAMLZIP),)
	$(error "The zip / camlzip library is required...")
endif
ifeq ($(BIGARRAY),)
	$(error "The bigarray library is required...")
endif

_tags: depchecks GNUmakefile
	@echo "true : package($(BIGARRAY)), package($(CAMLZIP))" > $@

imagelib.cma:  $(FILES) GNUmakefile imagelib.mllib _tags
	$(OCAMLBUILD) $(FLAGS) $@

imagelib.cmxa: $(FILES) GNUmakefile imagelib.mllib _tags
	$(OCAMLBUILD) $(FLAGS) $@

clean:
	$(OCAMLBUILD) -clean

distclean: clean
	rm -f *~ _tags
