OCAMLBUILD := ocamlbuild
FLAGS := -cflags -w,-3 -use-ocamlfind
IMPLFILES := $(wildcard *.ml)
INTFFILES := $(wildcard *.ml)
VERSION := 20160413
URL=https://patoline.org/archive/imagelib

all: imagelib.cma imagelib.cmxa META

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

META: depchecks
	@echo "name=\"imagelib\"" > $@
	@echo "version=\"0.1\"" >> $@
	@echo "description=\"A library for reading / writing images\"" >> $@
	@echo "requires=\"$(CAMLZIP),$(BIGARRAY)\"" >> $@
	@echo "archive(byte)=\"imagelib.cma\"" >> $@
	@echo "archive(native)=\"imagelib.cmxa\"" >> $@

imagelib.cma:  $(IMPLFILES) $(INTFFILES) GNUmakefile imagelib.mllib _tags
	$(OCAMLBUILD) $(FLAGS) $@

imagelib.cmxa: $(IMPLFILES) $(INTFFILES) GNUmakefile imagelib.mllib _tags
	$(OCAMLBUILD) $(FLAGS) $@

clean:
	$(OCAMLBUILD) -clean

distclean: clean
	rm -f *~ _tags META

uninstall:
	@ocamlfind remove imagelib

#for imalib developper
OPAMREPO=$(HOME)/Caml/opam-repository/packages/imagelib
OPAMDIR := $(OPAMREPO)/imagelib.$(VERSION)
URLSSH=patoline@patoline.org:/var/www/patoline/archive/imagelib
TAR=imagelib_$(VERSION).tar.gz

tar:
	cd ../imagelib_0; darcs pull; make clean
	cd ..; tar cvfz $(TAR) --exclude=_darcs --transform "s,imagelib_0,imagelib-$(VERSION),"  imagelib_0

distrib: tar
	scp ../$(TAR) $(URLSSH)/
	ssh patoline@patoline.org "cd /var/www/patoline/archive/imagelib; ln -sf $(TAR) imagelib-latest.tar.gz"

opam: distrib
	sed -e s/__VERSION__/$(VERSION)/g opam.tmpl > opam
	mkdir -p $(OPAMDIR)
	cp opam $(OPAMDIR)/opam
	cp description.txt $(OPAMDIR)/descr
	echo -n "archive: \""  > $(OPAMDIR)/url
	echo -n "$(URL)/$(TAR)" >> $(OPAMDIR)/url
	echo "\"" >> $(OPAMDIR)/url
	echo -n "checksum: \"" >> $(OPAMDIR)/url
	echo -n `md5sum ../$(TAR) | cut -b -32` >> $(OPAMDIR)/url
	echo "\"" >> $(OPAMDIR)/url

IMPL := $(addprefix _build/,$(IMPLFILES))
INTF := $(addprefix _build/,$(INTFFILES))
CMX  := $(IMPL:.ml=.cmx)
CMO  := $(IMPL:.ml=.cmo)
CMI  := $(IMPL:.ml=.cmi)
OBJ  := $(IMPL:.ml=.o)
LIB  := _build/imagelib.cma _build/imagelib.cmxa _build/imagelib.a META

install: all uninstall
	@ocamlfind install imagelib $(CMX) $(CMO) $(CMI) $(OBJ) $(INTF) $(LIB)
