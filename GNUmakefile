all: imagelib.cma imagelib.cmxa

FILES=image.ml imageUtil.ml readImg.ml \
			imagePNG.ml imagePPM.ml imageGIF.ml imageJPG.ml imageXCF.ml

imagelib.cma: $(FILES) GNUmakefile
	ocamlbuild -cflags -w,-3 -use-ocamlfind $@

imagelib.cmxa: $(FILES) GNUmakefile
	ocamlbuild -cflags -w,-3 -use-ocamlfind $@

clean:
	ocamlbuild -clean

distclean: clean
	rm -f *~
