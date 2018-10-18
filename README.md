The imagelib library
====================

The imagelib library implements image formats such as PNG and PPM  in
OCaml, relying on only one external dependency: `decompress`.

Supported image formats:
 - PNG (full implementation of RFC 2083),
 - PPM, PGM, PBM, ... (fully supported),
 - JPG (only image size natively, conversion to PNG otherwise),
 - GIF (only image size natively, conversion to PNG otherwise),
 - XCF (only image size natively, conversion to PNG otherwise),
 - Other formats rely on convert (imagemagick).

As imagelib only requires `decompress`, it is suitable for compilation to
javascript using `js_of_ocaml` (only for operations not requiring the
convert binary).

Dependencies
------------
List of dependencies:
 - OCaml
 - decompress (version 0.7)
 - Findlib (build)
 - OCamlbuild (build)
 - GNU Make (build)

Additional packages:
 - ImageMagick (convert) for handling some formats.

Installation
------------

Imagelib is available on `opam` (run `opam install imagelib`). It can also
be installed from source as follows.

```bash
make
make install
```
