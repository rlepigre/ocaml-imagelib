The imagelib library
====================

The imagelib library implements image formats such as PNG and PPM  in
OCaml, relying on only one external dependency: `decompress`.

Supported image formats:
 - PNG (full implementation of RFC 2083),
 - PPM, PGM, PBM, ... (fully supported),
 - BMP (mostly supported),
 - JPG (only image size natively, conversion to PNG otherwise),
 - GIF (only image size natively, conversion to PNG otherwise),
   - There is an experimental native implementation available in the pure `ImageLib` module.
 - XCF (only image size natively, conversion to PNG otherwise),
 - Other formats rely on convert (imagemagick).

As imagelib only requires `decompress`, it is suitable for compilation to
javascript using `js_of_ocaml` (only for operations not requiring the
convert binary).

Dependencies
------------
List of dependencies:
 - OCaml (at least 4.03.0)
 - dune (at least 1.2.0)
 - decompress (version 0.8.1)
 - GNU Make (build)

Additional packages:
 - ImageMagick (convert) for handling some formats.
 - Crowbar, Alcotest, afl-persistent (for the test suite)

Installation
------------

Imagelib is available on `opam` (run `opam install imagelib`). It can also
be installed from source as follows.

```bash
make
make install
```

Fuzzing
-------

This section is primarily of use for developers of the library.

To exectute `crowbar` tests, it is enough to run `make slowtest`.

The `aflrunner.exe` target can be used to fuzz test the image parsers with [AFL](http://lcamtuf.coredump.cx/afl).
The parser is selected using the filename extension.

Here is an example, requiring at least one valid BMP file in a folder called `sample-bmps/`:
```shell
imagelib $ dune build tests/aflrunner.exe
imagelib $ afl-fuzz \
  -i sample-bmps/ \
  -o bmp-results/ \
  -f /dev/shm/my.bmp -- ./_build/default/tests/aflrunner.exe x /dev/shm/my.bmp
```

Once one or more crashes have been identified, you can inspect them like this:
```shell
imagelib/bmpfuzz/crashes $ export OCAMLRUNPARAM=b
imagelib/bmpfuzz/crashes $ for x in id\:*
    do echo ":: $x"
    cp "$x" x.bmp
    ../../_build/default/tests/aflrunner.exe x x.bmp
    echo "// $x"
  done

:: id:000015,sig:06,src:000110,op:havoc,rep:8
Fatal error: exception Invalid_argument("Bytes.create")
Raised by primitive operation at file "stdlib.ml", line 432, characters 10-26
Called from file "unix/imageUtil_unix.ml", line 60, characters 17-52
Re-raised at file "unix/imageUtil_unix.ml", line 68, characters 8-15
Called from file "src/imageBMP.ml", line 37, characters 10-34
Called from file "src/imageBMP.ml", line 369, characters 6-40
Called from file "src/imageBMP.ml", line 376, characters 4-53
Called from file "src/imageBMP.ml", line 556, characters 20-43
Called from file "tests/aflrunner.ml", line 13, characters 21-62
Called from file "tests/aflrunner.ml", line 21, characters 9-34
// id:000015,sig:06,src:000110,op:havoc,rep:8
```
