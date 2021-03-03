The imagelib library
====================

The imagelib library implements image formats such as PNG, BMP, and PPM in
OCaml, relying on only one external dependency: 'decompress'.

Unix-dependent functionality such as reading or writing to files in the
filesystem are packaged in the `imagelib.unix` findlib module inside this
OPAM package; to use it you need to include `imagelib.unix` specifically
in your project's dependencies, for instance `(libraries imagelib.unix)`
in your Dune file.

Supported image formats:
 - PNG (full implementation of RFC 2083),
 - PPM, PGM, PBM, ... (fully supported),
 - BMP (read-only)
 - JPG (only image size natively),
 - GIF (only image size natively),
    - There is an experimental native implementation available in the pure `ImageLib` module.
 - XCF (only image size natively),
 - Utility functions for handling unimplemented formats are available in
   the 'imagelib.unix' findlib package and handle conversion from unsupported
   image formats like JPG, GIF, XCF by converting them to PNG using the
   `convert` commandline utility from `imagemagick`.

As imagelib only requires `decompress`, it is suitable (excluding operations
requiring the `imagemagick` `convert` binary) for compilation to javascript
using `js_of_ocaml`, or inclusion in MirageOS unikernels.

Dependencies
------------
List of dependencies:
 - OCaml (at least 4.03.0)
 - dune (at least 1.2.0)
 - decompress (version 0.8.1)
 - GNU Make (build)

Additional packages:
 - ImageMagick (`convert`) for handling some formats.
 - Crowbar, Alcotest, afl-persistent (for the test suite)

Installation
------------

Imagelib is available on `opam` (run `opam install imagelib`). It can also
be installed from source as follows.

```bash
make
make install
```

Imagetool
---------

`app/imagetool.ml` contains an example binary that acts as a command-line interface to many of the functions in the library. It will be installed as `imagetool` or `imagetool.exe` if you use **opam** to install the library, and otherwise it will be in `_build/default/app/imagetool.exe`

```
usage: /home/user/ocaml/imagelib/_build/default/app/imagetool.exe [args] INPUT-FILE [OUTPUT-FILE]
Displays a picture in the terminal, or convert it (if OUTPUT-FILE is specified)
The OUTPUT-FILE is a template; any '#' will be replaced with
the frame number (useful if the image has multiple frames)
Options:
--irc | --vt100   Output to terminal using escape codes
 \--character     Fill character(s) for text-mode output
--resize          Set output dimensions. Syntax: '80x25' | '10%'
  \--gamma        Adjust/reinterpret gamma
--crop-x WIDTH    Crop final output to WIDTH pixels
--crop-y HEIGHT   Crop final output to HEIGHT pixels
--frames RANGE    Only operate on RANGE frames from a multi-frame file format
                  (like an animated GIF). The format is comma-separated with '-' separating each range
                    example: --frames 1-10,20-30,50-
                    example: --frames -200,300-400
--background      Set background color for transparency using RGB,
                  e.g. '0xFF0000' is red. Defaults to black.
```

*Example: Displaying an animated GIF in the terminal with a pink background color:*
```shell
imagetool --background 0xff00ff turtle.gif
```

*Example: Resizing specific frames from a GIF file and exporting them to individual PNG files:*
```shell
$ imagetool --resize '50%' --frames -2,3-4,6- turtle.gif small-\#.png
$ ls
small-1.png small-2.png small-3.png small-4.png small-6.png small-7.png
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
