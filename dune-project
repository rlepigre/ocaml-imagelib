(lang dune 3.14)
(name imagelib)
(generate_opam_files true)

(authors
  "Rodolphe Lepigre <lepigre@mpi-sws.org>")
(maintainers
  "Rodolphe Lepigre <lepigre@mpi-sws.org>")
(source (github rlepigre/ocaml-imagelib))
(license "LGPL-3.0-or-later")

(package
 (name imagelib)
 (synopsis "Pure OCaml image library")
 (description "The imagelib library implements image formats such as PNG, BMP,
and PPM in OCaml, relying on only one external dependency: 'decompress'.

Unix-dependent functionality such as reading or writing to files in the
filesystem are packaged in the `imagelib.unix` findlib module inside this
OPAM package; to use it you need to include `imagelib.unix` specifically
in your project's dependencies, for instance `(libraries imagelib.unix)`
in your Dune file.

Supported image formats:
 - PNG (full implementation of RFC 2083),
 - PPM, PGM, PBM, ... (fully supported),
 - BMP (read-only)
 - JPEG (read and write),
    - Baseline sequential and progressive DCT decoding, 8-bit samples,
      arbitrary chroma subsampling and restart intervals.  Writing produces
      baseline sequential images.  Arithmetic coding, lossless modes, 12-bit
      samples and CMYK are not supported.
 - GIF (read-only, animated),
    - There is an experimental native implementation available in the pure `ImageLib` module, which now
      handles interlaced images, local colour tables, the disposal methods 0 to 3 and comment extensions.
      `imagelib.unix` still prefers `imagemagick` for GIF, to avoid changing the behaviour
      existing applications depend on.
 - XCF (only image size natively),
 - Utility functions for handling unimplemented formats are available in
   the 'imagelib.unix' findlib package and handle conversion from unsupported
   image formats like GIF, XCF by converting them to PNG using the
   `convert` commandline utility from `imagemagick`.

As imagelib only requires `decompress`, it is suitable (excluding operations
requiring the `imagemagick` `convert` binary) for compilation to javascript
using `js_of_ocaml`, or inclusion in MirageOS unikernels.

`app/imagetool.ml` contains an example binary that acts as a command-line
interface to many of the functions in the library. It will be installed as
`imagetool` or `imagetool.exe` if you use **opam** to install the library,
and otherwise it will be in `_build/default/app/imagetool.exe`")
 (depends
  (ocaml (>= 4.14.0))
  (decompress (>= 1.5.3))
  (alcotest :with-test)
  (crowbar :with-test)
  (afl-persistent :with-test)))
