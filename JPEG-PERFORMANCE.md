# JPEG performance

How the pure-OCaml JPEG codec compares to ImageMagick (and to libjpeg, which is
what ImageMagick uses), where the time goes, and what could be done about it.

All figures below are medians of 9 runs on the machine described in
[Method](#method). Timings are wall-clock milliseconds.

## Summary

* Correctness is not in question: across 395 JPEGs produced by libjpeg the
  decoder agrees with libjpeg pixel for pixel up to the rounding of the
  floating-point DCT (worst mean difference 2.18/255, worst single-pixel
  difference 12 on a 1x17 image).
* Speed is. On a megapixel image the decoder is **7.5 to 8.9x slower than
  ImageMagick** and **18 to 29x slower than libjpeg**; the encoder is **6.7 to
  10.7x** and **29 to 59x** slower respectively.
* On small images the picture reverses: the codec is fast enough that
  ImageMagick's own process start-up dominates, and `imagelib-convert` is
  actually the faster of the two end to end.
* Most of the remaining gap is **not** the entropy decoder. It is the inverse
  DCT (56% of decode) and the per-pixel pixel-buffer access pattern (31%),
  both of which are straightforward to improve. See
  [Where the time goes](#where-the-time-goes).
* Separately, the command line tools are dominated by the PPM and PNG writers,
  which cost more than JPEG decoding does. That is pre-existing and unrelated
  to JPEG, but it means `imagelib-convert in.jpg out.ppm` is not a good way to
  measure this codec.

## Method

Everything is timed in-process where possible, so that neither side pays the
other's start-up cost, and with the *installed* `imagelib-convert` rather than
`dune exec` for the end-to-end numbers (going through `dune exec` costs about
35 ms per invocation, which would have swamped the results).

* **libjpeg** is measured through Pillow in the same Python process. Both
  ImageMagick and Pillow link libjpeg-turbo (3.2.0 and 3.1.4 respectively), so
  this is the same codec ImageMagick uses, measured without ImageMagick's own
  overheads. That isolates the codec from the tool.
* **ImageMagick** is measured as `magick in.jpg ppm:-`, i.e. the real tool,
  including its start-up.
* **imagelib** is measured in-process through `ImageLib.JPG.parsefile` and
  `ImageLib.JPG.bytes_of_jpg`, and separately end to end through the installed
  `imagelib-convert`.

Machine and toolchain:

| | |
|---|---|
| CPU | Intel N100, 4 cores |
| OS | Linux x86-64 |
| OCaml | 5.6.0+dev0 (native, dune default flags) |
| ImageMagick | 7.1.2-32 Q16-HDRI, built-in JPEG delegate libjpeg-turbo 3.2.0 |
| libjpeg | libjpeg-turbo 3.1.4, via Pillow 12.3.0 |

Test images, chosen to cover the easy and the hard end:

| name | size | content |
|---|---|---|
| `flat` | 1280x960 | one flat colour; almost no entropy |
| `photo` | 1280x960 | smooth gradients; compresses well |
| `noise` | 1280x960 | uniform random; worst case for JPEG, nearly every AC coefficient nonzero |
| `medium` | 640x480 | flat colour |
| `small` | 64x64 | tiny, so start-up dominates |

Encoding is at quality 90 with 4:4:4 (no chroma subsampling) for both sides,
which is the default of both `bytes_of_jpg` and Pillow.

## Results

### Decoding

| image | imagelib | libjpeg | ImageMagick | vs libjpeg | vs ImageMagick |
|---|---:|---:|---:|---:|---:|
| `flat` 1280x960 | 267.1 | 11.2 | 35.3 | 23.9x | 7.6x |
| `photo` 1280x960 | 276.0 | 9.4 | 36.8 | 29.4x | 7.5x |
| `noise` 1280x960 | 518.7 | 29.2 | 58.3 | 17.8x | 8.9x |
| `medium` 640x480 | 66.7 | 1.7 | 15.3 | 39.3x | 4.4x |
| `small` 64x64 | 1.1 | 0.1 | 4.0 | 7.6x | **0.27x** |

### Encoding

| image | imagelib | libjpeg | ImageMagick | vs libjpeg | vs ImageMagick |
|---|---:|---:|---:|---:|---:|
| `flat` 1280x960 | 407.9 | 6.9 | 38.2 | 58.9x | 10.7x |
| `photo` 1280x960 | 409.0 | 7.7 | 39.8 | 53.3x | 10.3x |
| `noise` 1280x960 | 538.7 | 18.8 | 80.1 | 28.7x | 6.7x |
| `medium` 640x480 | 100.5 | 1.9 | 11.3 | 53.4x | 8.9x |
| `small` 64x64 | 1.4 | 0.1 | 4.2 | 12.8x | **0.33x** |

Throughput on the megapixel `photo` image: we decode at 4.5 MP/s and encode at
3.0 MP/s, against 131 MP/s and 160 MP/s for libjpeg.

### End to end, as a user sees it

`imagelib-convert in.jpg out.ppm` against `magick in.jpg ppm:-`, using the
installed binary:

| image | imagelib | ImageMagick | ratio |
|---|---:|---:|---:|
| `flat` 1280x960 | 521.6 | 25.7 | 20.3x |
| `photo` 1280x960 | 569.8 | 27.6 | 20.7x |
| `noise` 1280x960 | 1154.3 | 48.8 | 23.7x |
| `medium` 640x480 | 131.5 | 10.8 | 12.2x |
| `small` 64x64 | 5.2 | 3.9 | 1.3x |
| *process start-up only* | *2.3* | *4.6* | *0.5x* |

We start up in half the time ImageMagick does, so the crossover where the
codec stops mattering is small: by 64x64 we are within 1.3x, and the gap only
becomes dramatic on images big enough for the decode to dominate.

## Where the time goes

Attribution for a 1280x960 4:4:4 decode (276 ms total), measured directly on
each stage. A 4:4:4 frame has three components, so the per-block costs are
counted three times over.

| stage | ms | share |
|---|---:|---:|
| inverse DCT, 3 x 19200 blocks | ~154 | 56% |
| output assembly, one `write_rgb` per pixel | ~85 | 31% |
| storing samples into component planes | ~14 | 5% |
| allocating the component planes | ~11 | 4% |
| dequantisation | ~5 | 2% |
| entropy decoding and marker parsing | small | <2% |

Two conclusions:

* The **inverse DCT is over half the time**, at 2.7 us per 8x8 block. Ours is
  a straightforward separable transform over a precomputed cosine basis,
  which is clear and accurate but does 1024 floating-point multiply-adds per
  block through bounds-checked array accesses. libjpeg uses a partially
  factorised integer IDCT with about an order of magnitude less arithmetic.
* The **per-pixel pixel-buffer access** is the other third. `Image.write_rgb`
  is a closure call that pattern-matches on the pixmap and then does a
  bounds-checked `Bigarray.Array2.set`, once per pixel per channel. Hoisting
  the pixmap match out of the loop and touching the bigarrays directly would
  remove most of this, and it affects every codec in the library, not just
  JPEG.

The entropy decoder, which is the part that is actually JPEG specific, is
almost free by comparison: decoding `noise` instead of `flat` adds 252 ms, and
that is the extra Huffman work over roughly 1.2M nonzero coefficients.

For reference, the other codecs are in the same ballpark as JPEG and are
dominated by the same per-pixel costs. On a 1280x960 image:

| operation | time |
|---|---:|
| filling an image through `Image.write_rgb`, 1.2M pixels | 87 ms |
| `ImagePPM.write_ppm` to a buffer, 3.7 MB | 314 ms |
| `ImageLib.PNG.write` to a buffer | 281 ms |

`write_ppm` emits one `chunk_write_char` per byte, which is why it is the
slowest of the three; that is a pre-existing issue in the PPM writer and has
nothing to do with JPEG.

### Encoding

Encoding is in the same shape: ~50x slower than libjpeg, dominated by the
forward DCT and by the same per-pixel `Image.read_rgb` pattern used to sample
the source image. The `noise` case is relatively better (28.7x) because it has
more arithmetic per coefficient, which dilutes the fixed per-pixel costs.

## What was already fixed

`upsample` was 38% of decode time, and at 4:4:4 it was allocating and copying
two full-size arrays per component to reproduce a plane exactly. It now returns
the plane unchanged when the replication factor is 1:

```
flat  1280x960  decode   438.9 ms -> 267.1 ms   (-39%)
photo 1280x960  decode   441.6 ms -> 276.0 ms   (-38%)
noise 1280x960  decode   679.8 ms -> 518.7 ms   (-24%)
small 64x64      decode     1.5 ms ->   1.1 ms   (-28%)
```

Verified against the same 395 libjpeg-encoded fixtures with identical results
(worst mean difference 2.18/255 before and after).

## What could be done next

Roughly in order of value:

1. **Integer inverse and forward DCT** (56% of decode, and similarly large in
   encode). Replacing the float basis with the standard partially factorised
   8-point integer transform, as libjpeg does, should cut decode time by
   something like 2.5x. This is a self-contained change to two functions and
   the existing 395-fixture comparison is a strong safety net.
2. **Avoid the per-pixel closure in the hot loops** (31% of decode, and the
   same cost on the encode side). Matching on `img.pixels` once per image
   rather than once per pixel, and then indexing the bigarrays directly in the
   assembly loops, is straightforward and would help every codec.
3. **Flambda.** OCaml 5 with flambda would vectorise both the DCT loops and the
   plane copies. This is the largest single lever available, but the library
   currently supports OCaml 4.14, so it cannot be relied on.
4. **Avoid the intermediate planes entirely** for the common 4:4:4 case by
   writing IDCT output straight into the image pixmaps. Worth ~9% on its own,
   and more once 1 and 2 are done because it removes a whole pass over memory.

## Caveats

* Figures are from one machine and one build of OCaml; ratios will move on
  other hardware. The shape of the attribution should not.
* ImageMagick is measured as a subprocess, so its numbers include start-up. That
  flatters us on small images and does not affect the megapixel rows, where
  start-up is a few percent.
* Only baseline sequential decoding was benchmarked. Progressive decoding does
  more Huffman work and should be relatively less affected by the fixed costs,
  but it was not measured here.
* The benchmark harness used to produce these numbers is not committed: it
  depends on a specific Pillow environment and, for the attribution, on
  reaching into `ImageJPG`'s internals. The methodology above is enough to
  reproduce the headline numbers.
