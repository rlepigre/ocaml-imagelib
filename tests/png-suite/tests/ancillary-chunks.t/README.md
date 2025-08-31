Ancillary Chunks
================

To test the correct decoding of ancillary chunks, these test-files contain one
or more examples of these chunks. Depending on the type of chunk, a number of
typical values are selected to test. Unluckily, the test-set can not contain
all combinations, because that would be an endless set.

The significant bits are used in files with the next higher bit-depth. They
indicate how many bits are valid.

- `cs3` - 3 significant bits
- `cs5` - 5 significant bits
- `cs8` - 8 significant bits (reference)
- `cs3` - 13 significant bits

For the physical pixel dimensions, the result of each decoding should be a
square picture. The first (cdf) image is an example of flat (horizontal)
pixels, where the pHYS chunk (x is 1 per unit, y = 4 per unit) must take care
of the correction. The second is just the other way round. The last example
uses the unit specifier, for 1000 pixels per meter. This should result in a
picture of 3.2 cm square.

- `cdf` - physical pixel dimensions, 8x32 flat pixels
- `cdh` - physical pixel dimensions, 32x8 high pixels
- `cds` - physical pixel dimensions, 8x8 square pixels
- `cdu` - physical pixel dimensions, with unit-specifier

The chromaticity chunk defines the rgb and whitepoint coordinates according to
the 1931 CIE Committee XYZ color space.

- `ccw` - primary chromaticities and white point

PNG files can contain a chunk giving a histogram of the colors in the image.

- `ch1` - histogram 15 colors
- `ch2` - histogram 256 colors

The time chunk specifies when the picture last was modified (or created).

- `cm7` - modification time, 01-jan-1970
- `cm9` - modification time, 31-dec-1999
- `cm0` - modification time, 01-jan-2000

In the textual chunk, a number of the standard and some non-standard text
items are included. Text can optionally be compressed.

- `ct0` - no textual data
- `ct1` - with textual data
- `ctz` - with compressed textual data
- `cte` - UTF-8 international text - english
- `ctf` - UTF-8 international text - finnish
- `ctg` - UTF-8 international text - greek
- `cth` - UTF-8 international text - hindi
- `ctj` - UTF-8 international text - japanese

The exif chunk was added to PNG in 2017 to contain exif data typically added
by digital cameras to JPEG images.

- `exif` - image attributes
