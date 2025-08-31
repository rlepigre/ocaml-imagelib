PNG Test Suite
==============

This PNG test suite is taken from http://www.schaik.com/pngsuite/, and was
originally created by Willem van Schaik.

Each test folder contains a `README.md` file describing the features that are
exercised by the contained images. The text is taken directly from the above
website, for quicker reference, and as is the following.

PNG Capabilities
----------------

Supported color-types are:
- grayscale
- grayscale + alpha-channel
- color palettes
- rgb
- rgb + alpha-channel

Allowed bit-depths are depending on the color-type, but are in the range of
1-bit (grayscale, which is b&w) upto 16-bits.

Special features are:
- interlacing (Adam-7)
- gamma-support
- transparency (a poor-man's alpha solution)

File Naming
-----------

Where possible, the test-files are 32x32 bits icons. This results in a still
reasonable size of the suite even with a large number of tests. The name of
each test-file reflects the type in the following way:
```
    filename:                               g04i2c08.png
                                            || ||||
    test feature (in this case gamma) ------+| ||||
    parameter of test (here gamma-value) ----+ ||||
    interlaced or non-interlaced --------------+|||
    color-type (numerical) ---------------------+||
    color-type (descriptive) --------------------+|
    bit-depth ------------------------------------+
```

color-type:
- `0g` - grayscale
- `2c` - rgb color
- `3p` - paletted
- `4a` - grayscale + alpha channel
- `6a` - rgb color + alpha channel

bit-depth:
- `01` - with color-type 0, 3
- `02` - with color-type 0, 3
- `04` - with color-type 0, 3
- `08` - with color-type 0, 2, 3, 4, 6
- `16` - with color-type 0, 2, 4, 6

interlacing:
- `n` - non-interlaced
- `i` - interlaced
