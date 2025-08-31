Background Colors
=================

When the PNG file contains a background chunk, this should be used for
pictures with alpha-channel or pictures with a transparency chunk. For
pictures without this background-chunk, but with alpha, this test-set assumes
a black background.

For the images in this test, the left-side should be 100% the background
color, where moving to the right the color should gradually become the image
pattern.

- `bga` - alpha + no background
- `bgw` - alpha + white background
- `bgg` - alpha + gray background
- `bgb` - alpha + black background
- `bgy` - alpha + yellow background
