Gamma Values
============

To test if your viewer handles gamma-correction, (3x) 6 test-files are
available. They contain corrected color-ramps and a corresponding gamma-chunk
with the file-gamma value. These are created in such a way that when the
viewer does the gamma correction right, all 6 should be displayed identical.

If they are different, probably the gamma correction is omitted. In that case,
have a look at the two right columns in the 6 pictures. The image where those
two look the same (when looked from far) reflects the gamma of your system.
However, because of the limited size of the image, you should do more
elaborate tests to determine your display gamma.

For comparisons, three pages with GIF images are available. Depending on the
display gamma of your system, select the NeXT-, the Mac- or the PC-version.

- `g03` - file-gamma = 0.35, for display with gamma = 2.8
- `g04` - file-gamma = 0.45, for display with gamma = 2.2 (PC)
- `g05` - file-gamma = 0.55, for display with gamma = 1.8 (Mac)
- `g07` - file-gamma = 0.70, for display with gamma = 1.4
- `g10` - file-gamma = 1.00, for display with gamma = 1.0 (NeXT)
- `g25` - file-gamma = 2.50, for display with gamma = 0.4
