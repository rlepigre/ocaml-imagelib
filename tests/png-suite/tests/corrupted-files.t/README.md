Corrupted Files
===============

All these files are invalid PNG images. When decoding they should generate
appropriate error-messages.

- `xs1` - signature byte 1 MSBit reset to zero
- `xs2` - signature byte 2 is a 'Q'
- `xs4` - signature byte 4 lowercase
- `xs7` - 7th byte a space instead of control-Z
- `xcr` - added cr bytes
- `xlf` - added lf bytes
- `xhd` - incorrect IHDR checksum
- `xc1` - color type 1
- `xc9` - color type 9
- `xd0` - bit-depth 0
- `xd3` - bit-depth 3
- `xd9` - bit-depth 99
- `xdt` - missing IDAT chunk
- `xcs` - incorrect IDAT checksum
