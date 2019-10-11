module type OUT_CHANNEL = sig
    type out_channel
    val output_string : out_channel -> string -> unit
    val output_char : out_channel -> char -> unit
end

module Buffer_channel
  : OUT_CHANNEL with type out_channel = Buffer.t
= struct
    type out_channel = Buffer.t
    let output_string = Buffer.add_string
    let output_char = Buffer.add_char
  end

module Chunk_channel
  : OUT_CHANNEL with type out_channel = ImageUtil.chunk_writer
= struct
  type out_channel = ImageUtil.chunk_writer
  let output_string = ImageUtil.chunk_write
  let output_char = ImageUtil.chunk_write_char
end
