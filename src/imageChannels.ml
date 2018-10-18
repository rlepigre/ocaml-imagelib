module type OUT_CHANNEL = sig
    type out_channel
    val output_string : out_channel -> string -> unit
    val output_char : out_channel -> char -> unit
end

module Buffer_channel = struct
    type out_channel = Buffer.t
    let output_string = Buffer.add_string
    let output_char = Buffer.add_char
  end
