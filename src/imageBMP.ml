(*
 * This file is part of Imagelib.
 *
 * Imagelib is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Imabelib is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Imagelib.  If not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright (C) 2018 Rymdhund
 *)
open ImageUtil
open Image

(* The maximum allowed image width/height *)
let max_dimension = 1 lsl 15

type errors = [ `Bmp_error of string | chunk_reader_error ]

module BmpUtils = struct
  let bind_result res fn =
    match res with
    | Ok v -> fn v
    | Error e -> Error e

  let (>>=) = bind_result

  (* Result based bytes getter instead of raising exceptions *)
  let get_bytes_res (ich:chunk_reader) num_bytes: (string, [>chunk_reader_error]) result =
    match (ich (`Bytes num_bytes)) with
    | Ok x -> Ok x
    | Error e -> Error (e : chunk_reader_error :> [>chunk_reader_error])

  let get_int2_le (ich:chunk_reader): (int, [>chunk_reader_error]) result =
    get_bytes_res ich 2  >>= fun bs ->
    Ok (int_of_str2_le bs)

  let get_int4_le (ich:chunk_reader) : (int, [>chunk_reader_error]) result=
    get_bytes_res ich 4  >>= fun bs ->
    Ok (int_of_str4_le bs)

  (* Read signed 4 byte int from a channel and return it as an int *)
  let get_int4_signed_le (ich:chunk_reader): (int, [>chunk_reader_error]) result =
    get_bytes_res ich 4  >>= fun bs ->
    Ok (int32_of_str4_le bs |> Int32.to_int)

  let get_int32 (ich:chunk_reader): (Int32.t, [>chunk_reader_error]) result =
    get_bytes_res ich 4  >>= fun bs ->
    Ok (int32_of_str4_le bs)
end

open BmpUtils

module FileHeader = struct
  type t = {
    typ: string;
    image_size: int;
    pixel_offset: int;
  }

  let size = 14

  (* Read the first 14 bytes of ich and produce a file header *)
  let read (ich:ImageUtil.chunk_reader): (t, [> errors]) result =
    get_bytes_res ich 2 >>= fun typ ->
    if typ <> "BM" then
      Error (`Bmp_error  "BMP signature expected...")
    else
    get_int4_le ich >>= fun image_size ->
    get_bytes_res ich 4 >>= fun _reserved ->
    get_int4_le ich >>= fun pixel_offset ->
    Ok {
      typ;
      image_size;
      pixel_offset;
    }
end

(* A bitfield is used to extract a set of bits from an int32 and create an
 * 8 bit color channel value
 *)
module Bitfield = struct
  type t = {
    shift: int;
    len: int;
    mask: Int32.t
  }

  let of_mask (m: Int32.t): (t, [> errors]) result =
    let (land) = Int32.logand in
    let (lsr) = Int32.shift_right_logical in
    let rec get_shift ?(acc=0) m =
      if m = 0l || m land 1l = 1l
      then acc
      else get_shift ~acc:(acc+1) (m lsr 1)
    in

    let rec get_len ?(acc=0) m =
      if m land 1l = 0l
      then acc
      else get_len ~acc:(acc+1) (m lsr 1)
    in

    let shift = get_shift m in
    let shifted = m lsr shift in
    let len = get_len shifted in

    (* Make sure bitfield is continous and at most 8 bits long *)
    if len > 8 || m lsr (shift + len) <> 0l
    then
      Error (`Bmp_error "Invalid bitfield")
    else
      Ok {
        shift;
        len;
        mask=m
      }

  let of_mask_exn mask =
    match of_mask mask with
    | Ok bf -> bf
    | Error _ -> raise (Invalid_argument "Bitmask")

  let convert_3_to_8_bit = [|
    0; 36; 73; 109; 146; 182; 219; 255
  |]

  let convert_4_to_8_bit = [|
    0; 17; 34; 51; 68; 85; 102; 119; 136; 153; 170; 187; 204; 221; 238; 255
  |]

  let convert_5_to_8_bit = [|
    0; 8; 16; 25; 33; 41; 49; 58; 66; 74; 82; 90; 99; 107; 115; 123;
    132; 140; 148; 156; 165; 173; 181; 189; 197; 206; 214; 222; 230;
    239; 247; 255
  |]

  let convert_6_to_8_bit = [|
    0; 4; 8; 12; 16; 20; 24; 28; 32; 36; 40; 45; 49; 53; 57; 61; 65;
    69; 73; 77; 81; 85; 89; 93; 97; 101; 105; 109; 113; 117; 121; 125;
    130; 134; 138; 142; 146; 150; 154; 158; 162; 166; 170; 174; 178;
    182; 186; 190; 194; 198; 202; 206; 210; 215; 219; 223; 227; 231;
    235; 239; 243; 247; 251; 255;
  |]

  let color_of_pixel t pixel =
    let v = Int32.(shift_right_logical (logand pixel t.mask) t.shift |> to_int) in
    match t.len with
    | 0 -> 0
    | 1 -> v * 0xff
    | 2 -> v * 0x55
    | 3 -> convert_3_to_8_bit.(v)
    | 4 -> convert_4_to_8_bit.(v)
    | 5 -> convert_5_to_8_bit.(v)
    | 6 -> convert_6_to_8_bit.(v)
    | 7 -> (v lsl 1) lor (v lsr 6)
    | 8 -> v
    | _ -> assert false (* Constructor makes sure this doesn't happen *)

  let empty = {
    len=0;
    shift=0;
    mask=0l;
  }
end

(* This is a collection of the meta data we extract from a bmp image *)
module BitmapMetaData = struct
  type compression_method =
    | RGB
    | Bitfields

  type bits_per_pixel =
    | BPP_1
    | BPP_4
    | BPP_8
    | BPP_16
    | BPP_24
    | BPP_32

  module HeaderVersion = struct
    type t =
      | Info
      | V2
      | V3
      | V4
      | V5

    let of_size s =
      match s with
      | 40 -> Some Info
      | 52 -> Some V2
      | 56 -> Some V3
      | 108 -> Some V4
      | 124 -> Some V5
      | _ -> None
  end

  type bitmap_info_header = {
    header_version: HeaderVersion.t;
    size: int;
    width: int;
    height: int;
    color_planes: int;
    bits_per_pixel: bits_per_pixel;
    compression_method: compression_method;
    image_data_size: int;
    horizontal_ppm: int;
    vertical_ppm: int;
    palette_colors: int;
    important_colors: int;
  }

  type bitfields = Bitfield.t * Bitfield.t * Bitfield.t * Bitfield.t

  type t = {
    file_header: FileHeader.t;
    info_header: bitmap_info_header;
    bitfields: bitfields option;
    palette: string option;
  }

  let int_of_bpp = function
    | BPP_1 -> 1
    | BPP_4 -> 4
    | BPP_8 -> 8
    | BPP_16 -> 16
    | BPP_24 -> 24
    | BPP_32 -> 32

  let read_info_header (ich:chunk_reader): (bitmap_info_header, [> errors]) result =
    let bpp_of_int = function
      | 1 -> Ok BPP_1
      | 4 -> Ok BPP_4
      | 8 -> Ok BPP_8
      | 16 -> Ok BPP_16
      | 24 -> Ok BPP_24
      | 32 -> Ok BPP_32
      | n ->  Error (`Bmp_error (Printf.sprintf "Invalid bits per pixel: %d" n))
    in

    let compression_method_of_int n =
      match n with
      | 0 -> Ok RGB
      | 3 -> Ok Bitfields
      | _ -> Error (`Bmp_error "Invalid compression method")
    in

    get_int4_le ich >>= fun size ->
    match HeaderVersion.of_size size with
    | None -> Error (`Bmp_error "Invalid BitmapInfoHeader size")
    | Some header_version ->

    get_int4_signed_le ich >>= fun width ->
    if width < 0 || width > max_dimension then
      Error (`Bmp_error (Printf.sprintf "Invalid width: %d" width))
    else

    get_int4_signed_le ich >>= fun height ->
    (* TODO: handle negative heights *)
    if height < 0 || width > max_dimension then
      Error (`Bmp_error (Printf.sprintf "Invalid height: %d" height))
    else

    get_int2_le ich >>= fun color_planes ->
    if color_planes != 1 then
      Error (`Bmp_error "Invalid number of color planes")
    else

    get_int2_le ich >>= fun bpp_n ->
    bpp_of_int bpp_n >>= fun bits_per_pixel ->

    get_int4_le ich
    >>= compression_method_of_int
    >>= fun compression_method ->

    get_int4_le ich >>= fun image_data_size ->
    get_int4_le ich >>= fun horizontal_ppm ->
    get_int4_le ich >>= fun vertical_ppm ->

    get_int4_le ich >>= fun palette_colors ->
    if palette_colors > (1 lsl bpp_n) then
      Error (`Bmp_error "Too many palette colors")
    else

    get_int4_le ich >>= fun important_colors ->
    Ok {
      header_version;
      size;
      width;
      height;
      color_planes;
      bits_per_pixel;
      compression_method;
      image_data_size;
      horizontal_ppm;
      vertical_ppm;
      palette_colors;
      important_colors;
    }

  let read_bitfield_palette
  (file_header: FileHeader.t)
  (info_header: bitmap_info_header)
  (ich:chunk_reader)
  : (bitfields option * string option, [> errors]) result =
    let read_bitfields ~alpha (ich:chunk_reader) =
      get_int32 ich >>= Bitfield.of_mask >>= fun r ->
      get_int32 ich >>= Bitfield.of_mask >>= fun g ->
      get_int32 ich >>= Bitfield.of_mask >>= fun b ->
      (
        if alpha then
          get_int32 ich >>= Bitfield.of_mask
        else
          Ok Bitfield.empty
      ) >>= fun a ->
      Ok (r, g, b, a)
    in

    match info_header.compression_method, info_header.bits_per_pixel with
    | Bitfields, _ ->
      (* Fetch bitfields *)
      let bitfield_size = match info_header.header_version with
      | V3 | V4 | V5 -> 16
      | _ -> 12
      in
      read_bitfields ~alpha:(bitfield_size = 16) ich >>= fun bfs ->

      (* Ignore the rest of the data until the start of the pixel data *)
      let offset_to_pixels = max 0 (file_header.pixel_offset - FileHeader.size - 40 - bitfield_size) in
      get_bytes_res ich offset_to_pixels >>= fun _ ->

      Ok (Some bfs, None)
    | RGB, BPP_1
    | RGB, BPP_4
    | RGB, BPP_8 ->
      (* Fetch palette *)
      let palette_offset = info_header.size - 40 in
      get_bytes_res ich palette_offset >>= fun _ ->

      (* The palette_colors will never be > 256 here *)
      (* default number of palette colors is  *)
      let palette_colors =
        if info_header.palette_colors <> 0
        then info_header.palette_colors
        else 1 lsl (int_of_bpp info_header.bits_per_pixel)
      in

      let max_bytes_per_pixel = 4 in
      let max_palette_size = palette_colors * max_bytes_per_pixel in
      let offset_to_pixels = file_header.pixel_offset - info_header.size - FileHeader.size in
      let palette_size = min offset_to_pixels max_palette_size in
      get_bytes_res ich palette_size >>= fun palette ->

      (* Ignore the rest of the bytes until we get to the pixels *)
      get_bytes_res ich (offset_to_pixels - palette_size) >>= fun _ ->

      Ok (None, Some palette)
    | _ ->
      (* Ignore the rest of the bytes until we get to the pixels *)
      let offset_to_pixels = file_header.pixel_offset - FileHeader.size - 40 in
      get_bytes_res ich offset_to_pixels >>= fun _ ->
      Ok (None, None)

  (* Read the data until the file header offset and produce the bmp meta data *)
  let read (ich:ImageUtil.chunk_reader): (t, [> errors]) result =
    FileHeader.read ich >>= fun file_header ->
    read_info_header ich >>= fun info_header ->
    read_bitfield_palette file_header info_header ich >>= fun (bitfields, palette) ->
    Ok {
      file_header;
      info_header;
      bitfields;
      palette
    }
end

module ReadBMP : ReadImage = struct
  let extensions = ["bmp"; "dib"]

  let size (ich:chunk_reader) =
    let meta_res = BitmapMetaData.read ich in
    ImageUtil.close_chunk_reader ich;
    match meta_res with
    | Ok meta -> meta.info_header.width, meta.info_header.height
    | Error (`End_of_file _) -> raise (Corrupted_image "Unexpected end of file")
    | Error (`Bmp_error msg) -> raise (Corrupted_image msg)

  let get_color_1 palette bytes offset =
    let byte_offset = offset / 8 in
    let bit_offset = 7 - offset + byte_offset * 8 in
    let idx = ((int_of_char bytes.[byte_offset]) lsr bit_offset) land 0x1 in
    assert (idx < 2 && idx >= 0);
    (
      (* ignore the alpha channel *)
      int_of_char palette.[4*idx+2],
      int_of_char palette.[4*idx+1],
      int_of_char palette.[4*idx+0]
    )

  let get_color_4 palette bytes offset =
    let byte_offset = offset / 2 in
    let shift = (offset mod 2) * 4 in
    let idx = ((int_of_char bytes.[byte_offset]) lsr shift) land 0x0f in
    (
      (* ignore the alpha channel *)
      int_of_char palette.[4*idx+2],
      int_of_char palette.[4*idx+1],
      int_of_char palette.[4*idx+0]
    )

  let get_color_8 palette bytes offset =
    let idx = int_of_char bytes.[offset] in
    (
      (* ignore the alpha channel *)
      int_of_char palette.[4*idx+2],
      int_of_char palette.[4*idx+1],
      int_of_char palette.[4*idx+0]
    )

  let get_color_16_bitmasks bms bytes offset =
    let bm_r, bm_g, bm_b, bm_a = bms in
    let pixel = String.sub bytes (offset*2) 2 |> int_of_str2_le |> Int32.of_int in
    let red = Bitfield.color_of_pixel bm_r pixel in
    let green = Bitfield.color_of_pixel bm_g pixel in
    let blue = Bitfield.color_of_pixel bm_b pixel in
    let alpha = Bitfield.color_of_pixel bm_a pixel in
    (
      red,
      green,
      blue,
      alpha
    )

  let get_color_24 bytes offset =
    (
      int_of_char bytes.[3*offset+2],
      int_of_char bytes.[3*offset+1],
      int_of_char bytes.[3*offset]
    )

  let get_color_32 bytes offset =
    (
      int_of_char bytes.[4*offset+2],
      int_of_char bytes.[4*offset+1],
      int_of_char bytes.[4*offset],
      int_of_char bytes.[4*offset+3]
    )

  let get_color_32_bitmasks bm bytes offset =
    let bm_r, bm_g, bm_b, bm_a = bm in
    let pixel = String.sub bytes (offset*4) 4 |> int32_of_str4_le in
    let red = Bitfield.color_of_pixel bm_r pixel in
    let green = Bitfield.color_of_pixel bm_g pixel in
    let blue = Bitfield.color_of_pixel bm_b pixel in
    let alpha = Bitfield.color_of_pixel bm_a pixel in
    (
      red,
      green,
      blue,
      alpha
    )

  let parse_pixels
  ~(meta: BitmapMetaData.t)
  ~bpp
  ~color_getter
  (ich:chunk_reader)
  : (image, [> errors]) result =
    let ih = meta.info_header in
    let row_size = (ih.width * bpp + 31) / 32 * 4 in
    let image = create_rgb ih.width ih.height in

    try
      for y = 0 to ih.height - 1 do
        (* fetch row including padding *)
        let row = get_bytes ich row_size in
        for x = 0 to ih.width - 1 do
          let (r, g, b) = color_getter row x in
          write_rgb image x (ih.height - y - 1) r g b;
        done;
      done;
      Ok image
    with End_of_file ->
      Error (`End_of_file 0)

  let parse_pixels_alpha
  ~(meta: BitmapMetaData.t)
  ~bpp
  ~color_getter
  (ich:chunk_reader)
  : (image, [> errors]) result =
    let ih = meta.info_header in
    let row_size = (ih.width * bpp + 31) / 32 * 4 in
    let image = create_rgb ~alpha:true ih.width ih.height in

    try
      for y = 0 to ih.height - 1 do
        (* fetch row including padding *)
        let row = get_bytes ich row_size in
        for x = 0 to ih.width - 1 do
          let (r, g, b, a) = color_getter row x in
          write_rgba image x (ih.height - y - 1) r g b a;
        done;
      done;
      Ok image
    with End_of_file ->
      Error (`End_of_file 0)

  let standard_16_bit_bitfields =
    Bitfield.(
      of_mask_exn 0x7c00l,
      of_mask_exn 0x03e0l,
      of_mask_exn 0x001fl,
      of_mask_exn 0x0000l
    )

  let parsefile (ich:chunk_reader): image =

    let make_image (meta:BitmapMetaData.t) =
      match
        meta.info_header.bits_per_pixel,
        meta.info_header.compression_method,
        meta.bitfields,
        meta.palette
      with
        | BPP_1,  RGB, _, Some pl ->
            parse_pixels ~meta ~bpp:1 ~color_getter:(get_color_1 pl) ich
        | BPP_4,  RGB, _, Some pl ->
            parse_pixels ~meta ~bpp:4 ~color_getter:(get_color_4 pl) ich
        | BPP_8,  RGB, _, Some pl ->
            parse_pixels ~meta ~bpp:8 ~color_getter:(get_color_8 pl) ich
        | BPP_16, RGB, _, _ ->
            let color_getter = get_color_16_bitmasks standard_16_bit_bitfields in
            parse_pixels_alpha ~meta ~bpp:16 ~color_getter ich
        | BPP_16, Bitfields, Some bfs, _ ->
            let color_getter = get_color_16_bitmasks bfs in
            parse_pixels_alpha ~meta ~bpp:16 ~color_getter ich
        | BPP_24, RGB, _, _ ->
            parse_pixels ~meta ~bpp:24 ~color_getter:get_color_24 ich
        | BPP_32, RGB, _, _ ->
            parse_pixels_alpha ~meta ~bpp:32 ~color_getter:get_color_32 ich
        | BPP_32, Bitfields, Some bfs, _->
            let color_getter = get_color_32_bitmasks bfs in
            parse_pixels_alpha ~meta ~bpp:32 ~color_getter ich
        | _ ->
            Error (`Bmp_error "Invalid bits per pixel/compression method combination")
    in
    let image_res = BitmapMetaData.read ich >>= make_image in
    close_chunk_reader ich;
    match image_res with
    | Error (`End_of_file _) -> raise (Corrupted_image "Unexpected end of file")
    | Error (`Bmp_error msg) -> raise (Corrupted_image msg)
    | Ok img -> img
end
include ReadBMP
