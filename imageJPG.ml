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
 * along with Imabelib.  If not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright (C) 2014 Rodolphe Lepigre.
 *)
open Pervasives
open ImageSequence
open ImageUtil
open Image

module ReadJPG : ReadImage = struct
  let debug = ref true

  type component = {
    identifier : int;
    h_sample_factor : int;
    v_sample_factor : int;
    quant_index : int;
  }
  
  type sof = {
    length : int;
    precision : int;
    height : int;
    width : int;
    components : component list;
  }

  let extensions = ["jpg"; "jpeg"; "jpe"; "jif"; "jfif"; "jfi"]

  let string_of_marker c =
    let mask_char c mask =
      string_of_int @@ int_of_char c land mask
    in
    match c with
    | '\x01' -> "TEM"
    | '\xC0' .. '\xC3' | '\xC5' .. '\xC7' | '\xC9' .. '\xCB' | '\xCD' .. '\xCF' as c -> "SOF" ^ mask_char c 0x0F
    | '\xC4' -> "DHT"
    | '\xCC' -> "DAC"
    | '\xD0' .. '\xD7' as c -> "RST" ^ mask_char c 0x07
    | '\xD8' -> "SOI"
    | '\xD9' -> "EOI"
    | '\xDA' -> "SOS"
    | '\xDB' -> "DQT"
    | '\xDC' -> "DNL"
    | '\xDD' -> "DRI"
    | '\xDE' -> "DHP"
    | '\xDF' -> "EXP"
    | '\xE0' .. '\xEF' as c -> "APP" ^ mask_char c 0x0F
    | '\xF0' .. '\xFD' as c -> "JPG" ^ mask_char c 0x0F
    | '\xFE' -> "COM"
    | _ as c -> Printf.printf "Unknown marker %x\n" (int_of_char c); assert false

  let marker_stands_alone = function
    | "TEM"
    | "RST0" | "RST1" | "RST2" | "RST3" | "RST4" | "RST5" | "RST6" | "RST7"
    | "SOI"
    | "EOI" -> true
    | _ -> false

  let rec find_next_marker ich handle_function =
    match get_bytes ich 1 with
    | "\x00" (* ignore JPEG escaped 0xFF byte *)
    | "\xFF" -> find_next_marker ich handle_function
    | c -> handle_function ich c.[0]
  
  let parse_sof ich =
    let rec parse_component ich acc = function
      | 0 -> []
      | count ->
          let identifier = get_bytes ich 1 |> int_of_str1 in
          let dims = get_bytes ich 1 |> int_of_str1 in
          let h_sample_factor = dims land 0x0F in
          let v_sample_factor = (dims land 0xF0) lsr 4 in
          let quant_index = get_bytes ich 1 |> int_of_str1 in
          if !debug then
            Printf.printf "Component: identifier %d, h factor %d, v factor %d, quantization table index %d\n%!" identifier h_sample_factor v_sample_factor quant_index;
          let component = {
            identifier;
            h_sample_factor;
            v_sample_factor;
            quant_index;
          } in
          parse_component ich (component :: acc) (count - 1)
    in
    let length = get_bytes ich 2 |> int_of_str2_be in
    let precision = get_bytes ich 1 |> int_of_str1 in
    let height = get_bytes ich 2 |> int_of_str2_be in
    let width = get_bytes ich 2 |> int_of_str2_be in
    let compcount = get_bytes ich 1 |> int_of_str1 in
    if !debug then
      Printf.printf "SOF: length %d, precision %d, height %d, width %d, component count %d\n%!" length precision height width compcount;
    let components = parse_component ich [] compcount in
    { length; precision; height; width; components }

  (* Based on https://stackoverflow.com/a/48488655 *)
  let size ich =
    let rec handle_marker ich c =
      match string_of_marker c with
      | "TEM"
      | "RST0" | "RST1" | "RST2" | "RST3" | "RST4" | "RST5" | "RST6" | "RST7"
      | "SOI" -> find_next_marker ich handle_marker
      | "EOI" -> raise Not_found
      | "SOF0" | "SOF1"  | "SOF2"  | "SOF3"  | "SOF5"  | "SOF6"  | "SOF7"
      | "SOF9" | "SOF10" | "SOF11" | "SOF13" | "SOF14" | "SOF15" ->
          let sof = parse_sof ich in
          (sof.width, sof.height)
      | _ ->
          let len = get_bytes ich 2 |> int_of_str2_be in
          let _data = get_bytes ich (len - 2) in
          find_next_marker ich handle_marker
    in
    try
      find_next_marker ich handle_marker
    with End_of_file ->
      raise (Corrupted_image "Reached end of file while looking for SOF marker")

  let parsefile fn =
    let read_chunks = ref [] in
    let image = ref None in

    let rec handle_marker ich c =
      let curr_ctype = string_of_marker c in
      if !debug then
        Printf.printf "Found marker %s\n%!" curr_ctype;
      try (
        match curr_ctype with
        (* Required markers *)
        | "SOI" ->
          only_once ich read_chunks curr_ctype;
          only_before ich read_chunks "SOS" curr_ctype;
          is_first_chunk ich read_chunks curr_ctype;
        | "EOI" ->
          only_once ich read_chunks curr_ctype;
          only_after ich read_chunks "SOI" curr_ctype;
          only_after ich read_chunks "SOS" curr_ctype;
          is_not_first_chunk ich read_chunks curr_ctype;

          raise Exit
        | "SOF0" | "SOF1"  | "SOF2"  | "SOF3"  | "SOF5"  | "SOF6"  | "SOF7"
        | "SOF9" | "SOF10" | "SOF11" | "SOF13" | "SOF14" | "SOF15" ->
          only_once ich read_chunks curr_ctype;
          only_after ich read_chunks "SOI" curr_ctype;
          only_before ich read_chunks "SOS" curr_ctype;

          let sof = parse_sof ich in
          image := Some (create_rgb ~max_val:(ones sof.precision) sof.width sof.height)
        | "SOS" ->
          only_after ich read_chunks "SOI" curr_ctype;

          Printf.printf "JPEG Start of Scan: TODO\n%!";
        | _ as marker ->
          Printf.printf "Marker %s is TODO\n%!" marker;

          if not (marker_stands_alone marker) then
            let length = get_bytes ich 2 |> int_of_str2_be in
            let data = get_bytes ich (length - 2) in
            ignore data
      );
        read_chunks := curr_ctype :: !read_chunks;
        find_next_marker ich handle_marker
      with Exit ->
        match !image with
        | Some image -> image
        | None -> raise (Corrupted_image "End-of-file reached before image data")
    in
    find_next_marker fn handle_marker
end
