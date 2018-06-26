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
  let extensions = ["jpg"; "jpeg"; "jpe"; "jif"; "jfif"; "jfi"]

  let string_of_marker c =
    let mask_char c mask =
      string_of_int @@ int_of_char c land mask
    in
    match c with
    | '\x01' -> "TEM"
    | '\xC0' .. '\xC3' | '\xC5' .. '\xC7' | '\xC9' .. '\xCB' | '\xCD' .. '\xCF' as c -> "SOF" ^ mask_char c 0x0F
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

  let rec find_next_marker ich handle_function =
    match get_bytes ich 1 with
    | "\x00" (* ignore JPEG escaped 0xFF byte *)
    | "\xFF" -> find_next_marker ich handle_function
    | c -> handle_function ich c.[0]

  (* Based on https://stackoverflow.com/a/48488655 *)
  let size ich =
    let rec handle_marker ich c =
      match string_of_marker c with
      | "TEM"
      | "RST0" | "RST1" | "RST2" | "RST3" | "RST4" | "RST5" | "RST6" | "RST7"
      | "SOI" -> find_next_marker ich handle_marker
      | "EOI" -> raise Not_found
      | _ -> (
          let len = get_bytes ich 2 |> int_of_str2_be in
          let data = get_bytes ich (len - 2) in
          match string_of_marker c with
          | "SOF0" | "SOF1"  | "SOF2"  | "SOF3"  | "SOF5"  | "SOF6"  | "SOF7"
          | "SOF9" | "SOF10" | "SOF11" | "SOF13" | "SOF14" | "SOF15" ->
            let _precision = String.sub data 0 1 |> int_of_str1 in
            let height = String.sub data 1 2 |> int_of_str2_be in
            let width = String.sub data 3 2 |> int_of_str2_be in
            (width, height)
          | _ -> find_next_marker ich handle_marker
        )
    in
    try
      find_next_marker ich handle_marker
    with End_of_file ->
      raise (Corrupted_image "Reached end of file while looking for SOF marker")

  let parsefile fn =
    let read_chunks = ref [] in

    let image = create_rgb ~max_val:0 1 1 in

    let rec handle_marker ich c =
      let curr_ctype = string_of_marker c in
      try (
        match curr_ctype with
        (* Required markers *)
        | "SOI" ->
          only_once ich read_chunks curr_ctype;
          only_before ich read_chunks curr_ctype "SOF";
          only_before ich read_chunks curr_ctype "SOS";
          is_first_chunk ich read_chunks curr_ctype;
        | "EOI" ->
          only_once ich read_chunks curr_ctype;
          only_after ich read_chunks curr_ctype "SOI";
          only_after ich read_chunks curr_ctype "SOF";
          only_after ich read_chunks curr_ctype "SOS";
          is_not_first_chunk ich read_chunks curr_ctype;

          raise Exit
        | "SOF" ->
          only_once ich read_chunks curr_ctype;
          only_after ich read_chunks curr_ctype "SOI";
          only_before ich read_chunks curr_ctype "SOS";

          Printf.printf "JPEG Start of Frame: TODO\n%!";
        | "SOS" ->
          only_after ich read_chunks curr_ctype "SOI";
          only_after ich read_chunks curr_ctype "SOF";

          Printf.printf "JPEG Start of Scan: TODO\n%!";
        | _ as marker ->
          let s = Printf.sprintf "Marker %s is TODO" marker in
          raise (Not_yet_implemented s)
      );
        read_chunks := curr_ctype :: !read_chunks;
        find_next_marker ich handle_marker
      with Exit -> image
    in
    find_next_marker fn handle_marker
end
