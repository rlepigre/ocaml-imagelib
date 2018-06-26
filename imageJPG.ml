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
    | '\xC0' .. '\xC7' | '\xC9' .. '\xCB' | '\xCD' .. '\xCF' as c -> "SOF" ^ mask_char c 0x0F
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

  (* Based on https://stackoverflow.com/a/48488655 *)
  let size ich =
    let rec handle_marker ich c =
      Printf.printf "Found marker ff%x\n" (int_of_char c);
      match c with
      | '\x01' (* TEM *)
      | '\xD0' .. '\xD7' (* RST0-7 *)
      | '\xD8' (* SOI *) -> find_marker ich
      | '\xD9' (* EOI *) -> raise Not_found
      | '\xC0' .. '\xFE' as marker (* SOF0-15, JPG, DHT, DAC *) -> (
          let len = get_bytes ich 2 |> int_of_str2_be in
          Printf.printf "  has length %d\n" len;
          let data = get_bytes ich (len - 2) in
          match marker with
          | '\xC0' .. '\xCF' ->
            let _precision = String.sub data 0 1 |> int_of_str1 in
            let height = String.sub data 1 2 |> int_of_str2_be in
            let width = String.sub data 3 2 |> int_of_str2_be in
            (width, height)
          | _ -> find_marker ich
      )
      | _ -> find_marker ich
    and find_marker ich =
      match get_bytes ich 1 with
      | "\x00" (* ignore JPEG escaped 0xFF byte *)
      | "\xFF" -> find_marker ich
      | c -> handle_marker ich c.[0]
    in
    try
      find_marker ich
    with End_of_file ->
      raise (Corrupted_image "Reached end of file while looking for SOF marker")

  let parsefile fn =
    raise (Not_yet_implemented "ImageJPG.openfile") (* TODO  *)
end
