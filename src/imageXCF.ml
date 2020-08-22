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
open Stdlib
open ImageUtil
open Image

let xcf_signature = "gimp xcf "

type xcf_header_data = {
  version            : int ;
  image_size         : int * int ;
  colour_type        : int
}

module ReadXCF : ReadImage = struct
  let extensions = ["xcf"]

  let version_num s =
    match s with
    | "file\000" -> 0
    | "v001\000" -> 1
    | "v002\000" -> 2
    | _          -> raise (Corrupted_image "Unknown version number...")

  let read_header ich =
    let magic     = get_bytes ich 9 in
    let version   = get_bytes ich 5 in
    let width     = get_bytes ich 4 in
    let height    = get_bytes ich 4 in
    let base_type = get_bytes ich 4 in
    if magic <> xcf_signature then
      raise (Corrupted_image "Corrupted header...");
    let w  = int_of_str4 width in
    let h  = int_of_str4 height in
    let ct = int_of_str4 base_type in
    if ct < 0 || ct > 2 then
      raise (Corrupted_image "Unknown color type...");
    {
      version     = version_num version ;
      image_size  = w , h ;
      colour_type = ct
    }

  (* Read the size of a XCF image.
   * Arguments:
   *   - fn : filename.
   * Returns a couble (width, height).
   * Note: the image is not checked for inconsistency, only the signature and
   * header are checked.
   *)
  let size ich =
    let hdr = read_header ich in
    close_chunk_reader ich;
    hdr.image_size

  let parsefile _ =
    raise (Not_yet_implemented "ImageXCF.openfile") (* TODO  *)
end
include ReadXCF
