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
open Image

(* [size fn] returns a couple [(w,h)] corresponding to the size of the image
   contained in the file [fn]. The exception [Corrupted_image msg] is raised
   in case of problem. *)
val size : string -> int * int

(* [openfile fn] reads the image in the file [fn]. This function guesses the
   image format using the extension, and raises [Corrupted_image msg] in
   case of problem. *)
val openfile : string -> Image.image

(* [writefile fn img] writes the image [img] to the file [fn]. This function
   guesses the desired format using the extension. *)
val writefile : string -> Image.image -> unit


module PPM :
  sig
    module ReadPPM : ReadImage

    type ppm_mode = Binary | ASCII

    val write_ppm : string -> image -> ppm_mode -> unit
  end

module PNG :
  sig
    module ReadPNG : ReadImage

    val write_png : string -> image -> unit
  end

module JPG :
  sig
    module ReadJPG : ReadImage
  end

module GIF :
  sig
    module ReadGIF : ReadImage
  end

module XCF :
  sig
    module ReadXCF : ReadImage
  end
