(*
 * This file is part of Imagelib.
 *
 * Imagelib is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Imagelib is distributed in the hope that it will be useful,
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
open ImageUtil

(** Note: This module is an interface to the pure OCaml implementations of
   various image formats.

   For an easy to use file-based interface, see the [imagelib.unix] findlib
   package distributed with [imagelib].
   You find said interface in [src/imageLib_unix.mli] or in the
   documentation for the module {!ImageLib_unix}.

   [ImageLib_unix] will fall back to using [imagemagick's convert] utility
   to accomodate the manipulation of file formats that are not well supported
   by this library.

   The GIF implementation is still experimental;
   the [ImageLib_unix] module reverts to [imagemagick] to avoid causing
   problems stemming from the premature state of the GIF stack.
 *)


(* [size fn] returns a couple [(w,h)] corresponding to the size of the image
   contained via the chunk_reader [ich].
   The exception [{!Corrupted_image} msg] is raised
   in case of problem.
   NB: This will try to run the command "convert" from imagemagick
   to convert to PNG if the file extension is unknown.
*)
val size : extension:string -> ImageUtil.chunk_reader -> int * int

(* [openfile fn] reads the image in the file [fn]. This function guesses the
   image format using the extension, and raises [{!Corrupted_image} msg] in
   case of problem.
*)
val openfile : extension:string -> ImageUtil.chunk_reader -> Image.image

val openfile_streaming : extension:string -> ImageUtil.chunk_reader ->
  [`GIF of ImageGIF.read_state] option ->
  image option * int * [`GIF of ImageGIF.read_state] option
(** see {!ReadImageStreaming.read_streaming} *)

(* [writefile extension och img] writes the image [img] to the chunk reader [och]. 
   The desired format is specified via [extension].
*)
val writefile : extension:string ->
  ImageUtil.chunk_writer -> Image.image -> unit

module PPM :
  sig
    include WriteImage
    include ReadImage

    module ReadPPM : ReadImage

    type ppm_mode = Binary | ASCII

    val write_ppm : chunk_writer -> image -> ppm_mode -> unit
  end

module PNG :
  sig
    include WriteImage
    include ReadImage

    module ReadPNG : ReadImage

    [@@ocaml.deprecated]
    val write_png : chunk_writer -> image -> unit

    [@@ocaml.deprecated]
    val bytes_of_png : image -> Bytes.t
  end

module JPG :
  sig
    module ReadJPG : ReadImage
  end

module GIF :
  sig
    include ReadImage
    include WriteImage

    val write : chunk_writer -> image -> unit
    (** [write cw image] encodes [image] as a GIF and writes it to [cw].
        At the moment compression is not supported, so the GIF will
        NOT be uncompressed.*)
  end

module XCF :
  sig
    module ReadXCF : ReadImage
  end

module BMP :
  sig
    module ReadBMP : ReadImage
  end
