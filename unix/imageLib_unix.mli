(** This module provides an easy-to-use interface for imagelib.
    In most cases, you'd want to use these functions rather than those in
    imagelib.
*)

(** [writefile fn img] writes the image [img] to the file [fn]. This function
    guesses the desired format using the extension.
    Raises {!Corrupted_image} if it encounters a problem.
    If the file extension is unknown to imagelib,
    this will first write out a png
    and then convert that to the desired format using the "convert"
    command from imagemagick.
*)
val writefile : string -> Image.image -> unit


(** [size fn] reads the image from the file [fn].
    It returns the pixel dimensions of the image
    as the tuple [width, heigth].
*)
val size : string -> int * int


(** [openfile fn] reads the image from the file [fn].
    This function guesses the file format using the extension.
    Raises {!Corrupted_image} if it encounters a problem.
    If the file extension is unknown to imagelib, this will attempt to convert
    to png using imagemagick and then read in the png file.
*)
val openfile : string -> Image.image
