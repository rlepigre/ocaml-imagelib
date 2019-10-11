(* [writefile fn img] writes the image [img] to the file [fn]. This function
   guesses the desired format using the extension.
   Raises {!Corrupted_image} if it encounters a problem.
   If the file extension is unknown to imagelib, this will first write out a PNG
   and then convert that to the desired format using the "convert"
   command from imagemagick.
*)
val writefile : string -> Image.image -> unit

val size : string -> int * int

val openfile : string -> Image.image
