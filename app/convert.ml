type ('a,'b) koutfmt = ('a, Format.formatter, unit, unit, unit, 'b) format6

let panic : ('a,'b) koutfmt -> 'a = fun fmt ->
  let k _ = Format.eprintf "%s\n%!" (Format.flush_str_formatter ()); exit 1 in
  Format.kfprintf k Format.str_formatter fmt

let handle_exception f x =
  try f x with
  | Image.Not_yet_implemented(msg) -> panic "Not implemented: %s." msg
  | _                              -> panic "Unhandled exception."

let _ =
  let (input_path, output_path) =
    match Sys.argv with
    | [|_;i;o|] -> (i, o)
    | _         -> panic "Usage: %s <input_file> <output_file>" Sys.argv.(0)
  in
  let img =
    let ic = ImageUtil_unix.chunk_reader_of_path input_path in
    let extension = ImageUtil_unix.get_extension' input_path in
    handle_exception (fun ic -> ImageLib.openfile ~extension ic) ic
  in
  handle_exception (fun img -> ImageLib_unix.writefile output_path img) img
