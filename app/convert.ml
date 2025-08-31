type ('a,'b) koutfmt = ('a, Format.formatter, unit, unit, unit, 'b) format6

let panic : ('a,'b) koutfmt -> 'a = fun fmt ->
  let k _ = Format.eprintf "%s\n%!" (Format.flush_str_formatter ()); exit 1 in
  Format.kfprintf k Format.str_formatter fmt

let _ =
  let (input_path, output_path) =
    match Sys.argv with
    | [|_;i;o|] -> (i, o)
    | _         -> panic "Usage: %s <input_file> <output_file>" Sys.argv.(0)
  in
  try
    let ic = ImageUtil_unix.chunk_reader_of_path input_path in
    let extension = Filename.extension input_path in
    let img = ImageLib.openfile ~extension ic in
    ImageLib_unix.writefile output_path img
  with
  | Image.Not_yet_implemented(msg) -> panic "Not implemented: %s." msg
  | Image.Corrupted_image(msg)     -> panic "Corrupted image: %s." msg
  | Sys_error(msg)                 -> panic "%s" msg
  | e                              -> panic "Unhandled exception: %s."
                                        (Printexc.to_string e)
