open Image
open ImageUtil_unix

let warning fn msg =
  Printf.eprintf "[WARNING imagelib] file %s\n" fn;
  Printf.eprintf "  %s\n" msg;
  Printf.eprintf "  PNG is the preferred format!\n%!"

let convert fn fn' =
  let ret =
    (* don't accidentally put command-line options here *)
    assert (String.get fn  0 <> '-');
    assert (String.get fn' 0 <> '-');
    Unix.create_process "convert" [| "convert"; fn ; fn' |]
      (* "--" ; see:
       https://github.com/rlepigre/ocaml-imagelib/pull/15#discussion_r198867027
      *)
      Unix.stdin Unix.stdout Unix.stderr in
  if ret <> 0 then
    raise (Failure (Printf.sprintf "convert fn:%S fn':%S failed" fn fn'))

let rm fn =
  Sys.remove fn

let size fn =
  let extension = (get_extension' fn) in
  let ich = chunk_reader_of_path fn in
  try ImageLib.size ~extension ich with
  | Image.Not_yet_implemented _ ->
    begin
      warning fn "No support for image size...";
      let fn' = Filename.temp_file "image" ".png" in
      convert fn fn';
      let ich' = ImageUtil_unix.chunk_reader_of_path fn' in
      let sz = ImagePNG.size ich' in
      rm fn'; sz
    end

let openfile fn : image =
  let extension = (get_extension' fn) in
  let ich = chunk_reader_of_path fn in
  let fallback () =
    (* This will run imagemagick's "convert" utility to
       transform the picture to PNG, then use the mature PNG reader.
    *)
    warning fn "Cannot read this image format...";
    let fn' = Filename.temp_file "image" ".png" in
    convert fn fn';
    let ich' = ImageUtil_unix.chunk_reader_of_path fn' in
    let img = ImagePNG.parsefile ich' in
    rm fn'; img
  in
  if extension = "gif" then
    fallback ()
    (* GIF support is still limited, to avoid breaking existing applications
       we do not use it from the _unix module. *)
  else
  try ImageLib.openfile ~extension ich with
  | Image.Not_yet_implemented _ -> fallback ()

let writefile fn i =
  let extension = get_extension' fn in
  let och = ImageUtil_unix.chunk_writer_of_path fn in
  let fallback () =
    (* This will run the PNG writer, then use
       imagemagick's "convert" utility to transform the PNG
       into the target output format.
    *)
    warning fn "Cannot write to this image format...";
    let fn' = Filename.temp_file "image" ".png" in
    ImagePNG.write_png och i;
    convert fn' fn;
    rm fn'
  in
  if extension = "gif" then
    fallback ()
    (* GIF support is still limited, to avoid breaking existing applications
       we do not use it from the _unix module. *)
  else
    try ImageLib.writefile ~extension och i with
    | Not_yet_implemented _ -> fallback ()
