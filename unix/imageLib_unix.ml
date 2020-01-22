open Image
open ImageUtil_unix

module PNG = ImagePNG
module PPM = ImagePPM
module XCF = ImageXCF
module JPG = ImageJPG
module GIF = ImageGIF

let warning fn msg =
  Printf.eprintf "[WARNING imagelib] file %s\n" fn;
  Printf.eprintf "  %s\n" msg;
  Printf.eprintf "  PNG is the prefered format!\n%!"

let convert fn fn' =
  let ret =
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
      let sz = ImagePNG.ReadPNG.size ich' in
      rm fn'; sz
    end

let openfile fn : image =
  let extension = (get_extension' fn) in
  let ich = chunk_reader_of_path fn in
  try ImageLib.openfile ~extension ich with
  | Image.Not_yet_implemented _ ->
    begin
      warning fn "Cannot read this image format...";
      let fn' = Filename.temp_file "image" ".png" in
      convert fn fn';
      let ich' = ImageUtil_unix.chunk_reader_of_path fn' in
      let img = ImagePNG.ReadPNG.parsefile ich' in
      rm fn'; img
    end

let writefile fn i =
  let extension = get_extension' fn in
  let och = ImageUtil_unix.chunk_writer_of_path fn in
  try ImageLib.writefile ~extension och i with
  | Not_yet_implemented _ ->
    begin
      warning fn "Cannot write to this image format...";
      let fn' = Filename.temp_file "image" ".png" in
      ImagePNG.write_png och i;
      convert fn' fn;
      rm fn'
    end
