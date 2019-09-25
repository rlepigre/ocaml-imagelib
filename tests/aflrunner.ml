let perform () =
  let chunk_reader = ImageUtil_unix.chunk_reader_of_path Sys.argv.(1) in
  match ImageLib.PNG.ReadPNG.parsefile chunk_reader with
  | _ -> ()
  | exception End_of_file -> ()
  | exception Image.Corrupted_image(_) -> ()

let () = AflPersistent.run perform
