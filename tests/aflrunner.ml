
let crowbar_skip_known_errors f =
(fun cr ->
    try Crowbar.check
    (f cr)
    with End_of_file
            | Image.Corrupted_image("Invalid PNG header...")
            | Image.Corrupted_image("Size of chunk greater than OCaml can handle...")
            | Image.Corrupted_image("Reached end of file while looking for end of chunk")
        -> Crowbar.bad_test ())

let crowbar_png_parsefile () =
  (crowbar_skip_known_errors(fun cr -> (ignore @@ ImageLib.PNG.ReadPNG.parsefile cr; true)))

let () =
  let cr = ImageUtil_unix.chunk_reader_of_path Sys.argv.(1) in
  match ImageLib.PNG.ReadPNG.parsefile cr with
  | _ -> ()
  | exception End_of_file -> ()
  | exception Image.Corrupted_image("Invalid PNG header...") -> ()
  | exception Image.Corrupted_image("Size of chunk greater than OCaml can handle...") -> ()
  | exception Image.Corrupted_image("Reached end of file while looking for end of chunk") -> ()



