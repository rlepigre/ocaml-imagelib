module ImageLib_tests = struct

  (* let crowbar_gen_reader
  : ImageUtil.chunk_reader Crowbar.gen =
    let content = Crowbar.bytes in
    Crowbar.map [content] (fun x ->
      fun _bytesorclose -> Ok x
    )
    (* Crowbar.const @@ fun _bytesorclose ->
      Error(`End_of_file 8) *) *)

  let chunked_reader_with_png s =
    ImageUtil.chunk_reader_of_string @@ ImagePNG.png_signature ^ s

  let crowbar_gen_cr_png =
    let content = Crowbar.bytes in
    Crowbar.map [content] chunked_reader_with_png

  let crowbar_gen_cr_gif =
    let content = Crowbar.bytes in
    Crowbar.map [content] (fun s -> ImageUtil.chunk_reader_of_string ("GIF89a" ^s))

  let crowbar_skip_known_errors f =
    (fun cr ->
        try Crowbar.check
        (f cr)
        with End_of_file
           | Out_of_memory
           | Image.Not_yet_implemented _
           | Image.Corrupted_image("Invalid PNG header...")
           | Image.Corrupted_image("Size of chunk greater than OCaml can handle...")
           | Image.Corrupted_image("Reached end of file while looking for end of chunk")
           | Image.Corrupted_image ("GIF signature expected...")
          -> Crowbar.bad_test ())


  let crowbar_png_size () =
    Crowbar.add_test ~name:"ImageLib.GIF.ReadGIF.openfile"
      [crowbar_gen_cr_gif]
      (crowbar_skip_known_errors(fun cr -> (ignore @@ ImageLib.GIF.parsefile cr; true)))
      ;
    Crowbar.add_test ~name:"ImageLib.PNG.size"
      [crowbar_gen_cr_png]
      (crowbar_skip_known_errors(fun cr -> (ImageLib.PNG.size cr <> (0, 0))))

  let crowbar_png_parsefile () =
    Crowbar.add_test ~name:"ImageLib.PNG.openfile"
      [crowbar_gen_cr_png]
      (crowbar_skip_known_errors(fun cr -> (ignore @@ ImageLib.PNG.parsefile cr; true)))

  let fuzzing : unit Alcotest.test_case list =
    [
      (* "PNG fuzz size", `Slow, crowbar_png_size ; *)
      "PNG fuzz parser", `Slow, crowbar_png_parsefile ;
    ]


end


let tests : unit Alcotest.test list =
  [
    ("fuzzing", ImageLib_tests.fuzzing) ;
  ]

let () =
  Alcotest.run "Imagelib crowbar tests" tests;
  flush_all ()
