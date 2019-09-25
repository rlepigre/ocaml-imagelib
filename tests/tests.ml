(* redundant: imagelib.GIF.write *)
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

  let crowbar_gen_cr =
    let content = Crowbar.bytes in
    Crowbar.map [content] chunked_reader_with_png

  let crowbar_png_size () =
    Crowbar.add_test ~name:"ImageLib.PNG.ReadPNG.size"
      [crowbar_gen_cr]
      (fun cr ->
        try Crowbar.check
        (ImageLib.PNG.ReadPNG.size cr <> (0, 0))
        with End_of_file
             | Image.Corrupted_image("Invalid PNG header...")
             | Image.Corrupted_image("Size of chunk greater than OCaml can handle...")
             | Image.Corrupted_image("Reached end of file while looking for end of chunk")
          -> Crowbar.bad_test ())
    
  let fuzzing : unit Alcotest.test_case list =
    [ ("header PNG size", `Slow, crowbar_png_size) ]

  
end

module ImageLib_PNG_tests = struct
  let cr_as = ImageUtil.chunk_reader_of_string

  let chunk_reader_of_string_raises _ =
    Alcotest.(check_raises) "when reading outside bounds, End_of_file is raised"
    End_of_file
    (fun () -> ignore @@ ImageLib.PNG.ReadPNG.size(cr_as "\149\218\249"))

  let regressions : unit Alcotest.test_case list = []

  let unit_tests : unit Alcotest.test_case list =
    [("chunk_reader_of_string raises on EOF", `Quick, chunk_reader_of_string_raises)]
end

let tests : unit Alcotest.test list =
  [ 
    ("unit tests", ImageLib_PNG_tests.unit_tests);
    ("fuzzing", ImageLib_tests.fuzzing);
    ("regressions", ImageLib_PNG_tests.regressions);
    
   ]

let () =
  Alcotest.run "Imagelib tests" tests;
  flush_all ()
