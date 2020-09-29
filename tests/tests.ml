(* redundant: imagelib.GIF.write *)
module ImageLib_tests = struct

  let chunked_reader_with_png s =
    ImageUtil.chunk_reader_of_string @@ ImagePNG.png_signature ^ s

end

module ImageLib_PNG_tests = struct
  let cr_as = ImageUtil.chunk_reader_of_string

  let chunk_reader_of_string_raises _ =
    Alcotest.(check_raises) "when reading outside bounds, End_of_file is raised"
    End_of_file
    (fun () -> ignore @@ ImageLib.PNG.size(cr_as "\149\218\249"))

  let self_test_1 () =
    let img = Image.create_rgb 3 3 in
    Image.fill_rgb img 0 0 0;
    let enc = ImageLib.PNG.bytes_of_png img in
    let dec = ImageLib.PNG.parsefile
        (ImageUtil.chunk_reader_of_string (Bytes.to_string enc)) in
    Alcotest.check Alcotest.int "equality" 0 (Image.compare_image img dec) ;
    Image.write_rgb img 0 0 1 0 0 ;
    Alcotest.(check int) "compare works 1" 1 (Image.compare_image img dec) ;
    Alcotest.(check int) "compare works 2" (-1) (Image.compare_image dec img)

  

  let regressions : unit Alcotest.test_case list = [
  ]

  let unit_tests : unit Alcotest.test_case list =
    ["chunk_reader_of_string raises on EOF", `Quick, chunk_reader_of_string_raises
    ; "self-test-1", `Quick, self_test_1]
end

let tests : unit Alcotest.test list =
  [
    "PNG unit tests", ImageLib_PNG_tests.unit_tests;
    ("PNG regressions", ImageLib_PNG_tests.regressions);
   ]

let () =
  Alcotest.run "Imagelib tests" tests;
  flush_all ()
