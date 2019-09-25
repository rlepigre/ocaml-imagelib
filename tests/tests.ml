(* redundant: imagelib.GIF.write *)
module Imagelib_tests = struct

  let crowbar_gen_reader
  : ImageUtil.chunk_reader Crowbar.gen =
    let content = Crowbar.bytes in
    Crowbar.map [content] (fun x ->
      fun _bytesorclose -> Ok x
    )
    (* Crowbar.const @@ fun _bytesorclose ->
      Error(`End_of_file 8) *)

  let crowbar_png_size () =
    Crowbar.add_test ~name:"ImageLib.PNG.ReadPNG.size"
      [crowbar_gen_reader]
      (fun cr ->
        try Crowbar.check
        (ImageLib.PNG.ReadPNG.size cr <> (0, 0))
        with End_of_file |
             Image.Corrupted_image("Invalid PNG header...")
          -> Crowbar.bad_test ())
    
  let fuzzing : unit Alcotest.test_case list =
    [ ("header PNG size", `Slow, crowbar_png_size) ]

  
end

module ImageLib_PNG_tests = struct
  let reg01 _ =
    ()
  let regressions : unit Alcotest.test_case list =
    [ ("01: \"\\149\\218\\249 Invalid_argument(\"String.sub / Bytes.sub\")", `Quick, reg01)]
end

let tests : unit Alcotest.test list =
  [ ("fuzzing", Imagelib_tests.fuzzing);
    ("regressions", ImageLib_PNG_tests.regressions)
   ]

let () =
  Alcotest.run "Imagelib tests" tests;
  flush_all ()
