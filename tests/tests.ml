(* redundant: imagelib.GIF.write *)
module Imagelib_tests = struct

  let crowbar_gen_reader
  : ImageUtil.chunk_reader Crowbar.gen =
    let content = Crowbar.bytes in
    Crowbar.map [content] (fun x ->
      fun _bytesorclose ->
        Ok x
    )

  let crowbar_png_size () =
    Crowbar.add_test ~name:"ImageLib.PNG.ReadPNG.size"
      [crowbar_gen_reader]
      (fun cr ->
        try Crowbar.check
        (ignore @@ ImageLib.PNG.ReadPNG.size cr
        ; false)
        with _ -> Crowbar.bad_test ())
    

  let fuzzing : unit Alcotest.test_case list =
    [ ("header PNG size", `Slow, crowbar_png_size) ]
end

let tests : unit Alcotest.test list =
  [ ("fuzzing", Imagelib_tests.fuzzing) ]

let () =
  Alcotest.run "Imagelib tests" tests;
  flush_all ()
