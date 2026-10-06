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

  let regressions : unit Alcotest.test_case list = []

  let unit_tests : unit Alcotest.test_case list =
    ["chunk_reader_of_string raises on EOF", `Quick, chunk_reader_of_string_raises
    ; "self-test-1", `Quick, self_test_1]
end

(* ------------------------------------------------------------------ *)
(* JPEG tests.                                                         *)
(* ------------------------------------------------------------------ *)

(* The decoder is checked against reference images produced by libjpeg, stored
   beside the fixtures in [jpg/].  Comparing against an independent
   implementation is what gives these tests their value: a matching mistake in
   our own encoder and decoder would cancel out in a pure round trip.

   The encoder is checked by round tripping through our own decoder, and by
   checking the structural properties libjpeg relies on. *)

let fixture_dir = "jpg"

let fixture_path name = Filename.concat fixture_dir (name ^ ".jpg")

let reference_path name =
  Filename.concat fixture_dir (name ^ ".ref.ppm")

let gif_dir = "gif"

let gif_path name = Filename.concat gif_dir name

(* A minimal binary PPM reader, used to load the reference images. *)
let is_ws c = c = ' ' || c = '\n' || c = '\t' || c = '\r'

let read_ppm fn =
  let ic = open_in_bin fn in
  let magic = really_input_string ic 2 in
  if magic <> "P6" then (close_in ic ; failwith (fn ^ ": not a binary PPM")) ;
  let rec skip () =
    match input_char ic with
    | c when is_ws c -> skip ()
    | '#' ->
      while (try input_char ic with End_of_file -> '\n') <> '\n' do () done ;
      skip ()
    | c -> c
  in
  let token () =
    let buf = Buffer.create 8 in
    Buffer.add_char buf (skip ()) ;
    let rec more () =
      match input_char ic with
      | c when is_ws c -> ()
      | exception End_of_file -> ()
      | c -> Buffer.add_char buf c ; more ()
    in
    more () ;
    Buffer.contents buf
  in
  let w = int_of_string (token ()) in
  let h = int_of_string (token ()) in
  ignore (token ()) ;                            (* maxval *)
  let data = really_input_string ic (w * h * 3) in
  close_in ic ;
  (w, h, data)

let decode_fixture name =
  let ic = open_in_bin (fixture_path name) in
  let img = ImageLib.JPG.parsefile
      (ImageUtil_unix.chunk_reader_of_in_channel ic) in
  close_in ic ;
  img

let size_of_fixture name =
  let ic = open_in_bin (fixture_path name) in
  let sz = ImageLib.JPG.size (ImageUtil_unix.chunk_reader_of_in_channel ic) in
  close_in ic ;
  sz

(* Compares [img] against a reference decode, tolerating the small rounding
   differences between our floating-point DCT and libjpeg's fixed-point one.
   Returns the maximum and mean absolute difference over the channels. *)
let diff_to_reference (name : string) (img : Image.image) =
  let w, h, expected = read_ppm (reference_path name) in
  Alcotest.(check int) (name ^ ": width") w img.width ;
  Alcotest.(check int) (name ^ ": height") h img.height ;
  let maxdiff = ref 0 and sum = ref 0L and count = ref 0 in
  for y = 0 to h - 1 do
    for x = 0 to w - 1 do
      let o = ((y * w) + x) * 3 in
      Image.read_rgb img x y (fun r g b ->
        let d = max (abs (r - Char.code expected.[o]))
            (max (abs (g - Char.code expected.[o + 1]))
                 (abs (b - Char.code expected.[o + 2]))) in
        if d > !maxdiff then maxdiff := d ;
        sum := Int64.add !sum (Int64.of_int d) ;
        incr count)
    done
  done ;
  (!maxdiff, Int64.to_float !sum /. float !count)

(* The bounds below hold with a wide margin for these fixtures: the observed
   worst case is a maximum difference of 3 and a mean of 0.52. *)
let check_against_libjpeg name =
  let img = decode_fixture name in
  let maxdiff, mean = diff_to_reference name img in
  Alcotest.(check bool)
    (Printf.sprintf "%s: max difference from libjpeg is %d (limit 6)"
       name maxdiff)
    true (maxdiff <= 6) ;
  Alcotest.(check bool)
    (Printf.sprintf "%s: mean difference from libjpeg is %.3f (limit 2)"
       name mean)
    true (mean <= 2.0)

(* ------------------------------------------------------------------ *)

module ImageLib_JPG_tests = struct

  (* Each entry exercises a different decoder path. *)
  let decoders =
    [ "baseline 4:4:4"    , "baseline_444", (17, 13) ;
      "baseline 4:2:0"    , "baseline_420", (17, 13) ;
      "baseline 4:2:2"    , "baseline_422", (20, 9) ;
      "progressive"       , "progressive", (17, 13) ;
      "greyscale"         , "grey", (16, 16) ;
      "restart intervals" , "restarts", (23, 19) ;
      "flat colour"       , "solid", (16, 16) ;
    ]

  let decoder_tests =
    List.map
      (fun (label, name, _) ->
         (label, `Quick, (fun () -> check_against_libjpeg name)))
      decoders

  let size_tests =
    List.map
      (fun (label, name, (w, h)) ->
         (label ^ ": size", `Quick, (fun () ->
            Alcotest.(check int) "width" w (fst (size_of_fixture name)) ;
            Alcotest.(check int) "height" h (snd (size_of_fixture name)) ;
            let img = decode_fixture name in
            Alcotest.(check int) "size matches parsefile (width)"
              w img.width ;
            Alcotest.(check int) "size matches parsefile (height)"
              h img.height)))
      decoders

  let extensions () =
    Alcotest.(check (list string)) "known extensions"
      [ ".jfi" ; ".jfif" ; ".jif" ; ".jpe" ; ".jpeg" ; ".jpg" ]
      (List.sort String.compare ImageLib.JPG.extensions)

  let solid_is_exact () =
    (* A flat image has only DC coefficients.  The quantiser shifts the colour
       slightly, so what we assert is that the result is uniform and agrees
       with libjpeg exactly -- not that it is the original colour. *)
    let img = decode_fixture "solid" in
    Alcotest.(check int) "max_val" 255 img.max_val ;
    let _, _, expected = read_ppm (reference_path "solid") in
    let er = Char.code expected.[0]
    and eg = Char.code expected.[1]
    and eb = Char.code expected.[2] in
    let mismatches = ref 0 in
    for y = 0 to img.height - 1 do
      for x = 0 to img.width - 1 do
        Image.read_rgb img x y
          (fun r g b -> if (r, g, b) <> (er, eg, eb) then incr mismatches)
      done
    done ;
    Alcotest.(check int)
      (Printf.sprintf "every pixel equals libjpeg's (%d,%d,%d)" er eg eb)
      0 !mismatches

  let greyscale_is_greyscale () =
    let img = decode_fixture "grey" in
    let is_grey =
      match img.pixels with Image.Grey _ -> true | _ -> false in
    Alcotest.(check bool) "single component decodes to a Grey pixmap"
      true is_grey ;
    let uniform = ref true in
    for y = 0 to img.height - 1 do
      for x = 0 to img.width - 1 do
        let v = ref 0 and u = ref 0 in
        Image.read_grey img x y (fun g -> v := g) ;
        Image.read_rgb img x y (fun r _ _ -> u := r) ;
        if !v <> !u then uniform := false
      done
    done ;
    Alcotest.(check bool) "grey and rgb views agree" true !uniform

  (* Note: [Image.Corrupted_image _] is a pattern and so cannot be passed as an
   argument; the check is spelled out with an explicit match instead. *)
  let not_a_jpeg () =
    let outcome = ref `No_exception in
    (try
       ignore
         (ImageLib.JPG.parsefile
            (ImageUtil.chunk_reader_of_string "not a jpeg at all")) ;
       outcome := `Decoded
     with
     | Image.Corrupted_image _ -> outcome := `Corrupted
     | Image.Not_yet_implemented _ -> outcome := `Unsupported) ;
    match !outcome with
    | `Corrupted -> ()
    | other ->
      Alcotest.fail
        (Printf.sprintf "expected Corrupted_image, got %s"
           (match other with
            | `No_exception -> "a decoded image"
            | `Decoded -> "a decoded image"
            | `Unsupported -> "Not_yet_implemented"
            | `Corrupted -> "Corrupted_image"))

  let truncated () =
    (* A truncated file may be rejected outright or decoded leniently -- the
       decoder pads a truncated scan rather than failing -- but it must never
       raise anything else, nor loop. *)
    let ic = open_in_bin (fixture_path "baseline_420") in
    let data = really_input_string ic (in_channel_length ic) in
    close_in ic ;
    let len = String.length data in
    let decoded = ref 0 and rejected = ref 0 and bad = ref [] in
    for cut = 1 to len - 1 do
      let outcome =
        try
          ignore
            (ImageLib.JPG.parsefile
               (ImageUtil.chunk_reader_of_string (String.sub data 0 cut))) ;
          `Decoded
        with
        | Image.Corrupted_image _ -> `Rejected
        | Image.Not_yet_implemented _ -> `Rejected
        | End_of_file -> `Rejected
        | e -> `Unexpected (cut, Printexc.to_string e)
      in
      match outcome with
      | `Decoded -> incr decoded
      | `Rejected -> incr rejected
      | `Unexpected (cut, msg) ->
        bad := (cut, msg) :: !bad
    done ;
    Alcotest.(check (list (pair int string)))
      (Printf.sprintf
         "truncations that raised something unexpected (of %d, %d decoded \
          leniently and %d rejected)" (len - 1) !decoded !rejected)
      [] !bad ;
    Alcotest.(check bool) "every truncation was handled" true
      (!decoded + !rejected = len - 1)

  let unit_tests : unit Alcotest.test_case list =
    [ "extensions", `Quick, extensions
    ; "a flat image decodes exactly", `Quick, solid_is_exact
    ; "greyscale images decode to Grey", `Quick, greyscale_is_greyscale
    ; "non-JPEG input is rejected", `Quick, not_a_jpeg
    ; "truncated input is rejected", `Quick, truncated
    ]
    @ decoder_tests @ size_tests

  let regressions : unit Alcotest.test_case list = []
end

(* ------------------------------------------------------------------ *)
(* GIF tests.                                                          *)
(* ------------------------------------------------------------------ *)

(* The GIF decoder is exercised against expectations recorded from libgif's
   own decode of the fixtures, so every frame is pinned exactly.  The fixtures
   cover the cases that used to be refused or mis-decoded. *)

module ImageLib_GIF_tests = struct

  (* [sum] is a weighted sum of every pixel of a frame and [pos] a positional
     checksum, both over RGB; together they detect any pixel difference. *)
  let frame_checksum (img : Image.image) : int64 * int64 =
    let sum = ref 0L and pos = ref 0L in
    for y = 0 to img.height - 1 do
      for x = 0 to img.width - 1 do
        Image.read_rgb img x y (fun r g b ->
          sum := Int64.add !sum (Int64.of_int (r + (g * 256) + (b * 65536))) ;
          pos := Int64.add !pos (Int64.of_int ((r * 3) + (g * 5) + (b * 7))))
      done
    done ;
    (!sum, !pos)

  (* Decode every frame of a GIF.  This also checks that the stream terminates
     cleanly instead of raising. *)
  let decode_frames name =
    let ic = ImageUtil_unix.chunk_reader_of_path (gif_path name) in
    let rec go state acc =
      match ImageLib.openfile_streaming ~extension:".gif" ic state with
      | None, _, _ -> List.rev acc
      | Some img, _, next ->
        let sum, pos = frame_checksum img in
        go next
          ((img.Image.width, img.Image.height, sum, pos) :: acc)
    in
    go None []

  (* [expected] lists the (width, height, sum, pos) that libgif produced for
     each frame of the fixture.  Comparing frame by frame rather than as one
     list keeps the failure message pointing at the frame that differs.

     Note that OCaml reads [a, b, c, d] as [(a, b), (c, d)], so these records
     are pairs of pairs; that is consistent on both sides. *)
  let check_gif name expected =
    let got = decode_frames name in
    Alcotest.(check int)
      (name ^ ": number of frames") (List.length expected) (List.length got) ;
    List.iter2
      (fun (n, (ew, eh, esum, epos)) (gw, gh, gsum, gpos) ->
         Alcotest.(check int)
           (Printf.sprintf "%s frame %d: width" name n) ew gw ;
         Alcotest.(check int)
           (Printf.sprintf "%s frame %d: height" name n) eh gh ;
         Alcotest.(check int64)
           (Printf.sprintf "%s frame %d: pixel sum" name n) esum gsum ;
         Alcotest.(check int64)
           (Printf.sprintf "%s frame %d: positional sum" name n) epos gpos)
      (List.mapi (fun i e -> (i + 1, e)) expected) got

  let interlaced () = check_gif "interlaced.gif"
      [ 23, 19, 2263331793L, 655045L ]

  let local_table () = check_gif "local_table.gif"
      [ 20, 16, 104877600L, 71200L
      ; 20, 16, 105696800L, 87200L
      ; 20, 16, 106516000L, 103200L
      ]

  let multi_frame () = check_gif "multi_frame.gif"
      [ 24, 18, 0L, 0L
      ; 24, 18, 3718012752L, 636336L
      ; 24, 18, 188268192L, 498528L
      ; 24, 18, 3906280944L, 1134864L
      ; 24, 18, 348224832L, 444096L
      ]

  let comment () = check_gif "comment.gif"
      [ 16, 12, 1133975040L, 264960L ]

  let palette256 () = check_gif "palette256.gif"
      [ 21, 15, 2640131185L, 602756L ]

  let unit_tests : unit Alcotest.test_case list = [
    "interlaced single frame", `Quick, interlaced ;
    "animation with local colour tables", `Quick, local_table ;
    "multi-frame animation", `Quick, multi_frame ;
    "comment extension is skipped", `Quick, comment ;
    "256 entry palette", `Quick, palette256 ;
  ]

  let regressions : unit Alcotest.test_case list = []
end

(* ------------------------------------------------------------------ *)

module ImageLib_JPG_encoder_tests = struct

  (* A gradient with enough structure to produce nonzero AC coefficients. *)
  let make_gradient w h =
    let img = Image.create_rgb w h in
    for y = 0 to h - 1 do
      for x = 0 to w - 1 do
        Image.write_rgb img x y
          ((x * 255) / max 1 (w - 1))
          ((y * 255) / max 1 (h - 1))
          (((x + y) * 255) / max 1 (w + h - 2))
      done
    done ;
    img

  let roundtrip ?quality ?subsampling img =
    let encoded = ImageLib.JPG.bytes_of_jpg ?quality ?subsampling img in
    ImageLib.JPG.parsefile
      (ImageUtil.chunk_reader_of_string (Bytes.to_string encoded))

  let roundtrip_error ?quality ?subsampling img =
    let decoded = roundtrip ?quality ?subsampling img in
    Alcotest.(check int) "width is preserved" img.width decoded.width ;
    Alcotest.(check int) "height is preserved" img.height decoded.height ;
    Alcotest.(check int) "max_val is 255" 255 decoded.max_val ;
    let maxdiff = ref 0 and sum = ref 0L in
    for y = 0 to img.height - 1 do
      for x = 0 to img.width - 1 do
        Image.read_rgb img x y (fun r g b ->
          Image.read_rgb decoded x y (fun r' g' b' ->
            let d =
              max (abs (r - r')) (max (abs (g - g')) (abs (b - b'))) in
            if d > !maxdiff then maxdiff := d ;
            sum := Int64.add !sum (Int64.of_int d)))
      done
    done ;
    let pixels = img.width * img.height in
    (!maxdiff, Int64.to_float !sum /. float pixels)

  let check_roundtrip label ?quality ?subsampling
      ~max_mean ~max_max img =
    let maxdiff, mean = roundtrip_error ?quality ?subsampling img in
    Alcotest.(check bool)
      (Printf.sprintf "%s: mean error %.3f (limit %.2f)" label mean max_mean)
      true (mean <= max_mean) ;
    Alcotest.(check bool)
      (Printf.sprintf "%s: max error %d (limit %d)" label maxdiff max_max)
      true (maxdiff <= max_max)

  (* The quantisation error grows as quality falls, so each quality gets its own
   bound.  These are generous: the encoder was verified to agree with libjpeg's
   own output to within a mean of 1 at every quality. *)
let every_quality () =
  let img = make_gradient 32 24 in
  List.iter
    (fun (q, max_mean, max_max) ->
       check_roundtrip
         (Printf.sprintf "quality %d" q)
         ~quality:q ~subsampling:ImageLib.JPG.Four_four_four
         ~max_mean ~max_max img)
    [ 100, 4.0, 40 ; 90, 4.0, 40 ; 75, 5.0, 40 ; 50, 8.0, 40 ;
      25, 14.0, 60 ; 10, 18.0, 80 ]

  (* At quality 1 the quantiser is so coarse that a gradient is destroyed
     entirely, so only the structure of the result is meaningful. *)
  let degenerate_quality () =
    let img = make_gradient 32 24 in
    let decoded = roundtrip ~quality:1 img in
    Alcotest.(check int) "width is preserved" img.width decoded.width ;
    Alcotest.(check int) "height is preserved" img.height decoded.height ;
    Alcotest.(check int) "max_val is 255" 255 decoded.max_val

  let every_subsampling () =
    let img = make_gradient 32 24 in
    List.iter
      (fun (label, sub) ->
         check_roundtrip label ~quality:80 ~subsampling:sub
           ~max_mean:8.0 ~max_max:60 img)
      [ ("4:4:4", ImageLib.JPG.Four_four_four) ;
        ("4:2:2", ImageLib.JPG.Four_two_two) ;
        ("4:2:0", ImageLib.JPG.Four_two_zero) ]

  let odd_dimensions () =
    (* Dimensions that are not multiples of the block size, so the encoder must
       pad the last blocks of a row and of the image. *)
    List.iter
      (fun (w, h) ->
         let img = make_gradient w h in
         check_roundtrip (Printf.sprintf "%dx%d" w h) ~quality:90
           ~max_mean:4.0 ~max_max:40 img)
      [ (1, 1) ; (1, 17) ; (17, 1) ; (7, 8) ; (9, 9) ; (17, 33) ; (33, 17) ]

  let greyscale_roundtrip () =
    let img = Image.create_grey 21 13 in
    for y = 0 to 12 do
      for x = 0 to 20 do
        Image.write_grey img x y ((x * 11) + (y * 5) land 0xff)
      done
    done ;
    let decoded = roundtrip ~quality:85 img in
    (match decoded.pixels with
     | Image.Grey _ -> ()
     | _ -> Alcotest.fail "a greyscale image must encode as one component") ;
    check_roundtrip "greyscale" ~quality:85 ~max_mean:4.0 ~max_max:40 img

  let alpha_is_ignored () =
    let img = Image.create_rgb ~alpha:true 16 16 in
    Image.fill_rgb img 200 100 50 ;
    Image.fill_alpha img 0 ;
    ignore (roundtrip ~quality:90 img)          (* must not raise *)

  let high_bit_depth_is_scaled () =
    (* JPEG is 8-bit, so a 16-bit image must be scaled down rather than
       rejected or wrapped around. *)
    let img = Image.create_rgb ~max_val:65535 16 16 in
    for y = 0 to 15 do
      for x = 0 to 15 do
        Image.write_rgb img x y (x * 4096) (y * 4096) 0
      done
    done ;
    let decoded = roundtrip ~quality:95 img in
    Alcotest.(check int) "max_val" 255 decoded.max_val ;
    let v = ref (-1) in
    Image.read_rgb decoded 15 15 (fun r _ _ -> v := r) ;
    Alcotest.(check bool)
      (Printf.sprintf "the brightest pixel stays bright (got %d)" !v)
      true (!v > 200)

  let one_pixel () =
    let img = Image.create_rgb 1 1 in
    Image.fill_rgb img 10 20 30 ;
    let decoded = roundtrip ~quality:100 img in
    Alcotest.(check int) "width" 1 decoded.width ;
    Alcotest.(check int) "height" 1 decoded.height ;
    let r = ref (-1) and g = ref (-1) and b = ref (-1) in
    Image.read_rgb decoded 0 0 (fun a b' c -> r := a ; g := b' ; b := c) ;
    Alcotest.(check int) "red" 10 !r ;
    Alcotest.(check int) "green" 20 !g ;
    Alcotest.(check int) "blue" 30 !b

  let structure_is_valid () =
    (* Spot-check the header: libjpeg requires SOI first, a frame header with
       the right size, and EOI last. *)
    let img = make_gradient 16 16 in
    let enc = Bytes.to_string (ImageLib.JPG.bytes_of_jpg ~quality:75 img) in
    let n = String.length enc in
    Alcotest.(check string) "starts with SOI" "\255\216"
      (String.sub enc 0 2) ;
    Alcotest.(check string) "ends with EOI" "\255\217"
      (String.sub enc (n - 2) 2) ;
    let pos = ref 2 and found = ref false in
    while (not !found) && !pos < n - 4 do
      if enc.[!pos] = '\255' then begin
        match enc.[!pos + 1] with
        | '\192' ->
          (* SOF0: marker, length, precision, height, width, components *)
          let h = (Char.code enc.[!pos + 5] lsl 8)
              lor Char.code enc.[!pos + 6]
          and w = (Char.code enc.[!pos + 7] lsl 8)
              lor Char.code enc.[!pos + 8] in
          Alcotest.(check int) "frame width" 16 w ;
          Alcotest.(check int) "frame height" 16 h ;
          Alcotest.(check int) "sample precision" 8
            (Char.code enc.[!pos + 4]) ;
          found := true
        | m when Char.code m >= 0xc0 && Char.code m <= 0xcf -> found := true
        | _ ->
          let len =
            (Char.code enc.[!pos + 2] lsl 8) + Char.code enc.[!pos + 3] in
          pos := !pos + 2 + len
        | exception _ -> found := true
      end else incr pos
    done ;
    Alcotest.(check bool) "a frame header was found" true !found

  let writefile_dispatch () =
    (* [ImageLib.writefile] must route .jpg to the JPEG encoder. *)
    let img = make_gradient 16 16 in
    let buf = Buffer.create 4096 in
    let och = ImageUtil.chunk_writer_of_buffer buf in
    ImageLib.writefile ~extension:".jpg" och img ;
    ImageUtil.close_chunk_writer och ;
    let data = Buffer.contents buf in
    Alcotest.(check bool) "produces a JPEG" true
      (String.length data > 4 && String.sub data 0 2 = "\255\216")

  let unit_tests : unit Alcotest.test_case list = [
    "round trip at every quality", `Quick, every_quality ;
    "quality 1 still produces a valid image", `Quick, degenerate_quality ;
    "round trip at every subsampling", `Quick, every_subsampling ;
    "round trip with awkward dimensions", `Quick, odd_dimensions ;
    "greyscale round trip", `Quick, greyscale_roundtrip ;
    "alpha is ignored", `Quick, alpha_is_ignored ;
    "16-bit images are scaled", `Quick, high_bit_depth_is_scaled ;
    "1x1 image round trips exactly", `Quick, one_pixel ;
    "emitted headers are well formed", `Quick, structure_is_valid ;
    "writefile dispatches .jpg", `Quick, writefile_dispatch ;
  ]

  let regressions : unit Alcotest.test_case list = []
end

(* ------------------------------------------------------------------ *)
(* Extension dispatch.                                                 *)
(* ------------------------------------------------------------------ *)

(* [ImageLib] selects a format by matching the [~extension:] argument against
   each format's [extensions], which are all written with a leading dot.
   [Filename.extension] yields that dotted form, but the public [~extension:]
   parameters do not require it, so a caller may equally pass the bare name.
   Every spelling -- with or without the dot, in any case -- must dispatch to
   the same format instead of raising [Not_yet_implemented]. *)
module ImageLib_dispatch_tests = struct

  (* Run [f ext] for each spelling, failing the test if [f] reports the format
     as not implemented. *)
  let for_each_spelling name exts f =
    List.iter
      (fun ext ->
         (try f ext
          with Image.Not_yet_implemented e ->
             Alcotest.fail
               (Printf.sprintf "%s: extension %S reported Not_yet_implemented (%S)"
                  name ext e)))
      exts

  let check_png (img : Image.image) (png : string) (ext : string) =
    let w, h = ImageLib.size ~extension:ext (ImageUtil.chunk_reader_of_string png) in
    Alcotest.(check int) (ext ^ ": width") img.width w;
    Alcotest.(check int) (ext ^ ": height") img.height h;
    let dec = ImageLib.openfile ~extension:ext (ImageUtil.chunk_reader_of_string png) in
    Alcotest.(check int) (ext ^ ": openfile") 0 (Image.compare_image img dec);
    (match ImageLib.openfile_streaming ~extension:ext (ImageUtil.chunk_reader_of_string png) None with
     | Some dec, _, _ ->
         Alcotest.(check int) (ext ^ ": streaming") 0 (Image.compare_image img dec)
     | None, _, _ -> Alcotest.fail (ext ^ ": streaming returned no image"));
    let buf = Buffer.create 0 in
    let och = ImageUtil.chunk_writer_of_buffer buf in
    ImageLib.writefile ~extension:ext och img;
    ImageUtil.close_chunk_writer och;
    let data = Buffer.contents buf in
    Alcotest.(check bool) (ext ^ ": writefile PNG magic") true
      (String.length data >= 4 && String.sub data 0 4 = "\137PNG")

  let check_jpg (img : Image.image) (ext : string) =
    let enc = Bytes.to_string (ImageLib.JPG.bytes_of_jpg img) in
    let w, h = ImageLib.size ~extension:ext (ImageUtil.chunk_reader_of_string enc) in
    Alcotest.(check int) (ext ^ ": width") img.width w;
    Alcotest.(check int) (ext ^ ": height") img.height h;
    (* JPEG is lossy, so only the dimensions must survive the round trip. *)
    let dec = ImageLib.openfile ~extension:ext (ImageUtil.chunk_reader_of_string enc) in
    Alcotest.(check int) (ext ^ ": openfile dimensions") 0
      (max (abs (dec.width - img.width)) (abs (dec.height - img.height)));
    let buf = Buffer.create 0 in
    let och = ImageUtil.chunk_writer_of_buffer buf in
    ImageLib.writefile ~extension:ext och img;
    ImageUtil.close_chunk_writer och;
    let data = Buffer.contents buf in
    Alcotest.(check bool) (ext ^ ": writefile JPEG SOI") true
      (String.length data >= 2 && String.sub data 0 2 = "\255\216")

  let png_and_jpg_dispatch () =
    let img = Image.create_rgb 3 3 in
    Image.fill_rgb img 10 20 30;
    let png = Bytes.to_string (ImageLib.PNG.bytes_of_png img) in
    for_each_spelling "png" [ ".png"; "png"; ".PNG"; "PnG" ] (check_png img png);
    for_each_spelling "jpg" [ ".jpg"; "jpg"; ".JPG" ] (check_jpg img)

  let unit_tests : unit Alcotest.test_case list = [
    "extensions dispatch with or without a leading dot", `Quick, png_and_jpg_dispatch
  ]

  let regressions : unit Alcotest.test_case list = []
end

let tests : unit Alcotest.test list =
  [
    "PNG unit tests", ImageLib_PNG_tests.unit_tests;
    ("PNG regressions", ImageLib_PNG_tests.regressions);
    "JPG unit tests", ImageLib_JPG_tests.unit_tests;
    ("JPG regressions", ImageLib_JPG_tests.regressions);
    "GIF unit tests", ImageLib_GIF_tests.unit_tests;
    ("GIF regressions", ImageLib_GIF_tests.regressions);
    "JPG encoder unit tests", ImageLib_JPG_encoder_tests.unit_tests;
    ("JPG encoder regressions", ImageLib_JPG_encoder_tests.regressions);
    "extension dispatch", ImageLib_dispatch_tests.unit_tests;
    ("extension dispatch regressions", ImageLib_dispatch_tests.regressions);
  ]

let () =
  Alcotest.run "Imagelib tests" tests;
  flush_all ()