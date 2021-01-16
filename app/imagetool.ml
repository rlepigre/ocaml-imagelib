module FrameSelect = struct
  type t = (int * int) list
  let of_string frm : t =
    String.split_on_char ',' frm
    |> List.map (fun selection ->
        begin match String.split_on_char '-' selection with
          | [single] -> single,single
          | [first ; last] -> first, last
          | exception Not_found -> selection, selection
              | _ ->
                invalid_arg (Printf.sprintf
                               "Unable to parse frame selection: %S" selection)
        end |> fun (first,last) ->
        (match first with "" -> 1 | _ -> int_of_string first),
        (match last with "" -> max_int | _ -> int_of_string last)
      )
    |> List.sort compare
  let matches t frame_number =
    List.exists (fun (first,last) ->
        frame_number >= first && frame_number <= last) t
end

type config = {
  input_file : string;
  display_mode: [`IRC | `VT100] option;
  background: int; (* RGB *)
  character : string option; (* fill character for terminal printing *)
  crop_x : int;
  crop_y : int;
  gamma: float ;
  resize : [`Original | `Percent of float | `Dim of (int * int) ];
  output_file : string option;
  frames: FrameSelect.t; (* frames from multi-frame image, eg animated GIF *)
}

let arg_parser array : config =
  let rec erase leftover config =
    let set_display_mode mode tl =
      erase leftover {config with display_mode = Some mode} tl in
    function
    | [] -> config, Array.of_list leftover
    | "--help"::_ -> config, [| ""; "" |] (* return extra args *)
    | "--frames"::frm::tl ->
      let frames = FrameSelect.of_string frm in
      erase leftover {config with frames} tl
    | "--background"::bg::tl ->
      erase leftover {config with background = int_of_string bg} tl
    | "--character"::fill::tl ->
      erase leftover {config with character = Some fill} tl
    | "--irc"::tl -> set_display_mode `IRC tl
    | "--vt100"::tl -> set_display_mode `VT100 tl
    | "--gamma"::g::tl ->
      erase leftover {config with gamma = float_of_string g} tl
    | "--resize"::size::tl ->
      let m = String.index_opt size in
      let last = String.length size -1 in
      let resize = match m '%', m 'x' with
        | Some idx , None when idx = last ->
          Printf.printf "xx\n%!";
          `Percent (float_of_string (String.sub size 0 idx))
        | None, Some idx when idx > 0 && idx < String.length size -1 ->
          `Dim String.((int_of_string @@ sub size 0 idx),
                       (int_of_string @@ sub size (succ idx) @@ last-idx))
        | _ -> invalid_arg "Incorrect --resize parameter, see --help"
      in
      erase leftover {config with resize} tl
    | "--crop-x"::w::tl ->
      erase leftover {config with crop_x = int_of_string w} tl
    | "--crop-y"::h::tl ->
      erase leftover {config with crop_y = int_of_string h} tl
    | x :: _ when String.index_opt x '-' = Some 0 ->
      invalid_arg (Printf.sprintf "Unknown switch: %S" x)
    | input_file::tl when config.input_file = "" ->
      erase leftover {config with input_file } tl
    | output_file::tl when config.output_file = None ->
      erase leftover {config with display_mode = None ;
                                  output_file = Some output_file} tl
    | bad::_ when String.index_opt bad '-' = Some 0 ->
      invalid_arg (Printf.sprintf "Missing paramter for %S" bad)
    | unknown::_ ->
      invalid_arg (Printf.sprintf "Unknown argument %S" unknown)
  in
  match erase [] {
      (* default configuration *)
      display_mode = Some `VT100 ;
      input_file = "" ;
      character = None ;
      background = 0;
      gamma = 2.2;
      resize = `Original ;
      output_file = None ;
      frames = [1, max_int];
      crop_x = 65535;
      crop_y = 65535;
    } (Array.to_list array |> List.tl)
  with
  | config, [| |] when config.input_file <> "" -> config
  | _, extraneous ->
    if extraneous <> [| |] then
      Printf.eprintf "Error: extraneous arguments: %s"
      @@ String.concat " " (List.map String.escaped
                              (Array.to_list extraneous));
    Printf.eprintf "usage: %s [args] INPUT-FILE [OUTPUT-FILE]\n"
      Sys.executable_name;
    Printf.eprintf "Displays a picture in the terminal, or convert it ";
    Printf.eprintf "(if OUTPUT-FILE is specified)\n" ;
    Printf.eprintf "The OUTPUT-FILE is a template; any '#' will be replaced with \n";
    Printf.eprintf "the frame number (useful if the image has multiple frames)\n";
    Printf.eprintf "Options:\n";
    Printf.eprintf "--irc | --vt100   Output to terminal using escape codes\n" ;
    Printf.eprintf " \\--character     Fill character(s) for text-mode output\n";
    Printf.eprintf
      "--resize          Set output dimensions. Syntax: '80x25' | '10%%'\n";
    Printf.eprintf "  \\--gamma        Adjust/reinterpret gamma\n";
    Printf.eprintf
      "--crop-x WIDTH    Crop final output to WIDTH pixels\n" ;
    Printf.eprintf
      "--crop-y HEIGHT   Crop final output to HEIGHT pixels\n" ;
    Printf.eprintf
      "--frames RANGE    Only operate on RANGE frames from a multi-frame file format\n" ;
    Printf.eprintf
      "                  (like an animated GIF). The format is comma-separated with '-' separating each range\n" ;
    Printf.eprintf
      "          y          example: --frames 1-10,20-30,50-\n" ;
    Printf.eprintf
      "                    example: --frames -200,300-400\n" ;
    Printf.eprintf "--background      Set background color for transparency";
    Printf.eprintf " using RGB,\n";
    Printf.eprintf "                  e.g. '0xFF0000' is red.";
    Printf.eprintf " Defaults to black.\n" ;
    exit 2

let () =
  let config = arg_parser Sys.argv in
  let extension filename =
    let ri =
      try 1 (* the . itself *) + String.rindex filename '.'
      with Not_found -> invalid_arg "filename without extension"
    in
    String.(sub filename ri @@ (length filename) - ri)
  in
  let foreach_pixel f img =
    for y = 0 to (min config.crop_y img.Image.height) -1 do
      let current = ref (-1,-1,-1) in
      for x = 0 to (min config.crop_x img.width) -1 do
        Image.read_rgba img x y (fun r g b a ->
            let out, next = f ?current:(Some !current) r g b a in
            current := next;
            print_string out)
      done; print_endline "\x1b[0m\r" ;
    done
  in
  let read_next =
    ImageLib.openfile_streaming ~extension:(extension config.input_file)
      (ImageUtil_unix.chunk_reader_of_path config.input_file) in
  let handle_resize input_img =
    let doit x y =
      let alpha = match input_img.Image.pixels with
        | RGBA _ | GreyA _ -> true
        | RGB _ | Grey _ -> false in
      let x, y = max 1 x, max 1 y in
      let dst = Image.create_rgb ~alpha x y in
      Image.Resize.scale_copy_layer dst ~src:input_img config.gamma
    in
    match config.resize with
    | `Original -> input_img
    | `Dim (x,y) -> doit x y
    | `Percent p -> doit (int_of_float @@ float input_img.width *. p /. 100.0)
                      (int_of_float @@ float input_img.height *. p /. 100.0)
  in
  let handle_crop (img:Image.image) =
    {img with width = min img.width config.crop_x ;
              height = min img.height config.crop_y }
  in
  let foreach_img ~frames f_pre f =
    let rec loop frame_number last_parsed_ts state =
      match read_next state with
      | None, _, _ -> ()
      | Some o'img, delay, state ->
        let img = handle_resize o'img |> handle_crop in
        let delay = float delay /. 100. in
        let parsed_ts = Unix.gettimeofday() in
        if FrameSelect.matches frames frame_number then begin
          Unix.sleepf (max 0. (delay -. (parsed_ts -. last_parsed_ts)));
          f_pre ();
          foreach_pixel f img ;
          Printf.printf "%!";
        end ;
        if state <> None then loop (succ frame_number) parsed_ts state
    in loop 1 0. None
  in
  match config with
  | { display_mode = Some `VT100 ; background ; character ; frames ; _ } ->
    (* TODO: only the first frame should paint the alpha color; after that
       we should use the background color when the image is emitting alpha
       updates *)
    (* print to terminal: using 24-bit color escape codes  *)
    foreach_img ~frames
      (fun () -> Printf.printf "\x1b[0;0H") (* a bit annoying for debugging *)
      (ImageUtil.colorize_rgba8888 ?character ~background) ;
    print_string "\x1b[0m"

  | { display_mode = Some `IRC ; background ; frames ; _ } ->
    (* print to terminal, using IRC 24-bit color escape codes *)
    foreach_img ~frames (fun () -> ())
      (ImageUtil.colorize_rgba8888_irc ~background)

  | { output_file = Some fn ; frames ; _ } ->
    let output_this fn img =
      let img = handle_resize img |> handle_crop in
      (* output to filename specified in second argument *)
      if Sys.(file_exists fn)
      then begin
        Printf.eprintf "Output file %S already exists\n" fn ;
        exit 1
      end else begin
        let extension = extension fn in
        let wr = ImageUtil_unix.chunk_writer_of_path fn in
        ImageLib.writefile ~extension wr img
      end
    in
    let rec loop frame_number = function
      | Some image, _delay, state ->
        if FrameSelect.matches frames frame_number then
          let fn = match String.split_on_char '#' fn with
            | exception _ -> fn
            | parts -> String.concat (string_of_int frame_number) parts in
          output_this fn image ;
        loop (succ frame_number) (read_next state)
      | None , _, _ -> exit 0
    in
    loop 1 (read_next None)

  | _ -> invalid_arg "too many argv arguments"
