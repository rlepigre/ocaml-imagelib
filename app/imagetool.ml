type config = {
  input_file : string;
  display_mode: [`IRC | `VT100] option;
  background: int; (* RGB *)
  output_file : string option;
}

let arg_parser array : config =
  let rec erase leftover config =
    let set_display_mode mode tl =
      erase leftover {config with display_mode = Some mode} tl in
    function
    | [] -> config, Array.of_list leftover
    | "--help"::_ -> config, [| ""; "" |] (* return extra args *)
    | "--background"::bg::tl ->
      erase leftover {config with background = int_of_string bg} tl
    | "--irc"::tl -> set_display_mode `IRC tl
    | "--vt100"::tl -> set_display_mode `VT100 tl
    | x :: _ when String.index_opt x '-' = Some 0 ->
      invalid_arg (Printf.sprintf "Unknown switch: %S" x)
    | input_file::tl when config.input_file = "" ->
      erase leftover {config with input_file } tl
    | output_file::tl when config.output_file = None ->
      erase leftover {config with display_mode = None ;
                      output_file = Some output_file} tl
    | unknown::_ ->
      invalid_arg (Printf.sprintf "Unknown argument %S" unknown)
  in
  match erase [] { display_mode = Some `VT100 ;
                   input_file = "" ;
                   background = 0;
                   output_file = None ;
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
    Printf.eprintf "Options:\n";
    Printf.eprintf "--irc | --vt100   Output to terminal using escape codes\n" ;
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
    for y = 0 to img.Image.height -1 do
      let current = ref (-1,-1,-1) in
      for x = 0 to img.width -1 do
        Image.read_rgba img x y (fun r g b a ->
            let out, next = f ?current:(Some !current) r g b a in
            current := next;
            print_string out)
      done; print_newline () ;
    done
  in
  let read_next =
    ImageLib.openfile_streaming ~extension:(extension config.input_file)
      (ImageUtil_unix.chunk_reader_of_path config.input_file) in
  let foreach_img f_pre f =
    let rec loop last_parsed_ts state =
      match read_next state with
      | None, _, _ -> ()
      | Some img, delay, state ->
        let delay = float delay /. 100. in
        let parsed_ts = Unix.gettimeofday() in
        Unix.sleepf (max 0. (delay -. (parsed_ts -. last_parsed_ts)));
        f_pre ();
        foreach_pixel f img ;
        Printf.printf "%!";
        if state <> None then loop parsed_ts state
    in loop 0. None
  in
  match config with
  | { display_mode = Some `VT100 ; background ; _ } ->
    (* print to terminal: using 24-bit color escape codes  *)
    foreach_img
      (fun () -> Printf.printf "\x1b[0;0H") (* a bit annoying for debugging *)
      (ImageUtil.colorize_rgba8888 ~background) ;
    print_string "\x1b[0m"

  | { display_mode = Some `IRC ; background ; _ } ->
    (* print to terminal, using IRC 24-bit color escape codes *)
    foreach_img (fun () -> ())
      (ImageUtil.colorize_rgba8888_irc ~background)

  | { output_file = Some fn ; _ } ->
    let img = match read_next None with
      | Some img, _, None -> img
      | None, _, _ -> Printf.eprintf "No frames in image." ; exit 1
      | Some img, _, (Some _ as st) when (None, 0, None) = read_next st -> img
      | Some _, _, Some _ ->
        Printf.eprintf "TODO NOT IMPLEMENTED: Writing image with >1 frames" ;
        exit 1
    in
    (* output to filename specified in second argument *)
    if Sys.(file_exists fn)
    then begin
      Printf.eprintf "Output file already exists" ;
      exit 1
    end else begin
      let extension = extension fn in
      let wr = ImageUtil_unix.chunk_writer_of_path fn in
      ImageLib.writefile ~extension wr img
    end

  | _ -> invalid_arg "too many argv arguments"
