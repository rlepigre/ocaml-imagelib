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
  let img =
    ImageLib.openfile ~extension:(extension config.input_file)
      (ImageUtil_unix.chunk_reader_of_path config.input_file) in

  let foreach_pixel f =
    for y = 0 to img.height -1 do
      for x = 0 to img.width -1 do
        Image.read_rgba img x y (fun r g b a ->
            print_string (f r g b a))
      done; print_newline () ;
    done
  in
  match config with
  | { display_mode = Some `VT100 ; background ; _ } ->
    (* print to terminal: using 24-bit color escape codes  *)
    foreach_pixel (ImageUtil.colorize_rgba8888 ~background)

  | { display_mode = Some `IRC ; background ; _ } ->
    (* print to terminal, using IRC 24-bit color escape codes *)
    foreach_pixel (ImageUtil.colorize_rgba8888_irc ~background)

  | { output_file = Some fn ; _ } ->
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
