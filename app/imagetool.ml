type config = {
  input_file : string;
  text_color: [`IRC | `VT100 | `HTML];
  output_file : string option;
}

let arg_parser array : config =
  let rec erase leftover config =
    let set_text_color text_color tl =
      erase leftover {config with text_color} tl in
    function
    | [] -> config, Array.of_list leftover
    | "--irc"::tl -> set_text_color `IRC tl
    | "--html"::tl -> set_text_color `HTML tl
    | "--vt100"::tl -> set_text_color `VT100 tl
    | x :: _ when String.index_opt x '-' = Some 0 ->
      invalid_arg (Printf.sprintf "Unknown switch: %S" x)
    | input_file::tl when config.input_file = "" ->
      erase leftover {config with input_file } tl
    | output_file::tl when config.output_file = None ->
      erase leftover {config with output_file = Some output_file} tl
    | unknown::_ ->
      invalid_arg (Printf.sprintf "Unknown argument %S" unknown)
  in
  match erase [] { text_color = `VT100 ;
                   input_file = "" ;
                   output_file = None ;
                 } (Array.to_list array |> List.tl)
  with
  | config, [| |] when config.input_file <> "" -> config
  | _, extraneous ->
    if extraneous <> [| |] then
      Printf.printf "Error: extraneous arguments: %s"
      @@ String.concat " " (List.map String.escaped
                              (Array.to_list extraneous));
    Printf.printf "usage: %s INPUT-FILE [OUTPUT-FILE]\n" Sys.executable_name;
    Printf.printf "Displays a picture in the terminal, or convert it ";
    Printf.printf "(if OUTPUT.FILE is specified)\n" ;
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
  | { text_color = `VT100 ; _} -> (* print to terminal: *)
    foreach_pixel ImageUtil.colorize_rgb888
  | { text_color = `IRC ; _ } ->
    foreach_pixel ImageUtil.colorize_rgb888_irc
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
