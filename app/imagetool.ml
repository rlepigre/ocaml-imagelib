let () =
  if Array.length Sys.argv = 1 then begin
    Printf.printf "usage: %s INPUT-FILE [OUTPUT-FILE]\n" Sys.executable_name;
    Printf.printf "Displays a picture in the terminal, or convert it ";
    Printf.printf "(if OUTPUT.FILE is specified)\n" ;
    exit 2
  end ;
  let extension filename =
    let ri =
      try 1 (* the . itself *) + String.rindex filename '.'
      with Not_found -> invalid_arg "filename without extension"
    in
    String.(sub filename ri @@ (length filename) - ri)
  in
  let filename_in = Sys.argv.(1) in
  let img =
    ImageLib.openfile ~extension:(extension filename_in)
      (ImageUtil_unix.chunk_reader_of_path filename_in) in

  match Array.length Sys.argv with
  | 2 -> (* print to terminal: *)
    for y = 0 to img.height -1 do
      for x = 0 to img.width -1 do
        Image.read_rgb img x y (fun r g b ->
            print_string (ImageUtil.colorize_rgb888 r g b))
      done; print_newline () ;
    done
  | 3 -> (* output to filename specified in second argument *)
    if Sys.(file_exists argv.(2))
    then begin
      Printf.eprintf "Output file already exists" ;
      exit 1
    end else begin
      let fn = Sys.argv.(2) in
      let extension = extension fn in
      let wr = ImageUtil_unix.chunk_writer_of_path fn in
      ImageLib.writefile ~extension wr img
    end
  | _ -> invalid_arg "too many argv arguments"
