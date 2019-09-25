let perform () =
  let chunk_reader = ImageUtil_unix.chunk_reader_of_path Sys.argv.(1) in

  let extension filename =
    let ri =
      try 1 (* the . itself *) + String.rindex filename '.'
      with Not_found -> invalid_arg "filename without extension"
    in
    String.(sub filename ri @@ (length filename) - ri)
  in
  match ImageLib.openfile ~extension:(extension Sys.argv.(1)) chunk_reader with
  | _ -> ()
  | exception End_of_file -> ()
  | exception Image.Corrupted_image(_) -> ()

let () = AflPersistent.run perform
