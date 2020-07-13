let perform () =
  let chunk_reader = ImageUtil_unix.chunk_reader_of_path Sys.argv.(2) in

  let extension filename =
    let ri =
      try 1 (* the . itself *) + String.rindex filename '.'
      with Not_found -> invalid_arg "filename without extension"
    in
    String.(sub filename ri @@ (length filename) - ri)
  in
  let f ~extension chunk_reader = match Sys.argv.(1) with
    | "size" -> ignore @@ ImageLib.size ~extension chunk_reader
    | _ -> ignore @@ ImageLib.openfile ~extension chunk_reader in
  match f ~extension:(extension Sys.argv.(2)) chunk_reader with
  | _ -> ()
  | exception Out_of_memory -> ()
  | exception End_of_file -> ()
  | exception Image.Corrupted_image _ -> ()
  | exception Image.Not_yet_implemented _ -> ()

let () = AflPersistent.run perform
