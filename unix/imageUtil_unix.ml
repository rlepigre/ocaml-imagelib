open ImageUtil

(** [chop_extension' fname] is the same as [Filename.chop_extension fname] but
    if [fname] does not have an extension, [fname] is returned instead of
    raising [Invalid_argument]. *)
let chop_extension' fname =
  try Filename.chop_extension fname
  with _ -> fname


(** [get_extension fname] returns the extension of the file [fname]. If the
    file does not have an extension, [Invalid_argument] is raised. *)
let get_extension fname =
  let baselen = String.length (chop_extension' fname) in
  let extlen  = String.length fname - baselen - 1 in
  if extlen <= 0
  then let err = Printf.sprintf "No extension in filename '%s'." fname in
       raise (Invalid_argument err)
  else String.sub fname (baselen + 1) extlen


(** [get_extension' fname] is the same as [get_extension fname] but if [fname]
    does not have an extension, the empty string is returned and no exception
    is raised. *)
let get_extension' fname =
  try get_extension fname
  with _ -> ""


(*
 * Reads all the lines in the channel by calling input_line.
 * Returns a list of strings.
 *)
let lines_from_channel ich =
  let lines = ref [] in

  let rec intfun () =
    try
      let l = input_line ich in
      lines := l :: !lines;
      intfun ();
    with
    | End_of_file -> close_in ich
    | e -> close_in ich; raise e
  in
  intfun ();
  List.rev !lines

(*
 * Same as lines_from_channel but from a file.
 *)
let lines_from_file fn =
  let ich = open_in_bin fn in
  let ls = lines_from_channel ich in
  close_in ich; ls

let chunk_reader_of_in_channel ich : chunk_reader =
  function
  | `Bytes num_bytes ->
    begin try Ok (really_input_string ich num_bytes)
      with
      | End_of_file ->
        let offset = pos_in ich in
        close_in ich ;
        Error (`End_of_file offset )
      | e ->
        close_in ich ;
        raise e
    end
  | `Close -> close_in ich; Ok ""


let chunk_writer_of_out_channel och : chunk_writer =
  function
  | `String x ->
    ( try Ok (output_string och x) with
      | _ -> close_out och; Error `Write_error)
  | `Close ->
      close_out och; Ok ()

let chunk_reader_of_path fn =
  chunk_reader_of_in_channel (open_in_bin fn)


let chunk_writer_of_path fn =
  chunk_writer_of_out_channel (open_out_bin fn)

(** Define an output channel for the builtin buffered output on Uix.
    see {!ImageChannels} for more info*)

