open ImageUtil

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
