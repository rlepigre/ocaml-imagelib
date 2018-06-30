(*
 * This file is part of Imagelib.
 *
 * Imagelib is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Imabelib is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Imabelib.  If not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright (C) 2014 Rodolphe Lepigre.
 *)
open Pervasives
open ImageSequence
open ImageUtil
open Image

module ReadJPG : ReadImage = struct
  let debug = ref true

  (* Source: ITU T.81 Annex K, table K.1 *)
  let default_luminance_quantize_table = [|
    16; 11; 10; 16;  24;  40;  51;  61;
    12; 12; 14; 19;  26;  58;  60;  55;
    14; 13; 16; 24;  40;  57;  69;  56;
    14; 17; 22; 29;  51;  87;  80;  62;
    18; 22; 37; 56;  68; 109; 103;  77;
    24; 35; 55; 64;  81; 104; 113;  92;
    49; 64; 78; 87; 103; 121; 120; 101;
    72; 92; 95; 98; 112; 100; 103;  99
  |]

  (* Source: ITU T.81 Annex K, table K.2 *)
  let default_chrominance_quantize_table = [|
    17; 18; 24; 47; 99; 99; 99; 99;
    18; 21; 26; 66; 99; 99; 99; 99;
    24; 26; 56; 99; 99; 99; 99; 99;
    47; 66; 99; 99; 99; 99; 99; 99;
    99; 99; 99; 99; 99; 99; 99; 99;
    99; 99; 99; 99; 99; 99; 99; 99;
    99; 99; 99; 99; 99; 99; 99; 99;
    99; 99; 99; 99; 99; 99; 99; 99
  |]

  (* The JPEG entropy encoder uses a zig-zag order. *)
  let make_zigzag () =
    let xy_to_index x y =
      x + 8*y
    in
    let rec loop acc = function
      (* (X, Y, dir (if any), moved) *)
      (* Reached the end -> return *)
      | (7, 7, _) ->
        acc
      (* Top/bottom -> go left, then southwest until we reach the edge *)
      | (_, 0, _) | (_, 7, _) as m ->
        let (x, y, _) = m in
        loop (xy_to_index x y :: xy_to_index (x + 1) y :: acc) (x, y - 1, -1)
      (* Left/right -> go down, then northeast until we reach the edge *)
      | (0, _, _) | (7, _, _) as m ->
        let (x, y, _) = m in
        loop (xy_to_index x y :: xy_to_index x (y - 1) :: acc) (x + 1, y, +1)
      (* Anything else -> keep going *)
      | (x, y, dir) ->
        loop (xy_to_index x y :: acc) (x + dir, y + dir, dir)
    in
    Array.of_list (loop [] (0, 0, 0))

  type component = {
    identifier : int;
    h_sample_factor : int;
    v_sample_factor : int;
    quant_index : int;
  }

  type sof = {
    precision : int;
    height : int;
    width : int;
    components : component array;
  }

  type dqt = {
    precision : int;
    index : int;
    entries : int array;
  }

  type component_spec = {
    component_index : int;
    dc_table_index : int;
    ac_table_index : int;
  }

  type sos = {
    component_specs : component_spec list;
    first_dct_component : int;
    last_dct_component : int;
    approx_high : int;
    approx_low : int;
  }

  let extensions = ["jpg"; "jpeg"; "jpe"; "jif"; "jfif"; "jfi"]

  let string_of_marker c =
    let mask_char c mask =
      string_of_int @@ int_of_char c land mask
    in
    match c with
    | '\x01' -> "TEM"
    | '\xC0' .. '\xC3' | '\xC5' .. '\xC7' | '\xC9' .. '\xCB' | '\xCD' .. '\xCF' as c -> "SOF" ^ mask_char c 0x0F
    | '\xC4' -> "DHT"
    | '\xCC' -> "DAC"
    | '\xD0' .. '\xD7' as c -> "RST" ^ mask_char c 0x07
    | '\xD8' -> "SOI"
    | '\xD9' -> "EOI"
    | '\xDA' -> "SOS"
    | '\xDB' -> "DQT"
    | '\xDC' -> "DNL"
    | '\xDD' -> "DRI"
    | '\xDE' -> "DHP"
    | '\xDF' -> "EXP"
    | '\xE0' .. '\xEF' as c -> "APP" ^ mask_char c 0x0F
    | '\xF0' .. '\xFD' as c -> "JPG" ^ mask_char c 0x0F
    | '\xFE' -> "COM"
    | c -> raise (Corrupted_image (Printf.sprintf "%C is not a valid marker" c))

  (* If a marker does not have a length field, it "stands alone". *)
  let marker_stands_alone = function
    | "TEM"
    | "RST0" | "RST1" | "RST2" | "RST3" | "RST4" | "RST5" | "RST6" | "RST7"
    | "SOI"
    | "EOI" -> true
    | _ -> false

  let rec find_next_marker ich handle_function =
    match get_bytes ich 1 with
    | "\x00" (* ignore JPEG escaped 0xFF byte *)
    | "\xFF" -> find_next_marker ich handle_function
    | c -> handle_function ich c.[0]

  let parse_sof ich =
    let rec parse_component ich acc = function
      | 0 -> acc
      | count ->
        let identifier = get_bytes ich 1 |> int_of_str1 in
        let dims = get_bytes ich 1 |> int_of_str1 in
        let h_sample_factor = (dims land 0xF0) lsr 4 in

        if h_sample_factor = 0 || h_sample_factor > 4 then
          raise (Corrupted_image "Horizontal sampling factor is out of bounds");

        let v_sample_factor = dims land 0x0F in

        if v_sample_factor = 0 || v_sample_factor > 4 then
          raise (Corrupted_image "Vertical sampling factor is out of range");

        let quant_index = get_bytes ich 1 |> int_of_str1 in

        if quant_index > 4 then
          raise (Corrupted_image "Quantisation index is out of range");

        if !debug then
          Printf.eprintf "Component: identifier %d, h factor %d, v factor %d, quantization table index %d\n%!" identifier h_sample_factor v_sample_factor quant_index;
        
        let component = {
          identifier;
          h_sample_factor;
          v_sample_factor;
          quant_index;
        } in
        parse_component ich (component :: acc) (count - 1)
    in
    let length = get_bytes ich 2 |> int_of_str2_be in
    let precision = get_bytes ich 1 |> int_of_str1 in

    if precision <> 8 then
      raise (Not_yet_implemented "This decoder only supports 8-bit frame precision");

    let height = get_bytes ich 2 |> int_of_str2_be in
    let width = get_bytes ich 2 |> int_of_str2_be in

    if width = 0 then
      raise (Corrupted_image "Frame must be at least 1 pixel wide");

    let comp_count = get_bytes ich 1 |> int_of_str1 in

    if comp_count <> 1 || comp_count <> 3 then
      raise (Not_yet_implemented "This decoder only supports 1 or 3 component colour");

    if length <> (8 + 3*comp_count) then
      raise (Corrupted_image "Frame length does not match component count");

    if !debug then
      Printf.eprintf "SOF: length %d, precision %d, height %d, width %d, component count %d\n%!" length precision height width comp_count;
    
    let components = Array.of_list (parse_component ich [] comp_count) in
    { precision; height; width; components }

  let parse_sos ich =
    let rec parse_component_spec ich acc = function
      | 0 -> acc
      | count ->
        let component_index = get_bytes ich 1 |> int_of_str1 in
        let table_indexes = get_bytes ich 1 |> int_of_str1 in
        let dc_table_index = (table_indexes land 0xF0) lsr 4 in

        if dc_table_index > 1 then
          raise (Not_yet_implemented "Baseline decoding requires only two DC tables");

        let ac_table_index = table_indexes land 0x0F in

        if ac_table_index > 1 then
          raise (Not_yet_implemented "Baseline decoding requires only two AC tables");

        if !debug then
          Printf.eprintf "Component Spec: component %d, DC table %d, AC table %d\n%!" component_index dc_table_index ac_table_index;
        
        let component_spec = {
          component_index;
          dc_table_index;
          ac_table_index;
        } in
        parse_component_spec ich (component_spec :: acc) (count - 1)
    in
    let length = get_bytes ich 2 |> int_of_str2_be in
    let compspeccount = get_bytes ich 1 |> int_of_str1 in

    if length <> (6 + 2*compspeccount) then
      raise (Corrupted_image "Length does not match component specifier count");

    let component_specs = parse_component_spec ich [] compspeccount in
    let first_dct_component = get_bytes ich 1 |> int_of_str1 in

    if first_dct_component <> 0 then
      raise (Not_yet_implemented "Baseline decoding requires the first DCT component to be zero.");

    let last_dct_component = get_bytes ich 1 |> int_of_str1 in

    if last_dct_component <> 63 then
      raise (Not_yet_implemented "Baseline decoding requires the last DCT component to be 63");

    let approx = get_bytes ich 1 |> int_of_str1 in
    let approx_high = (approx land 0xF0) lsr 4 in

    if approx_high <> 0 then
      raise (Not_yet_implemented "Baseline decoding requires the previous point transform to be zero");

    let approx_low = approx land 0x0F in

    if approx_low <> 0 then
      raise (Not_yet_implemented "Baseline decoding requires the current point transform to be zero");

    if !debug then
      Printf.eprintf "SOS: length %d, component spec count: %d, first DCT component: %d, last DCT component: %d, approx high: %d, approx low: %d\n%!"
        length compspeccount first_dct_component last_dct_component approx_high approx_low;
    { component_specs; first_dct_component; last_dct_component; approx_high; approx_low }

  let parse_dqt ich =
    let rec get_qt_entries ich size acc = function
      | 0 -> acc
      | n ->
        let s = get_bytes ich size in

        if !debug then
          Printf.eprintf "DQT%d = %S\n" n s;

        if size = 2 then
          get_qt_entries ich size (int_of_str2_be s :: acc) (n - 1)
        else
          get_qt_entries ich size (int_of_str1 s :: acc) (n - 1)
    in
    let length = get_bytes ich 2 |> int_of_str2_be in
    (* Naming is difficult *)
    let tmp = get_bytes ich 1 |> int_of_str1 in
    let precision = (tmp land 0xF0) lsr 4 in
    let index = (tmp land 0x0F) in
    (* 
     * NOTE: the quantization elements are specified in zig-zag order.
     * If I screw up somewhere, maybe the table needs to be de-zig-zagged.
     *)
  let entries = Array.of_list (get_qt_entries ich (1 + precision) [] 64) in
    { precision; index; entries }

  (* Based on https://stackoverflow.com/a/48488655 *)
  let size ich =
    let rec handle_marker ich c =
      match string_of_marker c with
      | "EOI" -> raise Not_found
      | "SOF0" | "SOF1"  | "SOF2"  | "SOF3"  | "SOF5"  | "SOF6"  | "SOF7"
      | "SOF9" | "SOF10" | "SOF11" | "SOF13" | "SOF14" | "SOF15" ->
        let sof = parse_sof ich in
        (sof.width, sof.height)
      | marker ->
        (if not (marker_stands_alone marker) then
           let len = get_bytes ich 2 |> int_of_str2_be in
           let data = get_bytes ich (len - 2) in
           ignore data
        );
        find_next_marker ich handle_marker
    in
    try
      find_next_marker ich handle_marker
    with End_of_file ->
      raise (Corrupted_image "Reached end of file while looking for SOF marker")

  let parsefile fn =
    let make_dqt precision index entries =
      { precision; index; entries }
    in

    let read_chunks = ref [] in

    let sof = ref None in
    let dqts = [| make_dqt 8 0 default_luminance_quantize_table; make_dqt 8 1 default_chrominance_quantize_table |] in
    let sos = ref None in
    let image = ref None in

    let rec handle_marker ich c =
      let curr_ctype = string_of_marker c in
      if !debug then
        Printf.eprintf "Found marker %s\n%!" curr_ctype; (
        match curr_ctype with
        (* Required markers *)
        | "SOI" ->
          only_once ich read_chunks curr_ctype;
          only_before ich read_chunks "SOS" curr_ctype;
          is_first_chunk ich read_chunks curr_ctype;
        | "EOI" ->
          only_once ich read_chunks curr_ctype;
          only_after ich read_chunks "SOI" curr_ctype;
          only_after ich read_chunks "SOS" curr_ctype;
          is_not_first_chunk ich read_chunks curr_ctype;

          raise Exit
        | "SOF0" | "SOF1"  | "SOF2"  | "SOF3"  | "SOF5"  | "SOF6"  | "SOF7"
        | "SOF9" | "SOF10" | "SOF11" | "SOF13" | "SOF14" | "SOF15" as s ->
          only_once ich read_chunks curr_ctype;
          only_after ich read_chunks "SOI" curr_ctype;
          only_before ich read_chunks "SOS" curr_ctype;

          (* Frames other than SOF0 have patent issues, so are (currently) unimplemented *)
          if s = "SOF0" then (
            let sof0 = parse_sof ich in
            sof := Some sof0;
            if !debug then
              Printf.eprintf "Number of components: %d\n%!" (Array.length sof0.components);
            (*
             * JPEG requires being able to decode 1-4 components, but JFIF only requires
             * 1 and 3 components.
             *)
            match Array.length sof0.components with
              | 1 (* Y, AKA Greyscale *) ->
                image := Some (create_grey ~max_val:(ones sof0.precision) sof0.width sof0.height)
              | 3 (* YCbCr, which will be converted to RGB *) ->
                image := Some (create_rgb ~max_val:(ones sof0.precision) sof0.width sof0.height)
              | _ (* Handled in parse_sof *) -> 
                assert false
            )
        | "SOS" ->
          only_after ich read_chunks "SOI" curr_ctype;

          sos := Some (parse_sos ich)
        | "DQT" ->
          only_after ich read_chunks "SOI" curr_ctype;

          let dqt = parse_dqt ich in
          dqts.(dqt.index) <- dqt;
        | marker ->
          if !debug then
            Printf.eprintf "Marker %s is TODO\n%!" marker;

          if not (marker_stands_alone marker) then
            let length = get_bytes ich 2 |> int_of_str2_be in
            let data = get_bytes ich (length - 2) in
            if !debug then
              Printf.eprintf "(ignoring %d bytes)\n%!" length;
            ignore data
      );
      read_chunks := curr_ctype :: !read_chunks;
      find_next_marker ich handle_marker
    in
    try
      find_next_marker fn handle_marker
    with Exit | End_of_file ->
    match !image with
    | Some image -> image
    | None -> raise (Corrupted_image "End-of-file reached before image data")
end
