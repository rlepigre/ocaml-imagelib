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
 * Copyright (C) 2014-2019 Rodolphe Lepigre.
 *)
open Stdlib


(* https://create.stephan-brumme.com/flexigif-lossless-gif-lzw-optimization/
https://stackoverflow.com/questions/1196322/how-to-create-an-animated-gif-in-net
https://www.fileformat.info/format/gif/sample/index.htm
https://www.fileformat.info/format/gif/egff.htm
*)

open ImageUtil
open Image


type gif_header_data = {
  version            : string ;
  image_size         : int * int ;
  global_color_table : bool ;
  color_resolution   : int ;
  sort               : bool ;
  size_glob_col_tbl  : int ;
  bg_color_index     : int ;
  pix_aspect_ratio   : int
}

type decoder_state =
  {
    carry_bits : int ; (* count bits in the current symbol *)
    consumed_symbols_of_current_code : int ;

    consumed_symbols : int ;
    (* total number of consumed symbols since last CLEAR *)

    current_partial_symbol : int ; (* current symbol, or lower bits of it *)

    code_size : int ; (* current code size *)
    min_code_size : int ;
  }

let [@inline always] calc_clear_code lzw_min_size =
  1 lsl (lzw_min_size -1)

let bin_of_str ~clear_code (t:decoder_state) str =
  (* TODO unroll this loop to deal with whole bytes at a time instead
     of looping over each bit *)
  let code_size = ref t.code_size in
  let eoi_code = clear_code + 1 in
  let len = String.length str in
  let i = ref 0 in (* byte offset into current subblock *)
  let acc = ref [] in (* symbol list emitted for this subblock *)
  let ctr = ref t.carry_bits in
  let codectr = ref t.consumed_symbols_of_current_code in
  let symctr = ref t.consumed_symbols in
  let carry = ref t.current_partial_symbol in
  while !i <> len do
    let bit_offset = ref 0 in (* bit offset into current byte *)
    while !bit_offset <> 8 do
      let this_bit = (Char.code str.[!i] lsr !bit_offset) land 1 in
      carry := !carry lor (this_bit lsl !ctr) ;
      incr ctr ; (* emitted a bit *)
      if !ctr = !code_size then begin
        (* we have a full value *)
        if !carry = eoi_code then begin (* escape loops *)
          i := len -1 ;
          bit_offset := 8 -1 ;
        end ;
        acc := !carry::!acc ;
        incr symctr ; incr codectr; ctr := 0 ;
        if !carry = clear_code then begin
          code_size := t.min_code_size ;
          symctr := 0;
          codectr := 1;
        end ;
        carry := 0
      end ;
      if !codectr = calc_clear_code !code_size then
        (if !code_size < 12 then (incr code_size) ; codectr := 0) ;
      incr bit_offset
    done ;
    incr i ;
  done ;
  let symbols = List.rev !acc in
  match !acc with
  | hd::_ when hd = eoi_code -> (* finished: *)
    `Complete symbols
  | _ ->
    let state =
      { carry_bits = !ctr ;
        consumed_symbols_of_current_code = !codectr ;
        consumed_symbols = !symctr ;
        current_partial_symbol = !carry;
        code_size = !code_size;
        min_code_size = t.min_code_size ;
      }
    in
    `Partial (state, symbols)


module ReadGIF : sig
  include ReadImage
  include ReadImageStreaming
end = struct
  let extensions = ["gif"]

  let [@inline always] uint16le ?(off=0) buf =
    ((int_of_char buf.[off+1]) lsl 8) lor (int_of_char buf.[off])

  (* Read signature and header *)
  let read_header (ich:ImageUtil.chunk_reader) =
    let magic = get_bytes ich 3 in
    if magic <> "GIF" then
      raise (Corrupted_image "GIF signature expected...");
    let version = get_bytes ich 3 in
    if version <> "87a" && version <> "89a" then
      raise (Corrupted_image "Version of GIF not supported...");
    let width  = get_bytes ich 2 |> uint16le in
    let height = get_bytes ich 2 |> uint16le in
    if width = 0 || height = 0 then
      raise (Corrupted_image "Invalid global GIF dimensions") ;
    let packed = chunk_byte ich in
    let global_color_table = (packed lsr 7 = 1) in
    let bgcol  = chunk_byte ich in
    if bgcol <> 0 && not global_color_table  then
      raise (Corrupted_image
               "global background color but no global color table");
    let pixar  = chunk_byte ich in
    let size_glob_col_tbl = packed land 0b111 in
    (* TODO fails on wat.gif
       if global_color_table && size_glob_col_tbl = 0 then
       raise (Corrupted_image "GIF Global Color Table empty ") ; *)
    {
      version            = version ;
      image_size         = width , height ;
      global_color_table ;
      color_resolution   = (packed lsr 4) mod 8 ;
      sort               = ((packed lsr 3) mod 2 = 1) ;
      size_glob_col_tbl ;
      bg_color_index     = bgcol ;
      pix_aspect_ratio   = pixar
    }

  (* Read the size of a GIF image.
   * Arguments:
   *   - ich : an input_chunker (see {TODO}).
   * Returns a tuple (width, height).
   * Note: the image payload is not checked for inconsistency,
   * only the signature and header are checked.
   *)
  let size ich =
    let hdr = read_header ich in
    ImageUtil.close_chunk_reader ich;
    hdr.image_size

  module Dict : sig
    type instance
    type key = int
    val clear : instance -> instance
    val empty : color_table_size:int -> lzw_min_size:int -> instance
    val next_code : instance -> key -> int -> instance
    val retrieve_first : instance -> int -> int option
    val retrieve_entry : instance -> int -> (int * int list) option
    val last_entry : instance -> int list
    (*val cardinal : instance -> int*)
    val redim : instance -> color_table_size:int -> clear_code:int -> instance
  end = struct
    type key = int
    type instance = {
      t : (int * int list) array;
      (* first element * complete list of elements.
         the list of elements is kept in reverse to speed up addition
      *)

      cardinal : int ; (* cache this since that operation is expensive *)

      color_table_size : int ;
      clear_code : int ;
      eoi_code : int
    }
    let clear_array () =
      (* technically we only need 2^code_size,
         max is 12 bits, so we just preallocate that: *)
      Array.init (4096) (fun i -> i, [])
    let clear t = { t with t = clear_array(); cardinal = 0; }

    let redim t ~color_table_size ~clear_code =
      {t with color_table_size ;
              clear_code ;
              eoi_code = clear_code + 1;
      }
    let empty ~color_table_size ~lzw_min_size =
      { t = clear_array() ;
        cardinal = 0;
        color_table_size ;
        clear_code = calc_clear_code lzw_min_size ;
        eoi_code = (calc_clear_code lzw_min_size) + 1 ;
      }

    (*let cardinal t = t.cardinal*)

    let retrieve_first dict symbol =
      match dict.t.( symbol ) with
      | _, [] -> None
      | first, _ -> Some first

    let retrieve_entry dict symbol =
      match dict.t.( symbol ) with
      | _, [] -> None
      | first, lst -> Some (first, List.rev lst)


    let last_entry dict =
      (* TODO throws exception if empty *)
      dict.t.(dict.eoi_code + dict.cardinal -1)  |> snd |> List.rev

    let next_code (t:instance) (key:key) (v:int) : instance =
      let entry = match t.t.(key) with
        | _, [] ->
          assert (key < t.color_table_size) ;
          assert (key <> t.clear_code);
          assert (key <> t.eoi_code);
          key, [v; key]
        | first, lst -> first, v::lst
      in
      let new_key = (t.eoi_code + t.cardinal) in
      if new_key = 4096 then begin
        (* we have reached the maximum number of entries in the table.
           at this point we stop accepting new codes until a CLEAR is issued
           (which will reset this table) *)
        t
      end else begin
        assert ([] = (snd t.t.(new_key)));
        t.t.(new_key) <- entry ;
        {t with cardinal = succ t.cardinal }
      end
  end

  type image_descriptor_state = {
    must_clear : bool; (* decoder MUST start with a CLEAR code *)
    dict : Dict.instance ; (* decompression dictionary *)
    x : int ; (* current width pixel *)
    y: int ;  (* current height pixel *)
    previous_symbol : int ; (* previous symbol processed by decoder *)
    clear_code : int ; (* (1 lsl (lzw_min_size-1)) *)
    eoi_code : int ; (* (1 lsl (lzw_min_size-1)) *)
    lzw_code_size : int ; (* current *)
    decoder : decoder_state ;
  }

  type read_state =
    { header : gif_header_data ;
      gct : string option ; (* global color table *)
      compression_dict : Dict.instance option ;
      buffer : image ; (* needs to be copied if image is returned *)
      (* ancillary state parsed from various extensions etc: *)
      transparency_index : int;
      display_time : int; (* hundreds of a second; 0 means "forever" *)
    }

  let process_image_descriptor_subblock ~transparency_index
      ~color_table ~color_table_size
      ~image ~lzw_min_size original_state subblock =
    let clear_code = calc_clear_code lzw_min_size in
    let eoi_code = clear_code + 1 in
    let used_coord = ref false in (* keep track of emitted pixels to make sure we don't overwrite, and that we emit a symbol for each pixel *)
    let [@inline always] next_coord ({x=old_x ; y = old_y; _ } as state) =
      assert (!used_coord);
      let x, y =
        if old_x + 1 = image.width
        then 0, old_y+1
        else old_x+1, old_y
      in
      used_coord := false;
      let state =
        if x >= image.width then
          raise (Corrupted_image "blew x dimension");
        if y > image.height then
          raise (Corrupted_image "blew y dimension");
        if y = image.height && x > 0 then
          raise (Corrupted_image "blew past end position");
        {state with x; y }
      in
      if state.x >= image.width || state.y >= image.height
         || state.x < 0 || state.y < 0 then
        used_coord := true; (* don't write out of bounds *)
      state
    in
    let [@inline always] emit_pixel image x y symbol =
      assert (symbol <> clear_code);
      assert (symbol <> eoi_code);
      if (!used_coord) then begin
        invalid_arg @@ Printf.sprintf "EMIT  x:%d/%d y:%d/%d sym:%d min:%d\n"
          x image.width y  image.height symbol lzw_min_size
      end;
      assert (not !used_coord);
      used_coord := true;
      let offset = symbol * 3 in
      write_rgba image x y
        (Char.code color_table.[offset  ])
        (Char.code color_table.[offset +1])
        (Char.code color_table.[offset +2])
        (if symbol = transparency_index then 0 else 0xff)
    in
    let [@inline always] process_symbols process_state symbols =
      let [@inline always] rec loop state = function
        | [] -> state
        | hd::tl ->
          begin match hd with
          | (symbol:int) when symbol = clear_code ->
            {state with previous_symbol = symbol ;
                        must_clear = false ;
                        dict = Dict.clear state.dict}
          | symbol when state.must_clear ->
            raise (Corrupted_image (Printf.sprintf
                                      "Doesn't start with clear code (%#x): %#x (dict:%d)"
                                      clear_code symbol
                                      (Dict.last_entry state.dict |> List.length)))
          | symbol when symbol = eoi_code ->
            {state with previous_symbol = symbol}
          | symbol when state.previous_symbol = clear_code ->
            if (symbol >= color_table_size) then
              raise (Corrupted_image (
                  Printf.sprintf "GIF: %s: symbol:%d >= color_table_size:%d"
                    __LOC__ symbol color_table_size)) ;
            emit_pixel image state.x state.y symbol ;
            let state = next_coord state in
            {state with
             previous_symbol = symbol ;
             dict = Dict.next_code Dict.(clear state.dict) 0 symbol}
          | symbol when symbol <= color_table_size ->
            (* symbol is not compressed *)
            emit_pixel image state.x state.y symbol ;
            let state = next_coord state in
            {state with
             previous_symbol = symbol ;
             dict = Dict.next_code state.dict state.previous_symbol symbol}

          | coded_symbol -> (* need to consult our dict *)
            begin match Dict.retrieve_entry state.dict coded_symbol with
              | Some (first, lst) ->
                let state = List.fold_left (fun state symbol ->
                    emit_pixel image state.x state.y symbol ;
                    next_coord state
                  ) (state) lst in
                {state with
                 previous_symbol = coded_symbol ;
                 dict = Dict.next_code state.dict
                     state.previous_symbol first}
              | None ->
                let first =
                  match Dict.retrieve_first state.dict
                          state.previous_symbol with
                  | Some first -> first
                  | None ->
                    assert (state.previous_symbol <= color_table_size);
                    state.previous_symbol
                in
                let next_dict = Dict.next_code state.dict
                    state.previous_symbol first in
                let state = {state with dict = next_dict} in
                let state = List.fold_left (fun state symbol ->
                    emit_pixel image state.x state.y symbol ;
                    next_coord state
                  ) state (Dict.last_entry next_dict) in
                {state with previous_symbol = coded_symbol }
            end
          end |> fun state ->
          loop state tl
      in loop process_state symbols
    in
    match bin_of_str ~clear_code:original_state.clear_code
            original_state.decoder subblock with
    | `Complete symbols ->
      begin match process_symbols original_state symbols with
        | state when state.previous_symbol = eoi_code ->
          if state.x = 0 && state.y = image.height
          then (`Complete state.dict)
          else raise (Corrupted_image
                        "GIF decoder produced image of unexpected dimensions")
        | _ -> raise (Corrupted_image "no EOI at end")
      end
    | `Partial (decoder, symbols) ->
      let state = {original_state with decoder} in
      begin match process_symbols state symbols with
        | state when state.previous_symbol = eoi_code ->
          raise (Corrupted_image "premature EOI in partial")
        | state -> `Partial state
      end

  let process_image_descriptor_block (state:read_state) ich =
    let original_compression_dict = state.compression_dict in
    let block  = get_bytes ich (4+4+1+1) in (* four 16-bit coordinates *)
    let left   = uint16le ~off:0 block in
    let top    = uint16le ~off:2 block in     (* 4 : coordinates *)
    let width  = uint16le ~off:4 block in
    let height = uint16le ~off:6 block in     (* 4 : dimensions *)

    let flags = Char.code block.[8] in        (* 1 : feature flags *)
    let local_color_table_size = 2 lsl (flags land 0b111) in
    let lzw_min_size = Char.code block.[9] in (* 1: minimum/root code size  *)

    let lzw_min_size = lzw_min_size + 1 in
    (*Printf.printf "TODO lzw_min_size %d\n%!" lzw_min_size;*)


    if let global_width, global_height = state.header.image_size in
      width = 0 || height = 0
      || (left + width > global_width)
      || (top + height > global_height) then
      raise (Corrupted_image "Invalid image descriptor block dimensions");

    if lzw_min_size < 3 || lzw_min_size > 12 then
      raise (Corrupted_image (Printf.sprintf "Invalid LZW minimum code size %d"
                                lzw_min_size)) ;

    (* feature flags:
       has_local_color_table  = flags & 0x80
       has_interlace          = flags & 0x40
(* TODO on interlace:
https://www.commandlinefanatic.com/cgi-bin/showarticle.cgi?article=art011*)
       has_sorted_color_table = flags & 0x20
       color_table_size       = 2 << (flags & 0b111)
    *)
    let implemented_flags =
      0x80 (* local color table *)
      lor 0b111 (* color table size *)
    in
    if (0 <> flags land 0x40) then
      raise (Not_yet_implemented "GIF interlace feature flag") ;

    if 0 <> flags land (lnot implemented_flags) then
      raise (Not_yet_implemented
               "Unsupported ImageDescriptor feature flag(s)") ;

    if not state.header.global_color_table && (0 = flags land 0x80) then
      raise (Corrupted_image "No color table available for GIF fragment") ;

    let color_table, color_table_size =
      if 0 <> flags land 0x80
      then get_bytes ich (3 * local_color_table_size),
           local_color_table_size
      else match state.gct with
        | None -> raise (Corrupted_image
                           "GIF: No global color table, and no local either")
        | Some gct -> gct, 2 lsl state.header.size_glob_col_tbl
    in

    let image = create_rgb ~alpha:true ~max_val:255 width height in
    let [@inline always] rec process_subblock acc =
      match chunk_byte ich, acc with
      | 0, `Initial ->
        raise (Corrupted_image
                 "GIF image descriptor block contained single empty subblock")
      | 0, `Partial (_:image_descriptor_state) ->
        raise
          (Corrupted_image "GIF image descriptor block not enough subblocks")
      | 0, `Complete dict -> dict
      | ch, `Complete _dict ->
        raise (Corrupted_image
                 (Printf.sprintf "complete but not really %#x" ch))
      | encoded_size, (`Initial | `Partial _ as descriptor_state) ->
        let descriptor_state =
          match descriptor_state with
          | `Partial d_state -> d_state
          | `Initial ->
            let clear_code = calc_clear_code lzw_min_size in
            { previous_symbol = clear_code ;
              clear_code ;
              eoi_code = clear_code +1 ;
              lzw_code_size = lzw_min_size ;
              must_clear = true ;
              dict = (match original_compression_dict with
                | None -> Dict.empty ~color_table_size ~lzw_min_size
                | Some dict -> Dict.redim dict ~color_table_size ~clear_code) ;
              x = 0; y = 0;
              decoder = {
                carry_bits = 0 ;
                consumed_symbols_of_current_code = 0;
                consumed_symbols = 0;
                current_partial_symbol = 0;
                code_size = lzw_min_size ;
                min_code_size = lzw_min_size ;
              }
            } in
        let subblock = get_bytes ich encoded_size in
        let descriptor_state =
          process_image_descriptor_subblock
            ~transparency_index:state.transparency_index
            ~color_table ~color_table_size ~image ~lzw_min_size
            descriptor_state subblock in
        process_subblock descriptor_state
    in
    let dict = process_subblock `Initial in
    dict, (image, left, top)

  let fill_background_color image color_table bg_color_index =
    let c color_channel = int_of_char
        (color_table.[bg_color_index + color_channel]) in
    Image.fill_rgb image (c 0) (c 1) (c 2)

  let read_streaming ich (gif_state:read_state option)
    : image option * int * read_state option =
    let gif_state : read_state =
      begin match gif_state with
        | None ->
          let header = read_header ich in
          let buffer =
            let w,h = header.image_size in
            create_rgb ~alpha:true ~max_val:255 w h in
          let gct_size =
            (*Size of Global Color Table - If the Global Color Table Flag is
               set to 1, the value in this field is used to calculate the number
               of bytes contained in the Global Color Table. To determine that
               actual size of the color table, raise 2 to
              [the value of the field + 1].*)
            1 lsl (succ header.size_glob_col_tbl) in
          let gct_bytesize = gct_size * 3 in
          let gct =
            match header.global_color_table with
            | false -> None
            | true ->
              (*Global Color Table Flag - Flag indicating the presence of a
                  Global Color Table; if the flag is set, the
                Global Color Table will immediately follow the
                Logical Screen Descriptor.*)
              let gct = get_bytes ich gct_bytesize in
              if header.bg_color_index * 3 > String.length gct then
                raise (Image.Corrupted_image
                         "GIF: bg_color_index larger than GlobalColorTable");

              (* initialize with bgcolor if there's a global color table: *)
              fill_background_color buffer gct header.bg_color_index ;
              (*fill_alpha buffer 0xff;*)

              Some gct
          in
          { header; gct; compression_dict = None ;
            (* initialize ancillary state: *)
            transparency_index = -1 ;
            display_time = 0;
            buffer ;
          }
        | Some state -> state
      end in
    Printexc.record_backtrace true; (* TODO *)
    let [@inline always] rec parse_blocks gif_state
      : image option * int * read_state option =
      (* with the exception of LogicalScreenDescriptor and GlobalColorTable,
         all all other Control blocks have a liited scope restricted to the Grapic-Rendering block that follows them. TODO.
         3b..3b -> trailer
         00..7f -> Graphic Rendering block
         80..f9 -> Control Blocks
         fa..ff -> Special Purpose blocks
      *)
      match (get_bytes ich 1).[0] with
      | '\x3b' -> None, gif_state.display_time, None (* End of file *)

      | '\x21' -> (* Extension Introducer *)
        let extension_label = chunk_byte ich in
        begin match extension_label with
          | 0xf9 -> (* Graphic Control Extension *)
            let len = chunk_byte ich in
            (* TODO maybe just assert (len = 4) *)
            assert (len = 4);
            let body = get_bytes ich (len+1) in
            if body.[len] <> '\x00' then
              raise (Corrupted_image "GCE: missing end of block") ;
            let packed = int_of_char body.[0] in
            let display_time =
              (* delaytime <- body.(1..2)
                 100ths of seconds before displaying next frame *)
              uint16le ~off:1 body in
            let use_transparent_color = (packed land 1) = 1 in
            let transparency_index =
              (* transparent color index <- body.(3)
                 index into global color table.
                 TODO should this default to 0? *)
              match use_transparent_color with
              | true -> int_of_char body.[3]
              (* TODO maybe verify that the index fits the GCT? *)
              | false -> -1
            in
            let user_input_flag = (packed lsr 1) land 1 in
            (* The Reserved subfield is not used in GIF89a and
               is always set to 0: *)
            begin if 0 <> packed lsr 5 then
              raise (Corrupted_image "GIF: Reserved GIF89a bit set.")
            end ;
            (* TODO maybe assert that GDM must be 0 if there's no transparency at play? *)
            (* http://webreference.com/content/studio/disposal.html *)
            let graphics_disposal_method = match (packed lsr 2) land 0b111 with
              | 0 ->
                ()
                (*Printf.eprintf "graphic disposal method not specified\n%!"*)
              (* Use this option to replace one full-size, non-transparent frame with another. *)
              (* TODO *)

              | 1 -> (* do not dispose of graphic*)
                (*Printf.eprintf "DO NOT DISPOSE\n%!";*)
                fill_alpha gif_state.buffer 0xff;
              (*  In this option, any pixels not covered up by the next frame continue to display. This is the setting used most often for optimized animations. In the flashing light animation, we wanted to keep the first frame displaying, so the subsequent optimized frames would just replace the part that we wanted to change. That's what Do Not Dispose does. *)
                ()

              | 2 -> Printf.printf "overwrite graphic with background color\n%!"
              (* The background color or background tile - rather than a previous frame - shows through transparent pixels. In the GIF specification, you can set a background color. In Netscape, it's the page's background color or background GIF that shows through. *)
                ;
                (match gif_state.gct with
                 | None -> (* TODO default to black or fail? *)
                   fill_background_color gif_state.buffer
                     "\000\000\000" 0
                 | Some gct ->
                   fill_background_color gif_state.buffer
                     gct gif_state.header.bg_color_index) ;

              | 4 -> (* overwrite graphic with previous graphic*)
                Image.fill_alpha gif_state.buffer 0xFF
              (* TODO unclear if this means that alpha should show *)
              (* The background color or background tile - rather than a previous frame - shows through transparent pixels. In the GIF specification, you can set a background color. In Netscape, it's the page's background color or background GIF that shows through.
The thing to remember about Restore to Previous is that it's not necessarily the first frame of the animation that will be restored but the last frame set to Unspecified or Do Not Dispose*)

              | _ -> raise @@ Corrupted_image
                  ("GIF: Graphics Disposal Method multiple bits set")
            in
            let _TODO = user_input_flag, graphics_disposal_method in
            let gif_state = {gif_state with transparency_index ;
                                            display_time} in
            parse_blocks gif_state (* <-- next block *)

          (* | 0xfe -> (* comment *) *)

          | 0xff -> (* ApplicationExtension *)
            if chunk_byte ich <> 0x0b then (* 0x0b = 8+3 *)
              raise (Corrupted_image "GIF ApplicationExtension length not 0x0B");
            let identifier = get_bytes ich 8 in
            (* TODO technically identifier must be printable ASCII too*)
            let authentcode = get_bytes ich 3 in
            let subblock_len = chunk_byte ich in
            (* https://github.com/ArtifexSoftware/mupdf/blob/master/source/fitz/load-gif.c *)
            begin match identifier, authentcode, subblock_len with
              | "NETSCAPE", "2.0", 3
              | "ANIMEXTS", "1.0", 3 ->
                (* Netscape animated GIF looping extension
                   http://www.vurdalakov.net/misc/gif/netscape-looping-application-extension
                   http://www.vurdalakov.net/misc/gif/animexts-looping-application-extension
                *)
                let subblock = get_bytes ich subblock_len in
                if subblock.[0] <> '\x01' then
                  raise @@ Corrupted_image
                    (Printf.sprintf
                       "GIF NETSCAPE 2.0 block unknown sub-block ID") ;
                let _loop_count = uint16le ~off:1 subblock in
                (* TODO 0x00 (0) means infinite loop. *)
                (*Printf.printf "TODO NETSCAPE EXTENSION loop count:%d\n%!" loop_count ;*)
                let terminator = chunk_byte ich in
                if terminator <> 0 then
                  raise @@ Corrupted_image
                    (Printf.sprintf
                       "GIF ApplicationExtension subblock terminator \
                        expected 0x00, got %#x" terminator)

              (* section B.6: http://www.color.org/icc1V42.pdf *)
              | "ICCRGBG1", "012", n
              | "ImageMag", "ick", n
              | "MGK8BIM0", "000", n
              | "MGKIPTC0", "000", n
                (* https://github.com/ImageMagick/ImageMagick/blob/master/coders/gif.c#L1164 *)
                (* we ignore this, seems like it usually contains something
                   like "gamma=0.5" *)

              (* Zoner GIF animator 4.0 and 5.0 *)
              | ( "ZGATEXTI" | "ZGATILEI"
                | "ZGACTRLI" | "ZGAVECTI"), "\x35\x00\x00", n
              | ( "ZGANPIMG" | "ZGAALPHA"), "I5\x00", n
              | ( "ZGATITLE" | "ZGATEXTI"), "4.0", n

              (* http://fractint.net/fractsvn/trunk/fractint/common/loadfile.c *)
              | "fract", "int", n

              (* http://wwwimages.adobe.com/www.adobe.com/content/dam/Adobe/en/devnet/xmp/pdfs/XMPSpecificationPart3.pdf *)
              | "XMP Data", "XMP", n ->
                let rec skip = function
                  (* End subblock parsing once we reach the
                     Block Terminator a.k.a. `0`: *)
                  | 0 -> ()
                  | n ->  let _ = get_bytes ich (n) in
                          skip (chunk_byte ich)
                in skip n

              | _ ->
                raise @@ Corrupted_image
                  (Printf.sprintf
                     "Unknown GIF ApplicationExtension %S:%S [len %d]"
                     identifier authentcode subblock_len)
            end ;
            parse_blocks gif_state
          | unknown -> raise @@ Not_yet_implemented
              (Printf.sprintf "Unknown GIF Extension %#x" unknown)
        end

      | '\x2c' -> (* Image Descriptor*)
        let dict, (image,left,top) = process_image_descriptor_block gif_state ich in
        (* TODO here we need to read a bit more and determine if we need to copy the buffer or if the next byte is the end marker *)
        let gif_state = {gif_state with
                         compression_dict = Some dict ;
                         buffer = Image.copy gif_state.buffer} in
        let buffer = gif_state.buffer in
        assert (image.height <= buffer.height) ;
        assert (image.width <= buffer.width) ;
        assert (left <= buffer.width) ;
        assert (top <= buffer.height) ;
        for x = 0 to image.width -1 do
          if x+left < 0 || x+left >= buffer.width then () else
          for y = 0 to image.height -1 do
            if y+top < 0 || y+top >= buffer.height then () else
            Image.read_rgba image x y
              (fun r g b -> function
                 (* optimization: if the dot is invisible, only copy over the alpha value: y*)
                 | 0x00 -> begin match gif_state.buffer.pixels with
                     | RGBA (_,_,_,aa) ->
                       Pixmap.set aa (left+x) (top+y) 0x00
                     | _ -> failwith "gif_state.buffer.pixels is not rgba"
                   end
                 | a ->
                   Image.write_rgba gif_state.buffer (left+x) (top+y) r g b a)
          done
        done ;
        Some gif_state.buffer, gif_state.display_time, Some gif_state

      | c -> raise (Not_yet_implemented
                      (Printf.sprintf "unknown block type %C" c))
    in
    parse_blocks gif_state

  let parsefile cr = ImageUtil.parsefile_of_read_streaming ~read_streaming cr
end


(* this function emits an image encoded as GIF *)
let write (cw:chunk_writer) (original_image:image) =

  (* TODO
     http://webreference.com/dev/gifanim/index-2.html *)

  if original_image.height > 0xffff || original_image.width > 0xffff then
    raise @@ Invalid_argument ("Image dimensions too large for GIF") ;

  let module ColorTable : Hashtbl.S with type key = int =
    Hashtbl.Make(struct
      type t = int
      let [@inline] equal (a:int) (b:int) = a = b
      let [@inline] hash a = a
    end) in

  (* The purposed of this hashtable is to keep track of how many
     different colors we have seen so far, such that we may produce
     an adequately sized color table (incidentally we also track
     the number of times each of these colors occur as the element): *)
  let color_count = ColorTable.create 256 in

  let packed_plane =
    let [@inline always] pack_int r g b =
      b lor (g lsl 8) lor (r lsl 16) in
    let [@inline always] register_count packed =
      try ColorTable.replace color_count packed
            (succ @@ ColorTable.find color_count packed) with
      | Not_found -> ColorTable.add color_count packed 1 in
    (* normalize it to 24bit RGB *)
    let open Bigarray in
    match original_image.pixels with
    | Grey (Pix8 old_plane) ->
      (* weighted according to wavelengths:
         R: 30%   G: 59%   B:11% *)
      let open Bigarray in
      let width = Array2.dim1 old_plane in
      let height = Array2.dim2 old_plane in
      let target_plane = Array2.create Int C_layout width height in
      for x = 0 to width -1 do
        for y = 0 to height -1 do
          let v = Array2.get old_plane x y in
          (* instead of e.g. (v * 30)/ 100 we use the same fraction
             of 256 instead of 100 so we can divide by shifting 8 places *)
          let v_r = ((v*77) lsr 8)
          and v_g = ((v*151) lsr 8)
          and v_b = ((v*28) lsr 8) in
          let packed = pack_int v_r v_g v_b in
          register_count packed ;
          Array2.set target_plane x y packed
        done
      done ;
      target_plane
    | Grey (Pix16 old_plane) ->
      let width = Array2.dim1 old_plane in
      let height = Array2.dim2 old_plane in
      let target_plane = Array2.create Int C_layout width height in
      for x = 0 to width -1 do
        for y = 0 to height -1 do
          let v = Array2.get old_plane x y in
          let v_r = (v*77) lsr 16
          and v_g = (v*151) lsr 16
          and v_b = (v*28) lsr 16 in
          let packed = pack_int v_r v_g v_b in
          register_count packed ;
          Array2.set target_plane x y packed
        done
      done ;
      target_plane
    | RGB (Pix16 old_red, Pix16 old_green, Pix16 old_blue) ->
      let width = Array2.dim1 old_red in
      let height = Array2.dim2 old_red in
      (* TODO assert that the other arrays have same dims*)
      let target_plane = Array2.create Int C_layout width height in
      for x = 0 to Array2.dim1 old_red -1 do
        for y = 0 to Array2.dim2 old_red -1 do
          (* TODO not sure this is correct, but here we go:*)
          let v_r = (Array2.get old_red   x y) lsr 8 in
          let v_g = (Array2.get old_green x y) lsr 8 in
          let v_b = (Array2.get old_blue  x y) lsr 8 in
          let packed = pack_int v_r v_g v_b in
          register_count packed ;
          Array2.set target_plane x y packed
        done
      done ;
      target_plane
    | RGB (Pix8 red, Pix8 green, Pix8 blue) ->
      let width = Array2.dim1 red in
      let height = Array2.dim2 red in
      let target_plane = Array2.create Int C_layout width height in
      for x = 0 to width -1 do
        for y = 0 to height -1 do
          let packed = pack_int
            (Array2.get red   x y)
            (Array2.get green x y)
            (Array2.get blue  x y) in
          register_count packed ;
          Array2.set target_plane x y packed
        done
      done ;
      target_plane
    | RGB _ ->
      raise (Corrupted_image
               "Something is wrong, color planes of different resolution")
    | GreyA _
    | RGBA _ ->
      raise (Not_yet_implemented "GIF does not yet support transparency")
  in

  let color_table_shift =
    (* here we try to round the cardinal of color_count *UP* to
       the nearest power of two *)
    let rec loop bits = function
      | 0 -> max 1 bits
      | v -> loop (succ bits) (v lsr 1)
    in
    let bits = loop 0 (ColorTable.length color_count-1) in
    bits
  in let color_table_size = 1 lsl color_table_shift in

  assert (color_table_size >= ColorTable.length color_count);(*TODO*)

  let [@inline always] pack_uint16le v =
    let buf = Bytes.create 2 in
    Bytes.unsafe_set buf 0 @@ Char.chr (v land 0xff) ;
    Bytes.unsafe_set buf 1 @@ Char.chr ((v land 0xff00) lsr 8) ;
    Bytes.unsafe_to_string buf
  in

  let flags =
    0xf0 (*0x80 | (7 lsl 4): palette stuff *) (* global color table *)
    lor (0b111 land (color_table_shift-1))
    (* 2, 4, 8, 16, 32, 64, 256 *)
    (* 2 << (this value) = global color table size*)
  in

  let lzw_min_size = 1 + color_table_shift in

  chunk_write cw "GIF89a";
  chunk_write cw @@ pack_uint16le original_image.width ;
  chunk_write cw @@ pack_uint16le original_image.height ;
  chunk_write_char cw @@ Char.chr flags ; (* flags *)
  chunk_write_char cw '\x00' ; (* bgcol *)
  chunk_write_char cw '\x00' ; (* pixar *)

  (* now follows the global color table: *)

    (* color_table: maps from 0..256 to the rgb byte value
     color_table_inv: maps from packed rgb value int to 0..256 *)
  let color_table = ColorTable.create color_table_size in
  let color_table_inv = ColorTable.create color_table_size in
  let () =
    (* initialize to black:*)
    for i = 0 to color_table_size do
      ColorTable.replace color_table i "\x34\x33\000" (* just fill *)
    done ;
    let place = ref 0 in
    (* TODO ideally sort this first so the most used will have the
       lowest number? *)
    ColorTable.to_seq_keys color_count
    |> Seq.iter (fun key ->
        let [@inline always] pack_rgb24 v =
          let buf = Bytes.create 3 in
          Bytes.unsafe_set buf 0 @@ Char.chr ((v land 0xff0000) lsr 16) ;
          Bytes.unsafe_set buf 1 @@ Char.chr ((v land 0xff00) lsr 8) ;
          Bytes.unsafe_set buf 2 @@ Char.chr (v land 0xff) ;
          Bytes.unsafe_to_string buf in
        let packed = pack_rgb24 key in
        ColorTable.replace color_table !place packed ;
        ColorTable.replace color_table_inv key !place ;
        incr place
      ) ;
    (* emit global color table: *)
    for i = 0 to color_table_size -1 do
      let color = (ColorTable.find color_table i) in
      chunk_write cw color
    done ;
  in

  (* TODO if we support transparency, emit a Graphic Control Extension block *)

  (* Emit an Image Descriptor block: *)
  chunk_write_char cw '\x2c';
  chunk_write cw (pack_uint16le 0); (* left: offset*)
  chunk_write cw (pack_uint16le 0); (* top: offset *)
  chunk_write cw (pack_uint16le original_image.width);
  chunk_write cw (pack_uint16le original_image.height);
  chunk_write_char cw '\x00'; (* flags *)

  (*22. Table Based Image Data.*)
  chunk_write_char cw @@ Char.chr lzw_min_size;

  let pixels = original_image.height * original_image.width in
  assert (pixels > 0);
  let pixel = ref 0 in
  let output_buffer = Bytes.make 255 '\000'
  and output_offset = ref 0 in
  let [@inline always] reset_output_buffer () =
    output_offset := 0;
    (* TODO DEBUG Bytes.fill output_buffer 0 255 '\000' *)
  and [@inline always] flush_output_buffer () =
    let subblock = Bytes.sub_string output_buffer 0 (!output_offset) in
    chunk_write_char cw @@ Char.chr (String.length subblock);
    chunk_write cw subblock;
  and [@inline always] buffer_put char =
    Bytes.set output_buffer !output_offset char;
    incr output_offset
  in
  let x = ref 0 in
  let y = ref 0 in
  let code_size = ref (lzw_min_size+1) in
  let clear_code = calc_clear_code !code_size in
  let eoi_code = clear_code + 1 in
  let carry = ref clear_code in
  let carry_bits = ref !code_size in
  let symbol_ctr = ref 0 in
  let byte_ctr = ref 0 in
  while !pixel < pixels do

    let packed = Bigarray.Array2.get packed_plane !x !y in
    let idx = ColorTable.find color_table_inv packed in

    incr symbol_ctr;
    if !code_size <> 12 && !symbol_ctr = calc_clear_code !code_size then begin
      incr code_size ;
      symbol_ctr := 0;
    end ;
    carry := !carry lor (idx lsl !carry_bits) ;
    carry_bits := !carry_bits + !code_size ;

    if !pixel+1 = pixels then begin
      (* Prepare for writing the End-Of-Image (EOI) marker: *)
      carry := !carry lor (eoi_code lsl !carry_bits) ;
      carry_bits := !carry_bits + !code_size;
    end;
    while (!carry_bits >= 8) do
      buffer_put @@ Char.chr (!carry land 0xff)  ;
      incr byte_ctr;
      carry := !carry lsr 8;
      carry_bits := !carry_bits - 8;
    done ;
    if !byte_ctr > 255 - 100 && !carry_bits <= 0 then begin
      (* carrying across subblocks seems to work poorly, so we try to get a
         byte-aligned subblock *)
      flush_output_buffer () ;
      reset_output_buffer () ;
      byte_ctr := 0;
    end ;
    incr pixel ;
    if !x + 1 = original_image.width then begin
      x := 0 ;
      incr y ;
    end else incr x;
  done ;
  assert (!pixel = pixels);
  while !carry_bits > 0 do
    buffer_put @@ Char.chr (!carry land 0xff) ;
    carry := !carry lsr 8;
    carry_bits := !carry_bits - 8;
    incr byte_ctr;
  done ;
  assert (!carry = 0);
  assert (!carry_bits <= 0);

  if !byte_ctr <> 0 then begin
    flush_output_buffer ();
  end ;

  chunk_write_char cw '\x00'; (* END IMAGE DESCRIPTOR SUBBLOCKS *)

  (* END OF GIF: *)
  chunk_write_char cw '\x3b'

include ReadGIF
