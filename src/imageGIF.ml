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
    consumed_symbols_of_current_code : int ; (**)

    consumed_symbols : int ;
    (* total number of consumed symbols since last CLEAR *)

    current_partial_symbol : int ; (* current symbol, or lower bits of it *)

    code_size : int ; (* current code size *)
    min_code_size : int ;
  }

let bin_of_str ~clear_code (t:decoder_state) str =
  (*let color_table = String.init (1 lsl (min_size-1)) (fun i -> Char.chr i) in
  let width = 10 and height = 10 in
  let red = Pixmap.create8 width height
  and green = Pixmap.create8 width height
    and blue = Pixmap.create8 width height in*)
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
        end;
        carry := 0 end ;
      if !codectr = 1 lsl (!code_size-1) then
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


module ReadGIF : ReadImage = struct
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
    let bgcol  = chunk_byte ich in
    let pixar  = chunk_byte ich in
    let global_color_table = (packed lsr 7 = 1) in
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
  end = struct
    include Map.Make(struct type t = int
        let compare = (compare : int -> int -> int) end)
    type instance = {
      t : (int * int list) t;
      (* first element * complete list of elements.
         the list of elements is kept in reverse to speed up addition
      *)

      cardinal : int ; (* cache this since that operation is expensive *)

      color_table_size : int ;
      clear_code : int ;
      eoi_code : int
    }
    let clear t = { t with t = empty; cardinal = 0; }
    let empty ~color_table_size ~lzw_min_size =
      { t = (empty:(int * int list) t) ;
        cardinal = 0;
        color_table_size ;
        clear_code = (1 lsl (lzw_min_size-1)) ;
        eoi_code = (1 lsl (lzw_min_size-1)) + 1 ;
      }

    let retrieve_first dict symbol =
      match find_opt symbol dict.t with
      | Some (first, _) -> Some first
      | None -> None

    let retrieve_entry dict symbol =
      match find_opt symbol dict.t with
      | Some (_, []) ->
        raise (Corrupted_image "referencing uninitialized code")
      | Some (first, lst) -> Some (first, List.rev lst)
      | None -> None

    let last_entry dict =
    (* TODO throws exception if empty *)
      max_binding dict.t |> snd |> snd |> List.rev

    let next_code (t:instance) (key:key) (v:int) : instance =
      let entry = match find_opt key t.t with
        | Some (first, lst) -> first, v::lst (* TODO yikes *)
        | None -> assert (key < t.color_table_size) ;
          assert (key <> t.clear_code);
          assert (key <> t.eoi_code);
          key, [v; key]
      in
      let new_key = (t.eoi_code + t.cardinal) in
      assert (not @@ mem new_key t.t);
      {t with t = add new_key entry t.t ; cardinal = succ t.cardinal }
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
  let process_image_descriptor_subblock
      ~color_table ~color_table_size
      ~image ~lzw_min_size original_state subblock =
    let clear_code = 1 lsl (lzw_min_size -1) in
    let eoi_code = 1 lsl (lzw_min_size -1) + 1 in
    let [@inline always] next_coord ({x ; y; _ } as state) =
      if x + 1 = image.width then begin
        if y >= image.height then
          raise (Corrupted_image "blew y dimension");
        {state with x = 0; y = y+1 }
      end else begin
        if x >= image.width then
          raise (Corrupted_image "blew x dimension");
        {state with x = x+1; y } end in
    let [@inline always] emit_pixel image x y symbol =
      assert (symbol <> clear_code);
      assert (symbol <> eoi_code);
      let offset = symbol * 3 in
      write_rgb image x y
        (Char.code color_table.[offset  ])
        (Char.code color_table.[offset +1])
        (Char.code color_table.[offset +2])
    in
    let [@inline always] process_symbols process_state symbols =
      List.fold_left (fun state ->
          function
          | symbol when symbol = clear_code ->
            {state with previous_symbol = symbol ;
                        must_clear = false ;
                        dict = Dict.clear state.dict}
          | _symbol when state.must_clear ->
            raise (Corrupted_image "doesn't start with clear code")
          | symbol when symbol = eoi_code ->
            {state with previous_symbol = symbol}
          | symbol when state.previous_symbol = clear_code ->
            assert (symbol < color_table_size) ;
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
        ) process_state symbols
    in
    match bin_of_str ~clear_code:original_state.clear_code
            original_state.decoder subblock with
    | `Complete symbols ->
      begin match process_symbols original_state symbols with
        | state when state.previous_symbol = eoi_code ->
          if state.x = 0 && state.y = image.height
          then (`Complete)
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

  let process_image_descriptor_block ich hdr gct =
    let block  = get_bytes ich (4+4+1+1) in (* four 16-bit coordinates *)
    let left   = uint16le ~off:0 block in
    let top    = uint16le ~off:2 block in     (* 4 : coordinates *)
    let width  = uint16le ~off:4 block in
    let height = uint16le ~off:6 block in     (* 4 : dimensions *)

    let flags = Char.code block.[8] in        (* 1 : feature flags *)
    let local_color_table_size = 2 lsl (flags land 0b111) in
    let lzw_min_size = Char.code block.[9] in (* 1: minimum code size  *)

    let lzw_min_size = lzw_min_size + 1 in


    if let global_width, global_height = hdr.image_size in
      width = 0 || height = 0
      || (left + width > global_width)
      || (top + height > global_height) then
      raise (Corrupted_image "Invalid image descriptor block dimensions");

    if lzw_min_size < 3 || lzw_min_size > 9 then
      raise (Corrupted_image "Invalid LZW minimum code size") ;

    (* feature flags:
       has_local_color_table  = flags & 0x80
       has_interlace          = flags & 0x40
       has_sorted_color_table = flags & 0x20
       color_table_size       = 2 << (flags & 0b111)
    *)
    let implemented_flags =
      0x80 (* local color table *)
      lor 0b111 (* color table size *)
    in
    assert (0 = flags land 0x40);

    if 0 <> flags land (lnot implemented_flags) then
      raise (Not_yet_implemented
               "Unsupported ImageDescriptor feature flag(s)") ;

    if not hdr.global_color_table && (0 = flags land 0x80) then
      raise (Corrupted_image "No color table available for GIF fragment") ;

    let color_table, color_table_size =
      if 0 <> flags land 0x80
      then get_bytes ich (3 * local_color_table_size),
           local_color_table_size
      else gct, 2 lsl hdr.size_glob_col_tbl
    in

    let image = create_rgb ~max_val:255 width height in
    let [@inline always] rec process_subblock acc =
      match chunk_byte ich, acc with
      | 0, `Initial ->
        raise (Corrupted_image
                 "GIF image descriptor block contained single empty subblock")
      | 0, `Partial _ ->
        raise
          (Corrupted_image "GIF image descriptor block not enough subblocks")
      | 0, `Complete -> ()
      | _, `Complete ->
        raise (Corrupted_image "complete but not really")
      | encoded_size, (`Initial | `Partial _ as state) ->
        let state =
          match state with
          | `Partial state -> state
          | `Initial ->
            let clear_code = (1 lsl (lzw_min_size-1)) in
            { previous_symbol = clear_code ;
              clear_code ;
              eoi_code = clear_code +1 ;
              lzw_code_size = lzw_min_size ;
              must_clear = true ;
              dict = Dict.empty ~color_table_size ~lzw_min_size ;
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
        let state =
          process_image_descriptor_subblock
            ~color_table ~color_table_size ~image ~lzw_min_size
            state subblock in
        process_subblock state
    in
    process_subblock `Initial ;
    image


  let parsefile ich =
    let hdr = read_header ich in
    let gct = ref "" in
    let gct_size =
      (*Size of Global Color Table - If the Global Color Table Flag is
         set to 1, the value in this field is used to calculate the number
         of bytes contained in the Global Color Table. To determine that
         actual size of the color table, raise 2 to [the value of the field
         + 1].*)
      1 lsl (succ hdr.size_glob_col_tbl) in
    let gct_bytesize = gct_size * 3 in
    let () = match hdr.global_color_table with
      | false -> ()
      | true ->
        (*Global Color Table Flag - Flag indicating the presence of a
            Global Color Table; if the flag is set, the Global Color Table will
            immediately follow the Logical Screen Descriptor.*)
        gct := get_bytes ich gct_bytesize ;
    in
    let [@inline always] rec parse_blocks acc =
      (* 20. Image Descriptor*)
      match (get_bytes ich 1).[0] with
      | '\x3b' -> List.rev acc (* End of file *)

      | '\x21' -> (* Extension*)
        begin match chunk_byte ich with
          | 0xf9 -> (* Graphic Control Extension *)
            let len = chunk_byte ich in
            let body = get_bytes ich (len+1) in
            (* TODO transparency and stuff*)
            if body.[len] <> '\x00' then
              raise (Corrupted_image "GCE: missing end of block") ;
            parse_blocks acc (* <-- next block *)

          (* | 0xfe -> (* comment *) *)
          (* | 0xff -> (* application *) *)
          | _ -> raise (Not_yet_implemented "Extension")
        end

      | '\x2c' -> (* Image Descriptor*)
        let image = process_image_descriptor_block ich hdr !gct in
        parse_blocks (image::acc)

      | _ -> invalid_arg "TODO unknown block type"
    in
    let image =
      let w,h = hdr.image_size in
      let _image = create_rgb w h in
      (* TODO blit partial images into main image and return that instead *)
      let tODO_partial_images = parse_blocks [] in
      match tODO_partial_images with
      | [image] -> image
      | [] ->
        raise (Corrupted_image "GIF did not contain an image descriptor block")
      | _ ->
        raise (Not_yet_implemented
                 "multiple image descriptor blocks in GIF file")
    in
    ImageUtil.close_chunk_reader ich ;
    image
end
