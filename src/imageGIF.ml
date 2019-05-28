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

let bin_of_str min_size str =
  (*let color_table = String.init (1 lsl (min_size-1)) (fun i -> Char.chr i) in
  let width = 10 and height = 10 in
  let red = Pixmap.create8 width height
  and green = Pixmap.create8 width height
    and blue = Pixmap.create8 width height in*)
  let code_size = ref min_size in
  let clear_code = (1 lsl (min_size-1)) in
  let eoi_code = clear_code + 1 in
  let len = String.length str in
  let ctr = ref 0 in (* count bits in current symbol *)
  let codectr = ref 0 in (* count symbols in current code len *)
  let symctr = ref 0 in (* count symbols *)
  let i = ref 0 in (* byte offset into current subblock *)
  let carry = ref 0 in (* current symbol, or lower bits of it *)
  let acc = ref [] in (* symbol list *)
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
          code_size := min_size ;
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
  match !acc with
  | hd::_ when hd = eoi_code -> (* finished: *)
    List.rev !acc
  | _ -> raise (Corrupted_image "premature end of LZW symbol stream")


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

        let this_color_table, this_color_table_size =
          if 0 <> flags land 0x80
          then get_bytes ich (3 * local_color_table_size),
               local_color_table_size
          else !gct, 2 lsl hdr.size_glob_col_tbl
        in

        let [@inline always] rec process_subblock acc =
          let encoded_size = chunk_byte ich in
          if encoded_size = 0
          then String.concat "" (List.rev acc)
          else process_subblock (get_bytes ich encoded_size::acc)
        in
        let subblocks = process_subblock [] in
        let symbols = bin_of_str lzw_min_size subblocks in
        let image = create_rgb ~max_val:255 width height in
        let clear_code = 1 lsl (lzw_min_size -1) in
        let eoi_code = 1 lsl (lzw_min_size -1) + 1 in
        let [@inline always] next_coord (x,y) =
          if x + 1 = width then 0, y+1 else x+1, y in
        let module Dict = struct include Map.Make(struct type t = int
            let compare = (compare : int -> int -> int) end)
          let next_code (t:int list t) (key:key) (v:int) : int list t =
            let entry = match find_opt key t with
              | Some lst -> lst @ [v]
              | None -> assert (key < this_color_table_size) ;
                assert (key <> clear_code);
                assert (key <> eoi_code);
                [key ; v]
            in
            let new_key = (eoi_code + cardinal t) in
            assert (not @@ mem new_key t);
            add new_key entry t
        end in
        let _, _, (final_x, final_y) =
          let [@inline always] emit_pixel image x y symbol =
            assert (symbol <> clear_code);
            assert (symbol <> eoi_code);
            let offset = symbol * 3 in
            write_rgb image x y
              (Char.code this_color_table.[offset  ])
              (Char.code this_color_table.[offset +1])
              (Char.code this_color_table.[offset +2])
          in
          let must_clear = ref true in
          List.fold_left (fun (previous, dict, (x,y)) -> function
              | symbol when symbol = clear_code ->
                must_clear := false;
                symbol, Dict.empty, (x, y)
              | _symbol when !must_clear ->
                raise (Corrupted_image "doesn't start with clear code")
              | symbol when symbol = eoi_code ->
                (symbol, dict, (x, y))
              | symbol when previous = clear_code ->
                assert (symbol < this_color_table_size) ;
                emit_pixel image x y symbol ;
                symbol, Dict.next_code Dict.empty 0 symbol, next_coord (x,y)
              | symbol when symbol <= this_color_table_size ->
                (* symbol is not compressed *)
                emit_pixel image x y symbol ;
                symbol, (Dict.next_code dict previous symbol), next_coord (x,y)

              | coded_symbol -> (* need to consult our dict *)
                begin match Dict.find_opt coded_symbol dict with
                  | Some (first::_ as lst) ->
                    let res = List.fold_left (fun (x,y) symbol ->
                        emit_pixel image x y symbol ;
                        next_coord (x,y)
                      ) (x,y) lst in
                    let xx = (Dict.next_code dict previous first) in
                    coded_symbol, xx, res
                  | Some [] ->
                    raise (Corrupted_image "referencing uninitialized code")
                  | None ->
                    let first =
                      match Dict.find_opt previous dict with
                      | Some [] ->
                        raise (Corrupted_image "retrieved uninitialized code")
                      | Some (first::_) -> first
                      | None ->
                        assert (previous <= this_color_table_size);
                        previous
                    in
                    let next_dict = Dict.next_code dict previous first in
                    let (x,y) = List.fold_left (fun (x,y) symbol ->
                        emit_pixel image x y symbol ;
                        next_coord (x,y)
                      ) (x,y) (Dict.max_binding next_dict |> snd) in
                    coded_symbol, next_dict, (x,y)
                end
            ) (clear_code, Dict.empty, (0, 0)) symbols in
        if final_x <> 0 || final_y <> height then
          raise (Corrupted_image
                   "GIF decoder produced image of unexpected dimensions");
        parse_blocks (image::acc)


      | _ -> invalid_arg "TODO unknown block type"
    in
    let image =
      let w,h = hdr.image_size in
      let _image = create_rgb w h in
      (* TODO blit partial images into main image and return that instead *)
      let tODO_partial_images = parse_blocks [] in
      List.hd tODO_partial_images
    in
    ImageUtil.close_chunk_reader ich ;
    image
end
