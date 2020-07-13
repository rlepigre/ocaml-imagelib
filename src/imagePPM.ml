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
open Stdlib
open ImageUtil
open Image

module ReadPPM : ReadImage = struct
  (*
   * The list of standard extenisons for the PPM format.
   *   - ppm : portable pixmap format
   *   - pgm : portable graymap format
   *   - pbm : portable bitmap format
   *   - pnm : portable anymap format
   *)
  let extensions = ["ppm"; "pgm"; "pbm"; "pnm"]

  let read_header (content:chunk_reader) =
    let magic = ref "" in
    let width = ref (-1) and height = ref (-1) in
    let max_val = ref 1 in
    let scanner = Scanf.Scanning.from_function (fun () -> chunk_char content) in
    let rec pass_comments () =
      try
        Scanf.bscanf scanner "#%[^\n\r]%[\t\n\r]" (fun _ _ -> () );
        pass_comments ()
      with _ -> ()
    in
    Scanf.bscanf scanner "%s%[\t\n ]" (fun mn _ -> magic := mn);
    pass_comments ();

    if not (List.mem !magic ["P1"; "P2"; "P3"; "P4"; "P5"; "P6"]) then
      raise (Corrupted_image "Invalid magic number...");

    if List.mem !magic ["P1"; "P4"] then begin
      Scanf.bscanf scanner "%u%[\t\n ]" (fun w _ -> width := w);
      pass_comments ();
      Scanf.bscanf scanner "%u%1[\t\n ]" (fun h _ -> height := h)
    end else begin
      begin try
        Scanf.bscanf scanner "%u%[\t\n ]" (fun w _ -> width := w);
      with Stdlib.Scanf.Scan_failure _ ->
        raise(Image.Corrupted_image "PPM: invalid width")
      end;
      pass_comments ();
      begin try
        Scanf.bscanf scanner "%u%[\t\n ]" (fun h _ -> height := h);
      with Stdlib.Scanf.Scan_failure _ ->
        raise(Image.Corrupted_image "PPM: invalid height")
      end;
      pass_comments ();
      begin try
        Scanf.bscanf scanner "%u%1[\t\n ]" (fun mv _ -> max_val := mv);
      with Stdlib.Scanf.Scan_failure _ ->
        raise(Image.Corrupted_image "PPM: invalid max_val")
      end
    end;
    !magic,!width,!height,!max_val,scanner

  (*
   * Reads the size of a PPM image from a file.
   * Does not check if the file is correct. Only goes as far as the header.
   * Returns a couple (width, height)
   *)
  let size ich =
    let _,w,h,_,_ = read_header ich in
    close_chunk_reader ich;
    w, h

  (*
   * Read a PPM format image file.
   * Arguments:
   *   - fn : the path to the file.
   * Raise the exception Corrupted_image if the file is not valid.
   *)
  let parsefile (ich:chunk_reader) =
    let prev_byte = ref None in
    try
      let magic,w,h,max_val,scanner =
        (function | `Bytes 1 -> let b = ich (`Bytes 1) in
                                prev_byte := Some b; b
                  | orig -> ich orig )
        |> read_header
      in
      let content : chunk_reader = function
        | `Bytes 1 -> begin match !prev_byte with | Some x -> prev_byte:=None; x
                                                  | None -> ich (`Bytes 1) end
        | orig -> ich orig
      in

      match magic with
      | "P1" | "P2" ->
       let image = create_grey ~max_val:max_val w h in
       for y = 0 to h - 1 do
         for x = 0 to w - 1 do
           begin try
             Scanf.bscanf scanner "%d%[\t\n ]" (fun v _ ->
               write_grey image x y v)
           with Stdlib.Scanf.Scan_failure _ ->
             raise(Image.Corrupted_image "PPM: Invalid grayscale pixel data")
           end
         done
       done;
       image

     | "P3" ->
       let image = create_rgb ~max_val:max_val w h in
       for y = 0 to h - 1 do
         for x = 0 to w - 1 do
           Scanf.bscanf scanner "%d%[\t\n ]%d%[\t\n ]%d%[\t\n ]"
             (fun r _ g _ b _ ->
               write_rgb image x y r g b)
         done
       done;
       image

     | "P4" ->
       let image = create_grey ~max_val:1 w h in
       for y = 0 to h - 1 do
         let x = ref 0 in
         let byte = ref 0 in
         while !x < w do
           if !x mod 8 = 0 then
             byte := chunk_byte content;
           let byte_pos = !x mod 8 in
           let v = (!byte lsr (7 - byte_pos)) land 1 in
           write_grey image !x y v;
           incr x
         done
       done;
       image

     | "P5" ->
       let image = create_grey ~max_val:max_val w h in
       for y = 0 to h - 1 do
         for x = 0 to w - 1 do
           if max_val <= 255 then (
             let b0 = chunk_byte content in
             write_grey image x y b0)
           else (
             let b0 = chunk_byte content in
             let b1 = chunk_byte content in
             write_grey image x y ((b0 lsl 8) + b1))
         done
       done;
       image

     | "P6" ->
       let image = create_rgb ~max_val:max_val w h in
       for y = 0 to h - 1 do
         for x = 0 to w - 1 do
           if max_val <= 255 then (
             let r = chunk_byte content in
             let g = chunk_byte content in
             let b = chunk_byte content in
             write_rgb image x y r g b)
           else (
             let r1 = chunk_byte content in
             let r0 = chunk_byte content in
             let g1 = chunk_byte content in
             let g0 = chunk_byte content in
             let b1 = chunk_byte content in
             let b0 = chunk_byte content in
             let r = (r1 lsl 8) + r0 in
             let g = (g1 lsl 8) + g0 in
             let b = (b1 lsl 8) + b0 in
             write_rgb image x y r g b)
         done
       done;
       image

     | _ ->
       raise (Corrupted_image "Invalid magic number...")
    with End_of_file ->
      raise (Corrupted_image "Truncated file")

end

(*
 * PPM encoding of files (Binary or ASCII)
 *)
type ppm_mode = Binary | ASCII

(*
 * Write a PPM format image to a file.
 * Arguments:
 *   - fn : the path to the file.
 *   - img : the image.
 *   - mode : the image mode (Binary or ASCII).
 * Warning: the alpha channel is ignored since it is not supported by the PPM
 * image format.
 *)
let write_ppm (och:ImageUtil.chunk_writer) img mode =
  let w = img.width and h = img.height in

  (match img.pixels, mode, img.max_val with
   | (RGB _ | RGBA _)  , Binary, mv ->
     chunk_printf och "P6\n%i %i %i\n" w h mv;
     for y = 0 to h - 1 do
       for x = 0 to w - 1 do
         read_rgb img x y (fun r g b ->
           if mv < 256
           then begin
             chunk_printf och "%c%c%c"
               (char_of_int r) (char_of_int g) (char_of_int b)
           end else begin
             let r0 = char_of_int (r mod 256) in
             let r1 = char_of_int (r lsr 8) in
             let g0 = char_of_int (g mod 256) in
             let g1 = char_of_int (g lsr 8) in
             let b0 = char_of_int (b mod 256) in
             let b1 = char_of_int (b lsr 8) in
             chunk_printf och "%c%c%c%c%c%c" r1 r0 g1 g0 b1 b0
           end)
       done
     done
   | (RGB _ | RGBA _)  , ASCII,  mv ->
     chunk_printf och "P3\n%i %i %i\n" w h mv;
     for y = 0 to h - 1 do
       for x = 0 to w - 1 do
          read_rgb img x y (fun r g b ->
         chunk_printf och "%i %i %i\n" r g b)
       done;
     done
   | (Grey _ | GreyA _), Binary, 1  ->
     chunk_printf och "P4\n%i %i\n" w h;
     for y = 0 to h - 1 do
       let byte = ref 0 in
       let pos = ref 0 in

       let output_bit b =
         let bitmask = b lsl (7 - !pos) in
         byte := !byte lor bitmask;
         incr pos;
         if !pos = 8 then begin
           chunk_write_char och (char_of_int !byte);
           byte := 0;
           pos := 0;
         end
       in

       let flush_byte () =
         if !pos <> 0 then chunk_write_char och (char_of_int !byte)
       in

       for x = 0 to w - 1 do
         read_grey img x y (fun g ->
         output_bit g)
       done;

       flush_byte ()
     done

   | (Grey _ | GreyA _), ASCII,  1  ->
     let header = Printf.sprintf "P1\n%i %i\n" w h in
     chunk_write och header;
     for y = 0 to h - 1 do
       for x = 0 to w - 1 do
         read_grey img x y (fun g ->
         chunk_printf och "%i\n" g)
       done;
     done

   | (Grey _ | GreyA _), Binary, mv ->
     let header = Printf.sprintf "P5\n%i %i %i\n" w h mv in
     chunk_write och header;
     for y = 0 to h - 1 do
       for x = 0 to w - 1 do
        read_grey img x y (fun g ->
          if mv < 256
          then chunk_write_char och (char_of_int g)
          else begin
            let gl0 = char_of_int (g mod 256) in
            let gl1 = char_of_int (g lsr 8) in
            chunk_printf och "%c%c" gl1 gl0
          end)
       done;
     done

   | (Grey _ | GreyA _), ASCII,  mv ->
     let header = Printf.sprintf "P2\n%i %i %i\n" w h mv in
     chunk_write och header;
     for y = 0 to h - 1 do
       for x = 0 to w - 1 do
         read_grey img x y (fun g ->
         chunk_printf och "%i\n" g)
       done;
     done
  );

  close_chunk_writer och

include ReadPPM
let write cw img = write_ppm cw img Binary
