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
open Imagelib_common
open Interface
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

  (* check if any of space, CR, LF, TAB, VT, or FF. *)
  let is_space c =
    match c with
    | ' '    (* Space *)
    | '\n'   (* LF    *)
    | '\t'   (* TAB   *)
    | '\r'   (* CR    *)
    | '\x0B' (* VT    *)
    | '\xFF' (* 0XFF  *) -> true
    | _                  -> false

  type magic = P1 | P2 | P3 | P4 | P5 | P6

  let read_until_space : Buffer.t -> Reader.t -> unit = fun buf r ->
    let rec loop () =
      let c = Reader.input_char r in
      if not (is_space c) then (Buffer.add_char buf c; loop ())
    in loop ()

  let read_magic : Reader.t -> magic = fun r ->
    let buf = Buffer.create 3 in
    read_until_space buf r;
    match Buffer.contents buf with
    | "P1" -> P1 | "P2" -> P2 | "P3" -> P3
    | "P4" -> P4 | "P5" -> P5 | "P6" -> P6
    | _ -> raise (Corrupted_image "Invalid magic number...")

  let read_until_non_space : Reader.t -> char = fun r ->
    let rec loop () =
      match Reader.input_char r with
      | c when is_space c -> loop ()
      | '#'               -> ignore (Reader.input_line r); loop ()
      | c                 -> c
    in loop ()

  let read_token : Reader.t -> int = fun r ->
    let c = read_until_non_space r in
    let buf = Buffer.create 5 in
    Buffer.add_char buf c; read_until_space buf r;
    try int_of_string (Buffer.contents buf)
    with _ -> raise (Corrupted_image "Invalid token...")

  let read_header : Reader.t -> magic * int * int * int = fun r ->
    let magic = read_magic r in
    let width = read_token r in
    let height = read_token r in
    let max_val = if magic = P1 || magic = P4 then 1 else read_token r in
    (magic, width, height, max_val)

  (*
   * Reads the size of a PPM image from a file.
   * Does not check if the file is correct. Only goes as far as the header.
   * Returns a couple (width, height)
   *)
  let size : Reader.t -> int * int = fun r ->
    let (_,w,h,_) = read_header r in
    Reader.close r; (w, h)

  (*
   * Read a PPM format image file.
   * Arguments:
   *   - fn : the path to the file.
   * Raise the exception Corrupted_image if the file is not valid.
   *)
  let parsefile : Reader.t -> image = fun r ->
    try
      let (magic, w, h, max_val) = read_header r in
      match magic with
      | P1
      | P2 ->
          let image = create_grey ~max_val:max_val w h in
          for y = 0 to h - 1 do
            for x = 0 to w - 1 do
              write_grey image x y (read_token r)
            done
          done;
          Reader.close r; image
      | P3 ->
          let image = create_rgb ~max_val:max_val w h in
          for y = 0 to h - 1 do
            for x = 0 to w - 1 do
              let cr = read_token r in
              let cg = read_token r in
              let cb = read_token r in
              write_rgb image x y cr cg cb
            done
          done;
          Reader.close r; image
      | P4 ->
          let image = create_grey ~max_val:1 w h in
          for y = 0 to h - 1 do
            let x = ref 0 in
            let byte = ref 0 in
            while !x < w do
              if !x mod 8 = 0 then byte := Reader.input_byte r;
              let byte_pos = !x mod 8 in
              let v = (!byte lsr (7 - byte_pos)) land 1 in
              write_grey image !x y v;
              incr x
            done
          done;
          Reader.close r; image
      | P5 ->
          let image = create_grey ~max_val:max_val w h in
          for y = 0 to h - 1 do
            for x = 0 to w - 1 do
              if max_val <= 255 then (
                let b0 = Reader.input_byte r in
                write_grey image x y b0)
              else (
                let b0 = Reader.input_byte r in
                let b1 = Reader.input_byte r in
                write_grey image x y ((b0 lsl 8) + b1))
            done
          done;
          Reader.close r; image
      | P6 ->
          let image = create_rgb ~max_val:max_val w h in
          for y = 0 to h - 1 do
            for x = 0 to w - 1 do
              if max_val <= 255 then (
                let cr = Reader.input_byte r in
                let cg = Reader.input_byte r in
                let cb = Reader.input_byte r in
                write_rgb image x y cr cg cb)
              else (
                let r1 = Reader.input_byte r in
                let r0 = Reader.input_byte r in
                let g1 = Reader.input_byte r in
                let g0 = Reader.input_byte r in
                let b1 = Reader.input_byte r in
                let b0 = Reader.input_byte r in
                let cr = (r1 lsl 8) + r0 in
                let cg = (g1 lsl 8) + g0 in
                let cb = (b1 lsl 8) + b0 in
                write_rgb image x y cr cg cb)
            done
          done;
          Reader.close r; image
    with End_of_file -> raise (Corrupted_image "Truncated file")
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
let write_ppm fn img mode =
  let och = open_out_bin fn in
  let w = img.width and h = img.height in

  (match img.pixels, mode, img.max_val with
   | (RGB _ | RGBA _)  , Binary, mv ->
     Printf.fprintf och "P6\n%i %i %i\n" w h mv;
     for y = 0 to h - 1 do
       for x = 0 to w - 1 do
         read_rgb img x y (fun r g b ->
           if mv < 256
           then begin
             Printf.fprintf och "%c%c%c"
               (char_of_int r) (char_of_int g) (char_of_int b)
           end else begin
             let r0 = char_of_int (r mod 256) in
             let r1 = char_of_int (r lsr 8) in
             let g0 = char_of_int (g mod 256) in
             let g1 = char_of_int (g lsr 8) in
             let b0 = char_of_int (b mod 256) in
             let b1 = char_of_int (b lsr 8) in
             Printf.fprintf och "%c%c%c%c%c%c" r1 r0 g1 g0 b1 b0
           end)
       done
     done
   | (RGB _ | RGBA _)  , ASCII,  mv ->
     Printf.fprintf och "P3\n%i %i %i\n" w h mv;
     for y = 0 to h - 1 do
       for x = 0 to w - 1 do
          read_rgb img x y (fun r g b ->
         Printf.fprintf och "%i %i %i\n" r g b)
       done;
     done
   | (Grey _ | GreyA _), Binary, 1  ->
     Printf.fprintf och "P4\n%i %i\n" w h;
     for y = 0 to h - 1 do
       let byte = ref 0 in
       let pos = ref 0 in

       let output_bit b =
         let bitmask = b lsl (7 - !pos) in
         byte := !byte lor bitmask;
         incr pos;
         if !pos = 8 then begin
           output_char och (char_of_int !byte);
           byte := 0;
           pos := 0;
         end
       in

       let flush_byte () =
         if !pos <> 0 then output_char och (char_of_int !byte)
       in

       for x = 0 to w - 1 do
         read_grey img x y (fun g ->
         output_bit g)
       done;

       flush_byte ()
     done

   | (Grey _ | GreyA _), ASCII,  1  ->
     let header = Printf.sprintf "P1\n%i %i\n" w h in
     output_string och header;
     for y = 0 to h - 1 do
       for x = 0 to w - 1 do
         read_grey img x y (fun g ->
         Printf.fprintf och "%i\n" g)
       done;
     done

   | (Grey _ | GreyA _), Binary, mv ->
     let header = Printf.sprintf "P5\n%i %i %i\n" w h mv in
     output_string och header;
     for y = 0 to h - 1 do
       for x = 0 to w - 1 do
        read_grey img x y (fun g ->
          if mv < 256
          then Printf.fprintf och "%c" (char_of_int g)
          else begin
            let gl0 = char_of_int (g mod 256) in
            let gl1 = char_of_int (g lsr 8) in
            Printf.fprintf och "%c%c" gl1 gl0
          end)
       done;
     done

   | (Grey _ | GreyA _), ASCII,  mv ->
     let header = Printf.sprintf "P2\n%i %i %i\n" w h mv in
     output_string och header;
     for y = 0 to h - 1 do
       for x = 0 to w - 1 do
         read_grey img x y (fun g ->
         Printf.fprintf och "%i\n" g)
       done;
     done
  );

  close_out och
