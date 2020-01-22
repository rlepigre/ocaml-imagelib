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

type chunk_reader_error = [`End_of_file of int]
type chunk_reader = ([`Bytes of int | `Close]) ->
                    (string, chunk_reader_error) result

type chunk_writer =
  [`String of string | `Close] ->
  (unit, [`Write_error]) result

let chunk_write och (str:string) =
  match och (`String str) with
  | Ok () -> ()
  | Error `Write_error -> raise (Invalid_argument "imagelib write error")

let chunk_write_char och ch =
  chunk_write och (String.make 1 ch)

(* note that [och] is used to recover from weak polymorphism,
   so currying [chunk_printf och] will fix the number and types of arguments.*)
let chunk_printf : 'x. chunk_writer ->
  ('x, unit, string, unit) format4 -> 'x = fun och ->
  Printf.kprintf (chunk_write och)

let chunk_writer_of_buffer (buf:Buffer.t) =
  function
  | `String s -> Buffer.add_string buf s; Ok ()
  | `Close -> Ok ()

open Stdlib


(*
 * Test whether a character is a digit.
 *)
let is_digit c =
  List.mem c ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9']

(*
 * Reads a list of positive integers written in decimal from a string.
 * The integers can be separated by anything.
 *)
let int_array_from_string str =
  let words = ref [] in

  let wpos = ref (-1) in

  let len = String.length str in
  for i = 0 to len - 1 do
    if is_digit str.[i]
    then begin
      if !wpos < 0 then wpos := i;
      if i = len - 1
      then begin
        let w = String.sub str !wpos (len - !wpos) in
        words := (int_of_string w) :: !words;
      end
    end else begin
      if !wpos >= 0
      then begin
        let w = String.sub str !wpos (i - !wpos) in
        words := (int_of_string w) :: !words;
        wpos := -1;
      end
    end
  done;

  Array.of_list (List.rev !words)

(*
 * Read a string and returns the integer value of every character in an
 * array.
 *)
let byte_array_from_string str =
  let len = String.length str in
  Array.init len (fun i -> int_of_char str.[i])

(*
 * Computes the n-th power of 2.
 *)
let rec pow_of_2 n =
  if n = 0 then 1
  else (2 * pow_of_2 (n - 1))

(*
 * Same as Array.init but for dimension 2.
 * Arguments:
 *   - (w,h) : width / height couple.
 *   - f : initialization function.
 *)
let init_matrix (w,h) f =
  Array.init w (fun x -> Array.init h (fun y -> f (x,y)))

(*
 * Displays the hexadecimal representation of a string in the same way as in
 * an hexadecimal editor.
 * Arguments:
 *   - s : the string.
 *)
let show_string_hex s =
  let len = String.length s in
  let count = ref 0 in
  for i = 0 to len - 1 do
    let v = int_of_char s.[i] in
    Printf.fprintf stderr "%s%x" (if v < 16 then "0" else "") v;
    incr count;
    Printf.fprintf stderr "%s"
      (if !count mod 16 = 0 then "\n" else " ")
  done;
  if !count mod 16 <> 0 then Printf.fprintf stderr "\n";
  Printf.fprintf stderr "%!"

(*
 * Fetch bytes on an input channel and store them in a string.
 * Arguments:
 *   - ich : the input channel.
 *   - n : number of bytes to fecth
 * Returns a string of length n.
 *)
let get_bytes (reader:chunk_reader) num_bytes =
  reader (`Bytes num_bytes)
  |> function | Ok x -> x
              | Error (`End_of_file _) -> raise End_of_file

let chunk_char (reader:chunk_reader) = String.get (get_bytes reader 1) 0
let chunk_byte (reader:chunk_reader) = chunk_char reader |> Char.code

let chunk_reader_of_string s : chunk_reader =
  let offset = ref 0 in
  function
  | `Close -> Ok ""
  | `Bytes n ->
    begin match String.sub s !offset n with
      | s -> offset := !offset + n ; Ok s
      | exception Invalid_argument _ -> Error (`End_of_file (String.length s))
    end

let close_chunk_reader (reader:chunk_reader) = ignore (reader `Close)

let close_chunk_writer (wr:chunk_writer) = ignore (wr `Close)

let chunk_line (reader:chunk_reader) =
  let rec loop acc =
    match chunk_char reader with
    | '\n' ->
      let a = Array.of_list acc in
      Bytes.init (Array.length a) (fun i -> a.(i)) |> Bytes.to_string
    | c  -> loop (c::acc)
  in loop []

let print_byte v =
  for i = 7 downto 0 do
    let b = (v lsr i) land 1 in
    Printf.fprintf stderr "%i" b
  done;
  Printf.fprintf stderr "%!"

(*
 * Builds an integer which byte reprentation contains a given number of ones
 * starting form the right.
 * Arguments:
 *   - i : the number of ones.
 * Returns an integer.
 *)
let rec ones i = if i == 1 then 1 else ((ones (i-1)) lsl 1) lor 1 ;;

(*
 * Converts a string of size 2 into an integer
 * Arguments:
 *   - s : the string.
 * Returns an integer.
 *)
let int_of_str2_le s =
  (int_of_char s.[0]) lor ((int_of_char s.[1]) lsl 8)

(*
 * Converts a string of size 4 into an integer WITHOUT taking care of
 * overflow...
 * Arguments:
 *   - s : the string.
 * Returns an integer.
 *)
let int_of_str4 s =
  int_of_char s.[0] lsl 24 +
  int_of_char s.[1] lsl 16 +
  int_of_char s.[2] lsl 8 +
  int_of_char s.[3]

(*
 * Converts a string of size 4 into an integer WITHOUT taking care of
 * overflow. Using little endian
 * Arguments:
 *   - s : the string.
 * Returns an integer.
 *)
let int_of_str4_le s =
  (int_of_char s.[0]) lor
  ((int_of_char s.[1]) lsl 8) lor
  ((int_of_char s.[2]) lsl 16) lor
  ((int_of_char s.[3]) lsl 24)

(*
 * Converts a string of size 4 into an Int32.
 * Arguments:
 *   - s : the string (should have size 4 at least).
 * Returns an Int32.
 *)
let int32_of_str4 s =
  let (<<) = Int32.shift_left in
  let (++) = Int32.add in
  ((Int32.of_int (int_of_char s.[0])) << 24) ++
  ((Int32.of_int (int_of_char s.[1])) << 16) ++
  ((Int32.of_int (int_of_char s.[2])) << 8) ++
  (Int32.of_int (int_of_char s.[3]))

(*
 * Converts a string of size 4 into an Int32 using little endian
 * Arguments:
 *   - s : the string (should have size 4 at least).
 * Returns an Int32.
 *)
let int32_of_str4_le s =
  let (<<) = Int32.shift_left in
  let (++) = Int32.add in
  let of_int_at x = Int32.of_int (int_of_char s.[x]) in
  (of_int_at 0) ++
  (of_int_at 1 << 8) ++
  (of_int_at 2 << 16) ++
  (of_int_at 3 << 24)

(*
 * Converts an integer into a string of length 4.
 * Arguments:
 *   - i : the integer
 * Returns a string.
 *)
let int_to_str4 i : Bytes.t =
  let s = Bytes.create 4 in
  let mask = ones 8 in
  Bytes.set s 0 @@ char_of_int (i lsr 24);
  Bytes.set s 1 @@ char_of_int ((i lsr 16) land mask);
  Bytes.set s 2 @@ char_of_int ((i lsr 8) land mask);
  Bytes.set s 3 @@ char_of_int (i land mask);
  s
