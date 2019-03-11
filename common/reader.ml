(****************************************************************************)
(* This file is part of the Imagelib library.                               *)
(*                                                                          *)
(* The Imagelib library is free software:  you can redistribute  it  and/or *)
(* modify it under the terms of the GNU General Public License as published *)
(* by the Free Software Foundation, either version 3 of the License, or (at *)
(* your option) any later version.                                          *)
(*                                                                          *)
(* Imabelib is distributed in the hope that it will be useful,  but WITHOUT *)
(* ANY WARRANTY;  without even the implied warranty of  MERCHANTABILITY  or *)
(* FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for *)
(* more details.                                                            *)
(*                                                                          *)
(* You should have received a copy of the GNU General Public License along  *)
(* with the Imabelib library. If not, see <http://www.gnu.org/licenses/>.   *)
(*                                                                          *)
(* Copyright (C) 2014-2019 Rodolphe Lepigre.                                *)
(****************************************************************************)

type reader =
  { input_char  : unit -> char
  ; input_bytes : int  -> string
  ; input_line  : unit -> string
  ; close       : unit -> unit }

type t = reader

let input_bytes : reader -> int -> string = fun r -> r.input_bytes
let input_char : reader -> char = fun r -> r.input_char ()
let input_byte : reader -> int = fun r -> Char.code (r.input_char ())
let input_line : reader -> string = fun r -> r.input_line ()
let close : reader -> unit = fun r -> r.close ()

let of_channel : Pervasives.in_channel -> reader = fun ic ->
  { input_char  = (fun () -> Pervasives.input_char ic)
  ; input_bytes = Pervasives.really_input_string ic
  ; input_line  = (fun () -> Pervasives.input_line ic)
  ; close       = (fun () -> Pervasives.close_in_noerr ic) }

let of_file : string -> reader = fun path ->
  of_channel (Pervasives.open_in_bin path)

let of_string : string -> reader = fun s ->
  let len = String.length s in
  let offset = ref 0 in
  let input_char () =
    if !offset >= len then raise End_of_file;
    incr offset; s.[!offset - 1]
  in
  let input_bytes n =
    try let s = String.sub s !offset n in offset := !offset + n; s
    with Invalid_argument _ -> raise End_of_file
  in
  let input_line () =
    (* Check if end of file reached before the first character. *)
    if !offset >= len then raise End_of_file;
    (* Record the current offset and find the first newline (if any). *)
    let start = !offset in
    while !offset < len && String.get s !offset <> '\n' do incr offset done;
    (* The full line (withoug the newline) ends at the offset. *)
    let line = String.sub s start (!offset - start) in 
    (* Ignore the newline if we are not at then end of file. *)
    if !offset < len then incr offset;
    line
  in
  let close () = () in
  { input_char ; input_bytes ; input_line ; close }

let make_reader : ?input_char:(unit -> char) -> ?input_line:(unit -> string)
    -> ?close:(unit -> unit) -> (int -> string) -> reader =
  fun ?input_char ?input_line ?close input_bytes ->
    let input_char =
      match input_char with
      | None    -> (fun () -> String.get (input_bytes 1) 0)
      | Some(f) -> f
    in
    let input_line =
      match input_line with
      | None    ->
          let input_line () =
            let b = Buffer.create 73 in
            let last = ref (input_char ()) in
            begin
              try while !last <> '\n' do
                Buffer.add_char b !last;
                last := input_char ()
              done with End_of_file -> ()
            end;
            Buffer.contents b
          in input_line
      | Some(f) -> f
    in
    let close =
      match close with
      | None    -> ignore
      | Some(f) -> f
    in
    { input_char ; input_bytes ; input_line ; close }
