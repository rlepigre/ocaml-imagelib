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

open Image
open ImageUtil

let only_once ich read_chunks ctype =
  if List.mem ctype !read_chunks
  then begin
    let msg = Printf.sprintf
        "Chunk %s should not appear more than once..." ctype
    in
    close_chunk_reader ich;
    raise (Corrupted_image msg)
  end

let only_before ich read_chunks ctype ctype' =
  if List.mem ctype' !read_chunks
  then begin
    let msg = Printf.sprintf
        "Chunk %s should appear before chunk %s..." ctype ctype'
    in
    close_chunk_reader ich;
    raise (Corrupted_image msg)
  end

let only_after ich read_chunks ctype' ctype =
  if not (List.mem ctype' !read_chunks)
  then begin
    let msg = Printf.sprintf
        "Chunk %s should appear after chunk %s..." ctype ctype'
    in
    close_chunk_reader ich;
    raise (Corrupted_image msg)
  end

let is_first_chunk ich read_chunks ctype =
  if ([] <> !read_chunks)
  then begin
    let msg = Printf.sprintf
        "Chunk %s can only be the first chunk..." ctype
    in
    close_chunk_reader ich;
    raise (Corrupted_image msg)
  end

let is_not_first_chunk ich read_chunks ctype =
  if ([] = !read_chunks)
  then begin
    let msg = Printf.sprintf
        "Chunk %s cannot be the first chunk..." ctype
    in
    close_chunk_reader ich;
    raise (Corrupted_image msg)
  end

let is_not_compatible_with ich read_chunks ctype ctype' =
  if List.mem ctype' !read_chunks
  then begin
    let msg = Printf.sprintf
        "Chunk %s is not compatible with chunk %s..." ctype ctype'
    in
    close_chunk_reader ich;
    raise (Corrupted_image msg)
  end

let last_chunk read_chunks =
  match !read_chunks with
  | []   -> "NONE"
  | x::_ -> x

let has_read_chunk read_chunks ctype =
  List.mem ctype !read_chunks

let not_after ich read_chunks ctype' ctype =
  if List.mem ctype' !read_chunks
  then begin
    let msg = Printf.sprintf
        "Chunk %s cannot appear after chunk %s..." ctype ctype'
    in
    close_chunk_reader ich;
    raise (Corrupted_image msg)
  end


