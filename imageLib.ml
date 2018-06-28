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
open Filename
open Image
open ImageUtil

open ImagePNG
open ImagePPM
open ImageXCF
open ImageJPG
open ImageGIF

module PNG = ImagePNG
module PPM = ImagePPM
module XCF = ImageXCF
module JPG = ImageJPG
module GIF = ImageGIF

let convert fn fn' =
  let ret =
    Unix.create_process "convert" [| "convert"; "--" ; fn ; fn'|]
      Unix.stdin Unix.stdout Unix.stderr in
  if ret <> 0 then
    raise (Failure (Printf.sprintf "convert fn:%S fn':%S failed" fn fn'))

let rm fn =
  Sys.remove fn

let warning fn msg =
  Printf.eprintf "[WARNING imagelib] file %s\n" fn;
  Printf.eprintf "  %s\n" msg;
  Printf.eprintf "  PNG is the prefered format!\n%!"

let size fn =
  let ext = String.lowercase_ascii (get_extension' fn) in
  let ich = chunk_reader_of_path fn in
  if List.mem ext ReadPNG.extensions
  then ReadPNG.size ich else
  if List.mem ext ReadPPM.extensions
  then ReadPPM.size ich else
  if List.mem ext ReadXCF.extensions
  then ReadXCF.size ich else
  if List.mem ext ReadJPG.extensions
  then ReadJPG.size ich else
  if List.mem ext ReadGIF.extensions
  then ReadGIF.size ich else
  begin
    warning fn "No support for image size...";
    let fn' = temp_file "image" ".png" in
    convert fn fn';
    let ich' = chunk_reader_of_path fn' in
    let sz = ReadPNG.size ich' in
    rm fn'; sz
  end

let openfile fn : image =
  let ext = String.lowercase_ascii (get_extension' fn) in
  let ich = chunk_reader_of_path fn in
  if List.mem ext ReadPNG.extensions
  then ReadPNG.parsefile ich else
  if List.mem ext ReadPPM.extensions
  then ReadPPM.parsefile ich else
  begin
    warning fn "Cannot read this image format...";
    let fn' = temp_file "image" ".png" in
    convert fn fn';
    let ich' = chunk_reader_of_path fn' in
    let img = ReadPNG.parsefile ich' in
    rm fn'; img
  end

let writefile fn i =
  let ext = String.lowercase_ascii (get_extension' fn) in
  if List.mem ext ReadPNG.extensions
  then write_png fn i else
  if List.mem ext ReadPPM.extensions
  then write_ppm fn i Binary else
  begin
    warning fn "Cannot write to this image format...";
    let fn' = temp_file "image" ".png" in
    write_png fn' i;
    convert fn' fn;
    rm fn'
  end
