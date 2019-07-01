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

open ImagePNG
open ImagePPM
open ImageXCF
open ImageJPG
open ImageBMP

module PNG = ImagePNG
module PPM = ImagePPM
module XCF = ImageXCF
module JPG = ImageJPG
module GIF = ImageGIF
module BMP = ImageBMP

let size ~extension ich =
  let ext = String.lowercase_ascii extension in
  if List.mem ext ReadPNG.extensions
  then ReadPNG.size ich else
  if List.mem ext ReadPPM.extensions
  then ReadPPM.size ich else
  if List.mem ext ReadXCF.extensions
  then ReadXCF.size ich else
  if List.mem ext ReadJPG.extensions
  then ReadJPG.size ich else
  if List.mem ext GIF.extensions
  then GIF.size ich else
  if List.mem ext ReadBMP.extensions
  then ReadBMP.size ich else
    raise (Not_yet_implemented ext)

let openfile ~extension ich : image =
  let ext = String.lowercase_ascii extension in
  if List.mem ext ReadPNG.extensions
  then ReadPNG.parsefile ich else
  if List.mem ext GIF.extensions
  then GIF.parsefile ich else
  if List.mem ext ReadPPM.extensions
  then ReadPPM.parsefile ich else
  if List.mem ext ReadBMP.extensions
  then ReadBMP.parsefile ich else
    raise (Not_yet_implemented ext)

let openfile_streaming ~extension ich state =
  let if_some f = function
    | _, _, None as x  -> x
    | image, time, Some v -> image, time, Some (f v) in
  match state with
  | Some (`GIF t) ->
    if_some (fun v -> `GIF v) (GIF.read_streaming ich (Some t))
  | None ->
    let ext = String.lowercase_ascii extension in
    if List.mem ext ReadPNG.extensions
    then Some (ReadPNG.parsefile ich), 0, None else
    if List.mem ext GIF.extensions
    then if_some (fun v -> `GIF v) (GIF.read_streaming ich None) else
    if List.mem ext ReadPPM.extensions
    then Some (ReadPPM.parsefile ich), 0, None else
    if List.mem ext ReadBMP.extensions
    then Some (ReadBMP.parsefile ich), 0, None else
      raise (Not_yet_implemented ext)

let writefile ~extension (och:ImageUtil.chunk_writer) i =
  let extension = String.lowercase_ascii extension in
  if List.mem extension ImagePNG.ReadPNG.extensions
  then ImagePNG.write_png och i else
  if List.mem extension GIF.extensions
  then ImageGIF.write och i else
  if List.mem extension ImagePPM.ReadPPM.extensions
  then ImagePPM.write_ppm och i Binary else
    raise (Not_yet_implemented extension)
