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

let size ~extension ich =
  let ext = String.lowercase_ascii extension in
  if List.mem ext ImagePNG.extensions
  then ImagePNG.size ich else
  if List.mem ext ImagePPM.extensions
  then ImagePPM.size ich else
  if List.mem ext ImageXCF.extensions
  then ImageXCF.size ich else
  if List.mem ext ImageJPG.extensions
  then ImageJPG.size ich else
  if List.mem ext ImageGIF.extensions
  then ImageGIF.size ich else
  if List.mem ext ImageBMP.extensions
  then ImageBMP.size ich else
    raise (Not_yet_implemented ext)

let openfile ~extension ich : image =
  let ext = String.lowercase_ascii extension in
  if List.mem ext ImagePNG.extensions
  then ImagePNG.parsefile ich else
  if List.mem ext ImageGIF.extensions
  then ImageGIF.parsefile ich else
  if List.mem ext ImagePPM.extensions
  then ImagePPM.parsefile ich else
  if List.mem ext ImageBMP.extensions
  then ImageBMP.parsefile ich else
    raise (Not_yet_implemented ext)

let openfile_streaming ~extension ich state =
  let if_some f = function
    | _, _, None as x  -> x
    | image, time, Some v -> image, time, Some (f v) in
  match state with
  | Some (`GIF t) ->
    if_some (fun v -> `GIF v) (ImageGIF.read_streaming ich (Some t))
  | None ->
    let ext = String.lowercase_ascii extension in
    if List.mem ext ImagePNG.extensions
    then Some (ImagePNG.parsefile ich), 0, None else
    if List.mem ext ImageGIF.extensions
    then if_some (fun v -> `GIF v) (ImageGIF.read_streaming ich None) else
    if List.mem ext ImagePPM.extensions
    then Some (ImagePPM.parsefile ich), 0, None else
    if List.mem ext ImageBMP.extensions
    then Some (ImageBMP.parsefile ich), 0, None else
      raise (Not_yet_implemented ext)

let writefile ~extension (och:ImageUtil.chunk_writer) i =
  let extension = String.lowercase_ascii extension in
  if List.mem extension ImagePNG.extensions
  then ImagePNG.write och i else
  if List.mem extension ImageGIF.extensions
  then ImageGIF.write och i else
  if List.mem extension ImagePPM.extensions
  then ImagePPM.write och i else
    raise (Not_yet_implemented extension)

module PNG = ImagePNG
module PPM = ImagePPM
module XCF = ImageXCF
module JPG = ImageJPG
module BMP = ImageBMP
module GIF = ImageGIF
