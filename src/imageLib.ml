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

(* The format dispatchers match on the exact strings in their
   [extensions] lists, which all carry a leading dot (e.g. [".png"]).
   [Filename.extension] produces that dotted form, but the public
   [~extension:] parameters of this module do not require it, so callers may
   equally pass the bare name (["png"]).  Normalise case and the leading dot
   so that both spellings -- and either case -- select the same format. *)
let normalize_extension (extension : string) : string =
  let e = String.lowercase_ascii extension in
  if String.length e > 0 && e.[0] = '.' then e else "." ^ e

let size ~extension ich =
  let ext = normalize_extension extension in
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
  let ext = normalize_extension extension in
  if List.mem ext ImagePNG.extensions
  then ImagePNG.parsefile ich else
  if List.mem ext ImageGIF.extensions
  then ImageGIF.parsefile ich else
  if List.mem ext ImageJPG.extensions
  then ImageJPG.parsefile ich else
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
    let ext = normalize_extension extension in
    if List.mem ext ImagePNG.extensions
    then Some (ImagePNG.parsefile ich), 0, None else
    if List.mem ext ImageGIF.extensions
    then if_some (fun v -> `GIF v) (ImageGIF.read_streaming ich None) else
    if List.mem ext ImageJPG.extensions
    then Some (ImageJPG.parsefile ich), 0, None else
    if List.mem ext ImagePPM.extensions
    then Some (ImagePPM.parsefile ich), 0, None else
    if List.mem ext ImageBMP.extensions
    then Some (ImageBMP.parsefile ich), 0, None else
      raise (Not_yet_implemented ext)

let writefile ~extension (och:ImageUtil.chunk_writer) i =
  let extension = normalize_extension extension in
  if List.mem extension ImagePNG.extensions
  then ImagePNG.write och i else
  if List.mem extension ImageGIF.extensions
  then ImageGIF.write och i else
  if List.mem extension ImageJPG.extensions
  then ImageJPG.write och i else
  if List.mem extension ImagePPM.extensions
  then ImagePPM.write och i else
    raise (Not_yet_implemented extension)

module PNG = ImagePNG
module PPM = ImagePPM
module XCF = ImageXCF
module JPG = ImageJPG
module BMP = ImageBMP
module GIF = ImageGIF
