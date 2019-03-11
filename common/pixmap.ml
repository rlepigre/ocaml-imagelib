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

open Bigarray

type pixmap =
  | Pix8  of (int, int8_unsigned_elt , c_layout) Array2.t
  | Pix16 of (int, int16_unsigned_elt, c_layout) Array2.t

type t = pixmap

let create8  : int -> int -> pixmap = fun w h ->
  Pix8 (Array2.create int8_unsigned c_layout w h)

let create16 : int -> int -> pixmap = fun w h ->
  Pix16 (Array2.create int16_unsigned c_layout w h)

let get : pixmap -> int -> int -> int = fun m ->
  match m with
  | Pix8  p -> Array2.get p
  | Pix16 p -> Array2.get p

let set : pixmap -> int -> int -> int -> unit = fun m ->
  match m with
  | Pix8  p -> Array2.set p
  | Pix16 p -> Array2.set p
