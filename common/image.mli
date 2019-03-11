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

type pixmap =
  | Grey  of Pixmap.t
  | GreyA of Pixmap.t * Pixmap.t
  | RGB   of Pixmap.t * Pixmap.t * Pixmap.t
  | RGBA  of Pixmap.t * Pixmap.t * Pixmap.t * Pixmap.t

type image =
  { width   : int
  ; height  : int
  ; max_val : int
  ; pixels  : pixmap }

val create_rgb : ?alpha:bool -> ?max_val:int -> int -> int -> image
(** [create_rgb ?alpha ?max_val width height] is an RGB image
    of dimensions [width] * [height].
    Raises {!Invalid_argument} if [width] or [height] are negative,
    or if [max_val] is not in the range \[1;65535\].*)

val create_grey: ?alpha:bool -> ?max_val:int -> int -> int -> image
(** [create_rgb ?alpha ?max_val width height] is a greyscale image
    of dimensions [width] * [height].
    Raises {!Invalid_argument} if [width] or [height] are negative,
    or if [max_val] is not in the range \[1;65535\].*)

val read_rgba  : image -> int -> int -> (int -> int -> int -> int -> 'a) -> 'a
val read_rgb   : image -> int -> int -> (int -> int -> int -> 'a) -> 'a
val read_greya : image -> int -> int -> (int -> int -> 'a) -> 'a
val read_grey  : image -> int -> int -> (int -> 'a) -> 'a

val write_rgba : image -> int -> int -> int -> int -> int -> int -> unit
val write_rgb  : image -> int -> int -> int -> int -> int -> unit
val write_greya: image -> int -> int -> int -> int -> unit
val write_grey : image -> int -> int -> int -> unit

exception Corrupted_image of string

module type ReadImage =
  sig
    val extensions : string list
    val size       : Reader.t -> int * int
    val parsefile  : Reader.t -> image
  end

exception Not_yet_implemented of string
