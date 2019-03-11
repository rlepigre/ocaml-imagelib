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

(** This module provides an efficient representation for pixmaps, used for the
    representation of image components. The implementation is exposed to allow
    for low-level manipulations of images, and in particular the conversion to
    other formats. *)

(** {3 Pixmap representation} *)

open Bigarray

(** Representation of a pixmap, using {!module:Bigarray}. *)
type pixmap =
  | Pix8  of (int, int8_unsigned_elt , c_layout) Array2.t
  | Pix16 of (int, int16_unsigned_elt, c_layout) Array2.t

(** Short synonym for {!type pixmap}. *)
type t = pixmap

(** {3 Creattion functions} *)

(** [create8 w h] creates a pixmap of width [w] and height [h],  with unsigned
    integer values stored on a single byte (or 8 bits). *)
val create8 : int -> int -> pixmap

(** [create16 w h] creates a pixmap of width [w] and height [h], with unsigned
    integer values stored on two bytes (or 16 bits). *)
val create16 : int -> int -> pixmap

(** [get m x y] returns the value stored at the coordinates [(x,y)], in pixmap
    [m].  Note that {!exception:Invalid_argument} is raised if the coordinates
    are not in the bounds of the pixmap (as declared at its creation). *)
val get : pixmap -> int -> int -> int

(** [set m x y v] sets the value stored at the coordinates [(x,y)] in [m],  to
    the value [v].  Note that {!exception:Invalid_argument} is raised when the
    coordinates are not in the bounds of the pixmap. *)
val set : pixmap -> int -> int -> int -> unit
