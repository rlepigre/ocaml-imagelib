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

(** This module provides a high-level representation for images. They are seen
    as simple arrays of pixels, that can be read or written depending on color
    modes. The internal representation (using {!module:Pixmap}) is exposed, so
    that users can efficiently convert images to other formats. *)

(** {3 Representation of an image} *)

(** Representation of the pixels of an image, with one array per component. *)
type pixmap =
  | Grey  of Pixmap.t
  (** Greyscale, no alpha channel.   *)
  | GreyA of Pixmap.t * Pixmap.t
  (** Greyscale, with alpha channel. *)
  | RGB   of Pixmap.t * Pixmap.t * Pixmap.t
  (** RGB mode, no alpha channel.    *)
  | RGBA  of Pixmap.t * Pixmap.t * Pixmap.t * Pixmap.t
  (** RGB mode, with alpha channel.  *)

(** Representation of an image. *)
type image =
  { width   : int    (** Width of the image in pixels.  *)
  ; height  : int    (** Height of the image in pixels. *)
  ; max_val : int    (** Maximum value for a component. *)
  ; pixels  : pixmap (** Actual image data as a pixmap. *) }

(** A short synonym of {!type:image}. *)
type t = image

(** {3 Creation of images} *)

(** [create_rgb ~alpha ~max_val w h] creates an RGB mode image of size [(w,h)]
    and with maximum component value of [max_val] (with default value [255] if
    it is not given). If [alpha] is [true], then the created image is given an
    alpha (transparency) channel (the default value is [false]). Note that the
    function  raises {!exception:Invalid_argument} if [w] and [h] are not both
    strictly positive integers, or if [max_val] is not between 1 and 65536. *)
val create_rgb : ?alpha:bool -> ?max_val:int -> int -> int -> image

(** [create_grey ?alpha ?max_val w h] creates a greyscale image of width  [w],
    height [h] and maximum component value [max_val]  (with default [255] when
    if it is not given). An alpha channel is created if [alpha] is [true], and
    its value is [false] by default.  As for {!val:create_rgb},  the exception
    {!exception:Invalid_argument} is raised if [w] and [h] are not  (strictly)
    positive integers, or if [max_val] is not between 1 and 65536. *)
val create_grey: ?alpha:bool -> ?max_val:int -> int -> int -> image

(** {3 Reading pixel values} *)

(** [read_rgba img x y f] applies function [f] to the (RGBA) components of the
    pixel at position [(x,y)] in [img]. The value of the four component is fed
    to [f] in order. Note that the function also works when [img] is not RGBA,
    and in that case sensible values for the components are constructed.  Note
    that the function raises {!exception:Invalid_argument} if [(x,y)] is not a
    valid pixel position in [img]. *)
val read_rgba : image -> int -> int -> (int -> int -> int -> int -> 'a) -> 'a

(** [read_rgb img x y f] applies function [f] to the (RGB) components of pixel
    [(x,y)] in [img]. The value of the three component is fed to [f] in order.
    Note that the function also works when [img] is not RGB,  and in that case
    sensible values for the components are constructed. Note that the function
    raises {!exception:Invalid_argument} if [(x,y)] is invalid. *)
val read_rgb : image -> int -> int -> (int -> int -> int -> 'a) -> 'a

(** [read_greya img x y f] applies [f] to the (greyscale and alpha) components
    of the pixel at coordinate [(x,y)] in [img].  The component values are fed
    to [f] in order. Note that the function works for images in any mode,  and
    not only greyscale. In the case of an RGB image, for example,  the mean of
    the components (rounded down) is used for the greyscale component. As with
    the previous functions, {!exception:Invalid_argument} is raised if [(x,y)]
    does not correspond to valid pixel coordinates. *)
val read_greya : image -> int -> int -> (int -> int -> 'a) -> 'a

(** [read_grey img x y f] applies [f] to the greyscale component of the  pixel
    at coordinate [(x,y)] in [img]. Note that the function works for images in
    any mode, not only greyscale. In the case of an RGB image, the mean of the
    components (rounded down) is used for the greyscale component. As with the
    previous functions,  {!exception:Invalid_argument} is raised when  [(x,y)]
    does not correspond to valid pixel coordinates. *)
val read_grey : image -> int -> int -> (int -> 'a) -> 'a

(** {3 Writing pixel values} *)

(** [write_rgba img x y r g b a] ... *)
val write_rgba : image -> int -> int -> int -> int -> int -> int -> unit

(** [write_rgb img x y r g b] ... *)
val write_rgb : image -> int -> int -> int -> int -> int -> unit

(** [write_greya img x y g a] ... *)
val write_greya : image -> int -> int -> int -> int -> unit

(** [write_grey img x y g] ... *)
val write_grey : image -> int -> int -> int -> unit
