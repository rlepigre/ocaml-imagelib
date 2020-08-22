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

module Pixmap :
  sig
    open Bigarray
    type pixmap8  = (int, int8_unsigned_elt , c_layout) Array2.t
    type pixmap16 = (int, int16_unsigned_elt, c_layout) Array2.t

    type t =
      | Pix8  of pixmap8
      | Pix16 of pixmap16

    val create8  : int -> int -> t
    val create16 : int -> int -> t

    val get : t -> int -> int -> int
    val set : t -> int -> int -> int -> unit

    val fill : t -> int -> unit

    val copy : t -> t
    (** [copy t] is a copy of [t] using a new memory allocation.
        This is useful when code out of your control may hold references to a
        pixmap, since pixmaps are mutable.*)

    val compare : t -> t -> int
  end

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

val fill_rgb : ?alpha:int -> image -> int -> int -> int -> unit
(** [fill_rgb ?alpha image r g b] overwrites [image] with [r],[g],[b] colors. TODO *)

val fill_alpha : image -> int -> unit

val copy : image -> image
(** [copy image] is a copy of [image] backed my a new memory allocation,
    so that the mutations of either copy are independent of each other*)

val compare_image : image -> image -> int

exception Corrupted_image of string

module type ReadImage =
  sig
    open ImageUtil
    val extensions : string list
    val size       : chunk_reader -> int * int
    val parsefile  : chunk_reader -> image
  end

module type ReadImageStreaming =
  sig
    include ReadImage
    type read_state

    val read_streaming : ImageUtil.chunk_reader ->
      read_state option -> image option * int * read_state option
      (** [read_streaming io state] is an image frame,
          its suggested display time (in hundredths of a second;
          for animations), and
          optionally the state required to read the next frame.

          The first invocation should pass [state = None] to initialize a new
          reading context.

          If the resulting [read_state option] is [None],
          there are no more image frames available in [io].
          If it is [Some st], [st] must be passed to the next invocation of
          [read_streaming].

          @raises TODO when [io] ends prematurely.
      *)
  end

module type WriteImage =
  sig
    open ImageUtil
    val write : chunk_writer -> image -> unit
  end

exception Not_yet_implemented of string

module Resize : sig
  val scale_copy_layer : image -> src:image -> float -> image
end
