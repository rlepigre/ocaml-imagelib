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

(** This module provides an abstract representation for input readers. Such an
    input reader behaves similarly to a {!type:Pervasives.in_channel},  but it
    can actually represent the contents of a any stream of characters (be it a
    file, a string, a buffer or anything else you may need). *)

(** {3 Input reader and primitive operations} *)

(** Abstract type representing an input reader. *)
type reader

(** A short synonym of {!type:reader}. *)
type t = reader

(** [input_char r] consumes a single character from the given reader [r]. When
    no character is left, {!Pervasives.End_of_file} is raised. *)
val input_char : reader -> char

(** [input_byte r] is similar to [input_char r] but it returns the code of the
    read character (i.e., its value) as an 8-bit integer. This function may of
    course raise {!Pervasives.End_of_file} as well. *)
val input_byte : reader -> int

(** [input_bytes r n] consumes the [n] first characters (or bytes) from reader
    [r], and returns the corresponding string. If too few characters remain in
    the reader to satisfy the query, {!Pervasives.End_of_file} is raised. Note
    that the behaviour of the function is unspecified if [n] is negative. *)
val input_bytes : reader -> int -> string

(** [input_line r] consumes a single line of input from reader [r],  i.e.,  an
    arbitrary sequence of characters delimited by ['\n'] (or, possibly, by end
    of input in the case of the last line of a stream). The read line is given
    back as a string with no trailing ['\n']. If end of input is reached prior
    to consuming a single character, {!Pervasives.End_of_file} is raised. *)
val input_line : reader -> string

(** [position r] returns the current position of the input in the reader. This
    function can be used, for example, to build relevant error messages. *)
val position : reader -> int

(** [close r] closes the reader [r], meaning that the associated memory can be
    deallocated. As a consequence the above functions behave in an unspecified
    way on a reader [r] that has been closed. Closing never fails. *)
val close : reader -> unit

(** {3 Constructing a reader} *)

(** [of_channel ic] converts the input channel [ic] into a reader. The channel
    should not be further accessed (directly) after the call to this function.
    In particular, it should not be closed directly. *)
val of_channel : Pervasives.in_channel -> reader

(** [of_file path] builds a reader for the file corresponding to [path]. If it
    does not exist then {!exception:Sys_error} is raised  (in exactly the same
    way as with {!val:Pervasives.open_in}. *)
val of_file : string -> reader

(** [of_string s] builds a reader for the string [s]. *)
val of_string : string -> reader

(** {3 Generic construction of readers} *)

(** [make_reader ~input_char ~input_line ~close position input_bytes]  creates
    a new reader using the provided functions.  If the [close] function is not
    given, then the closing operation does nothing. If [input_char] is omitted
    then it is derived (not very efficiently) from [input_bytes] (and the same
    is also true for [input_line]). *)
val make_reader : ?input_char:(unit -> char) -> ?input_line:(unit -> string)
  -> ?close:(unit -> unit) -> (unit -> int) -> (int -> string) -> reader
