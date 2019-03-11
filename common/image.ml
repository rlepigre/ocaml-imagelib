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

type t = image

let create_rgb ?(alpha=false) ?(max_val=255) width height =
  if not (1 <= max_val && max_val <= 65535) then
    invalid_arg "create_rgb: false: (1 <= max_val && max_val <= 65535)";
  if not (width > 0 && height > 0) then
    invalid_arg "create_rgb: width or height <= 0";
  let create = if max_val <= 255 then Pixmap.create8 else Pixmap.create16 in
  let pixels =
    let r = create width height in
    let g = create width height in
    let b = create width height in
    if alpha then RGBA(r, g, b, create width height) else RGB(r, g, b)
  in
  { width ; height ; max_val ; pixels }

let create_grey ?(alpha=false) ?(max_val=255) width height =
  if not (1 <= max_val && max_val <= 65535) then
    invalid_arg "create_grey: false: (1 <= max_val && max_val <= 65535)";
  if not (width > 0 && height > 0) then
    invalid_arg "create_grey: width or height <= 0";
  let create = if max_val <= 255 then Pixmap.create8 else Pixmap.create16 in
  let pixels =
    let g = create width height in
    if alpha then GreyA(g, create width height) else Grey(g)
  in
  { width ; height ; max_val ; pixels }

let read_rgba i x y fn =
  match i.pixels with
  | RGB(r,g,b)    ->
      let r = Pixmap.get r x y in
      let g = Pixmap.get g x y in
      let b = Pixmap.get b x y in
      let a = i.max_val in
      fn r g b a
  | RGBA(r,g,b,a) ->
      let r = Pixmap.get r x y in
      let g = Pixmap.get g x y in
      let b = Pixmap.get b x y in
      let a = Pixmap.get a x y in
      fn r g b a
  | Grey(g)       ->
      let gr = Pixmap.get g x y in
      let a = i.max_val in
      fn gr gr gr a
  | GreyA(g,a)    ->
      let gr = Pixmap.get g x y in
      let a = Pixmap.get a x y in
      fn gr gr gr a

let read_rgb i x y fn =
  match i.pixels with
  | RGB(r,g,b)
  | RGBA(r,g,b,_) ->
      let r = Pixmap.get r x y in
      let g = Pixmap.get g x y in
      let b = Pixmap.get b x y in
      fn r g b
  | Grey(g)
  | GreyA(g,_)    ->
      let gr = Pixmap.get g x y in
      fn gr gr gr

let read_greya i x y fn =
  match i.pixels with
  | RGB(r,g,b)    ->
      let r = Pixmap.get r x y in
      let g = Pixmap.get g x y in
      let b = Pixmap.get b x y in
      let g = (r + g + b) / 3 in
      let a = i.max_val in
      fn g a
  | RGBA(r,g,b,a) ->
      let r = Pixmap.get r x y in
      let g = Pixmap.get g x y in
      let b = Pixmap.get b x y in
      let g = (r + g + b) / 3 in
      let a = Pixmap.get a x y in
      fn g a
  | Grey(g)       ->
      let g = Pixmap.get g x y in
      let a = i.max_val in
      fn g a
  | GreyA(g,a)    ->
      let g = Pixmap.get g x y in
      let a = Pixmap.get a x y in
      fn g a

let read_grey i x y fn =
  match i.pixels with
  | RGB(r,g,b)
  | RGBA(r,g,b,_) ->
      let r = Pixmap.get r x y in
      let g = Pixmap.get g x y in
      let b = Pixmap.get b x y in
      let g = (r + g + b) / 3 in
      fn g
  | Grey(g)
  | GreyA(g,_)    ->
      let g = Pixmap.get g x y in
      fn g

let write_rgba i x y r g b a =
  match i.pixels with
  | RGB(rr,gg,bb)     ->
      Pixmap.set rr x y r;
      Pixmap.set gg x y g;
      Pixmap.set bb x y b
  | RGBA(rr,gg,bb,aa) ->
      Pixmap.set rr x y r;
      Pixmap.set gg x y g;
      Pixmap.set bb x y b;
      Pixmap.set aa x y a
  | Grey(gg)          ->
      let g = (r + g + b) / 3 in
      Pixmap.set gg x y g
  | GreyA(gg,aa)      ->
      let g = (r + g + b) / 3 in
      Pixmap.set gg x y g;
      Pixmap.set aa x y a

let write_rgb i x y r g b =
  match i.pixels with
  | RGB(rr,gg,bb)
  | RGBA(rr,gg,bb,_) ->
      Pixmap.set rr x y r;
      Pixmap.set gg x y g;
      Pixmap.set bb x y b
  | Grey(gg)
  | GreyA(gg,_)      ->
      let g = (r + g + b) / 3 in
      Pixmap.set gg x y g

let write_greya i x y g a =
  match i.pixels with
  | RGB(rr,gg,bb)     ->
      Pixmap.set rr x y g;
      Pixmap.set gg x y g;
      Pixmap.set bb x y g
  | RGBA(rr,gg,bb,aa) ->
      Pixmap.set rr x y g;
      Pixmap.set gg x y g;
      Pixmap.set bb x y g;
      Pixmap.set aa x y a
  | Grey(gg)          ->
      Pixmap.set gg x y g
  | GreyA(gg,aa)      ->
      Pixmap.set gg x y g;
      Pixmap.set aa x y a

let write_grey i x y g =
  match i.pixels with
  | RGB(rr,gg,bb)
  | RGBA(rr,gg,bb,_) ->
      Pixmap.set rr x y g;
      Pixmap.set gg x y g;
      Pixmap.set bb x y g
  | Grey(gg)
  | GreyA(gg,_)      ->
      Pixmap.set gg x y g
