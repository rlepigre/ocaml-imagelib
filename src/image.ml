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

open ImageUtil

module Pixmap =
  struct
    open Bigarray

    type pixmap8  = (int, int8_unsigned_elt , c_layout) Array2.t
    type pixmap16 = (int, int16_unsigned_elt, c_layout) Array2.t

    type t =
      | Pix8  of pixmap8
      | Pix16 of pixmap16

    let create8  : int -> int -> t = fun w h ->
      Pix8 (Array2.create int8_unsigned c_layout w h)

    let create16 : int -> int -> t = fun w h ->
      Pix16 (Array2.create int16_unsigned c_layout w h)

    let get : t -> int -> int -> int = function
      | Pix8  p -> Array2.get p
      | Pix16 p -> Array2.get p

    let set : t -> int -> int -> int -> unit = function
      | Pix8  p -> Array2.set p
      | Pix16 p -> Array2.set p

    let fill t color : unit = match t with
      | Pix8 p -> Array2.fill p color
      | Pix16 p -> Array2.fill p color

    let copy = function
      | Pix8 p ->
        let w,h = Array2.dim1 p, Array2.dim2 p in
        let fresh = Array2.create int8_unsigned c_layout w h in
        Array2.blit p fresh ; Pix8 fresh
      | Pix16 p ->
        let w,h = Array2.dim1 p, Array2.dim2 p in
        let fresh = Array2.create int16_unsigned c_layout w h in
        Array2.blit p fresh ; Pix16 fresh

    let compare p1 p2 =
      let compare_pixels
          (type a1) (type a2) (* need this to be polymorphic re: elt*)
        (p1:(int,a1,c_layout) Array2.t)
        (p2:(int,a2,c_layout) Array2.t) =
        let d1 = Stdlib.compare (Array2.dim1 p1) (Array2.dim1 p2) in
        let d2 = Stdlib.compare (Array2.dim2 p1) (Array2.dim2 p2) in
        match d1, d2 with
        | 0, 0 -> (* everything matches, we need to look at pixels :-( *)
          let exception Found of int in
          begin try
            for x = 0 to (Array2.dim1 p1) -1 do
              for y = 0 to (Array2.dim2 p1) -1 do
                let diff = (Stdlib.compare:int->int->int)
                    (Array2.get p1 x y) (Array2.get p2 x y) in
                if diff <> 0 then
                  raise_notrace (Found diff)
              done
            done ;
            (* pixels are all the same: *)
            0
          with Found diff -> diff end
        | d, 0
        | 0, d -> d
        | d, _ -> d
      in
      match p1, p2 with
      | Pix8 _, Pix16 _ -> -2
      | Pix16 _, Pix8 _ -> 1
      | Pix8 p1, Pix8 p2 -> compare_pixels p1 p2
      | Pix16 p1, Pix16 p2 -> compare_pixels p1 p2
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

module type ReadImage =
  sig
    val extensions : string list
    val size       : chunk_reader -> int * int
    val parsefile  : chunk_reader -> image
  end

module type ReadImageStreaming =
  sig
    include ReadImage
    type read_state
    val read_streaming : chunk_reader -> read_state option ->
      image option * int * read_state option
  end

module type WriteImage =
  sig
    val write : chunk_writer -> image -> unit
  end


exception Corrupted_image of string
exception Not_yet_implemented of string

let create_rgb ?(alpha=false) ?(max_val=255) width height =
  if not (1 <= max_val && max_val <= 65535) then
    raise (Invalid_argument
             "create_rgb: false: (1 <= max_val && max_val <= 65535)") ;
  if not (width > 0 && height > 0) then
    raise (Invalid_argument "create_rgb: width or height <= 0") ;
  let create = if max_val <= 255 then Pixmap.create8 else Pixmap.create16 in
  let pixels =
    let r = create width height in
    let g = create width height in
    let b = create width height in
    if alpha then
      let a = create width height in
      RGBA (r,g,b,a)
    else RGB (r,g,b)
  in
  { width ; height ; max_val ; pixels }

let create_grey ?(alpha=false) ?(max_val=255) width height =
  if not (1 <= max_val && max_val <= 65535) then
    raise (Invalid_argument
             "create_grey: false: (1 <= max_val && max_val <= 65535)") ;
  if not (width > 0 && height > 0) then
    raise (Invalid_argument "create_grey: width or height <= 0") ;
  let create = if max_val <= 255 then Pixmap.create8 else Pixmap.create16 in
  let pixels =
    let g = create width height in
    if alpha then
      let a = create width height in
      GreyA (g,a)
    else Grey g
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
    (try
      let r = Pixmap.get r x y in
      let g = Pixmap.get g x y in
      let b = Pixmap.get b x y in
      fn r g b
    with Invalid_argument _ ->
      Printf.eprintf "x:%d y:%d\n%d : %d\n%d : %d\n%d %d" x y
        (match r with Pix8 r -> Bigarray.Array2.dim1 r | _ -> 0)
        (match r with Pix8 r -> Bigarray.Array2.dim1 r | _ -> 0)
        (match g with Pix8 g -> Bigarray.Array2.dim1 g | _ -> 0)
        (match g with Pix8 g -> Bigarray.Array2.dim1 g | _ -> 0)
        (match b with Pix8 b -> Bigarray.Array2.dim1 b | _ -> 0)
        (match b with Pix8 b -> Bigarray.Array2.dim1 b | _ -> 0)
      ;
      failwith "XXX")
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
      begin
        Pixmap.set rr x y r;
        Pixmap.set gg x y g;
        Pixmap.set bb x y b
      end
  | RGBA(rr,gg,bb,aa) ->
      begin
        Pixmap.set rr x y r;
        Pixmap.set gg x y g;
        Pixmap.set bb x y b;
        Pixmap.set aa x y a
      end
  | Grey(gg)          ->
      begin
        let g = (r + g + b) / 3 in
        Pixmap.set gg x y g
      end
  | GreyA(gg,aa)      ->
      begin
        let g = (r + g + b) / 3 in
        Pixmap.set gg x y g;
        Pixmap.set aa x y a
      end

let write_rgb i x y r g b =
  match i.pixels with
  | RGB(rr,gg,bb)
  | RGBA(rr,gg,bb,_) ->
      begin
        Pixmap.set rr x y r;
        Pixmap.set gg x y g;
        Pixmap.set bb x y b
      end
  | Grey(gg)
  | GreyA(gg,_)      ->
      begin
        let g = (r + g + b) / 3 in
        (* TODO see ImageUtil on blending *)
        Pixmap.set gg x y g
      end

let write_greya i x y g a =
  match i.pixels with
  | RGB(rr,gg,bb)     ->
      begin
        Pixmap.set rr x y g;
        Pixmap.set gg x y g;
        Pixmap.set bb x y g
      end
  | RGBA(rr,gg,bb,aa) ->
      begin
        Pixmap.set rr x y g;
        Pixmap.set gg x y g;
        Pixmap.set bb x y g;
        Pixmap.set aa x y a
      end
  | Grey(gg)          ->
      begin
        Pixmap.set gg x y g
      end
  | GreyA(gg,aa)      ->
      begin
        Pixmap.set gg x y g;
        Pixmap.set aa x y a
      end

let write_grey i x y g =
  match i.pixels with
  | RGB(rr,gg,bb)
  | RGBA(rr,gg,bb,_) ->
      begin
        Pixmap.set rr x y g;
        Pixmap.set gg x y g;
        Pixmap.set bb x y g
      end
  | Grey(gg)
  | GreyA(gg,_)      ->
      begin
        Pixmap.set gg x y g
      end

let fill_rgb ?alpha i r g b : unit =
  let fill_rgb ~rr ~gg ~bb =
    Pixmap.fill rr r ;
    Pixmap.fill gg g ;
    Pixmap.fill bb b in
  match i.pixels, alpha with
  | RGBA (rr, gg, bb, aa) , _ ->
    fill_rgb ~rr ~gg ~bb ;
    (match alpha with | None -> ()
                      | Some a -> Pixmap.fill aa a)
  | RGB (rr, gg, bb), None -> fill_rgb ~rr ~gg ~bb
  | RGB _, Some _ -> raise (Corrupted_image "fill_rgb with alpha")
  | Grey _, _ -> raise (Not_yet_implemented "fill_rgb for greyscale")
  | GreyA _, _ -> raise (Not_yet_implemented "fill_rgb for greyscale")

let fill_alpha i c = match i.pixels with
  | Grey _ | RGB _ -> ()
  | RGBA (_, _, _, aa)
  | GreyA (_, aa) -> Pixmap.fill aa c

let copy i = let open Pixmap in
  {i with pixels = match i.pixels with
       | Grey ii -> Grey (copy ii)
       | GreyA (ii,aa) -> GreyA (copy ii, copy aa)
       | RGB (rr,gg,bb)-> RGB (copy rr, copy gg, copy bb)
       | RGBA (rr,gg,bb,aa) -> RGBA (copy rr, copy gg, copy bb, copy aa) }

let compare_pixmap p1 p2 =
  match p1, p2 with
  | Grey g1 , Grey g2 -> Pixmap.compare g1 g2
  | GreyA (g1, a1), GreyA (g2,a2) ->
    let diff = Pixmap.compare g1 g2 in
    if diff <> 0 then diff else Pixmap.compare a1 a2
  | RGB (r1,g1,b1), RGB (r2,g2,b2) ->
    let rdiff = Pixmap.compare r1 r2 in
    if rdiff <> 0 then rdiff
    else let gdiff = Pixmap.compare g1 g2 in
      if gdiff <> 0 then gdiff else
        Pixmap.compare b1 b2
  | RGBA (r1,g1,b1,a1), RGBA (r2,g2,b2,a2) ->
    let rdiff = Pixmap.compare r1 r2 in
    if rdiff <> 0 then rdiff
    else let gdiff = Pixmap.compare g1 g2 in
      if gdiff <> 0 then gdiff else
        let bdiff = Pixmap.compare b1 b2 in
        if bdiff <> 0 then bdiff else
          Pixmap.compare a1 a2
  (* smaller than: *)
  | Grey _, (GreyA _ | RGB _ | RGBA _) -> -2
  | GreyA _, (RGB _ | RGBA _) -> -2
  | RGB _ , RGBA _ -> -2
  (* larger than: *)
  | GreyA _, Grey _ -> 3
  | RGB _, (GreyA _ | Grey _) -> 3
  | RGBA _, (RGB _ | GreyA _ | Grey _) -> 3

let compare_image
    {width ; height ; max_val ; pixels}
    {width = width2; height = height2 ; max_val = max_val2 ; pixels = pixels2}
  : int=
  match Stdlib.compare width width2,
        Stdlib.compare height height2,
        Stdlib.compare max_val max_val2 with
  | 0, 0, 0 -> compare_pixmap pixels pixels2
  (* match when two are zero and third is not: *)
  | 0, 0, diff -> diff (* differs on one value *)
  (* match when second is non-zero: *)
  | 0, 0, _ -> .
  | 0, d, _ -> d
  (* match when first is non-zero: *)
  | 0, _, _ -> .
  | d, _, _ -> d


module Resize : sig
  val scale_copy_layer : image -> src:image -> float (*gamma*) -> image
end = struct
  (* stolen from the gimp-image-scaler plugin
     https://blog.hartwork.org/?p=1173
     https://github.com/hartwork/gimp-image-scaler-plugin/blob/master/imagescaler/algorithms/cubic_gimp.py
*)
  let [@inline] s2rgba gamma (s:int array) =
    let max_val = 255 in (* TODO *)
    let max_valf = 255. in (* TODO *)
    let exp2linear pixel gamma =
      Float.pow ((float_of_int pixel) /. max_valf) gamma in
    let num = match s with
      | [| r ; g ; b |] -> [| r ; g ; b ; max_val |]
      | [| r ; g ; b ; a |] -> [| r ; g ; b ; a |] (* TODO *)
      | _whatever -> raise (Invalid_argument "TODO")
    in Array.map (fun c -> exp2linear c gamma) num

  let [@inline] clamp v _min _max =
    max _min (min v _max)


  let [@inline] get_rgba o_x o_y gamma (image:image) : float array =
    let x = clamp o_x 0 (image.width -1) in
    let y = clamp o_y 0 (image.height -1) in
    let colors = try read_rgba image x y (fun r g b a -> [| r;g;b;a |]) with
      | Invalid_argument _ ->
        Printf.eprintf "x:%d[%d] y:%d[%d]" x y image.width image.height ;
        failwith ("read_rgb failed: " ^ __LOC__)
    in
    s2rgba gamma colors

  let [@inline] weighted_sum dx dy s00 s10 s01 s11 =
    (* 0..1 float *)
    ((1. -. dy) *. ((1. -. dx) *. s00 +. dx *. s10)
     +. dy *. ((1. -. dx) *. s01 +. dx *. s11))

  let [@inline] interpolate_bilinear_gimp src_region sx sy xfrac yfrac gamma =
    (*Printf.printf "get_rgba sx:%d (sy+1):%d gamma:%f\n" sx(sy+1)gamma;*)
    let p1 = get_rgba  sx       sy      gamma src_region in
    let p2 = get_rgba (sx + 1)  sy      gamma src_region in
    let p3 = get_rgba  sx      (sy + 1) gamma src_region in
    let p4 = get_rgba (sx + 1) (sy + 1) gamma src_region in

    let pixel = [| 0. ; 0. ; 0. ;
                   float src_region.max_val |] in (* 0 for alpha *)

    (* TODO 
    if src_layer.has_alpha:
        alphasum = weighted_sum(xfrac, yfrac, p1[3], p2[3], p3[3], p4[3])
        if alphasum > 0:
            for b in xrange(0, 3):
                sum = weighted_sum(xfrac, yfrac,
                        p1[b] * p1[3], p2[b] * p2[3],
                        p3[b] * p3[3], p4[b] * p4[3])
                sum = sum / float(alphasum);

                pixel[b] = CLAMP(sum, 0, 255)

            pixel[3] = CLAMP(alphasum, 0, 255)
    else:
*)
    for b = 0 to 2 do
      let sum = weighted_sum xfrac yfrac p1.(b) p2.(b) p3.(b) p4.(b) in
      pixel.(b) <- clamp sum 0. 1.0
    done ;

    pixel

  let cubic_spline_fit dx pt0 pt1 pt2 pt3 =
    (* well this is some voodo. TODO. *)
    (((( (-.pt0) +. 3. *. pt1 -. 3. *. pt2 +. pt3 )*. dx +.
       ( 2. *. pt0 -. 5. *. pt1 +. 4. *. pt2 -. pt3 )
      ) *. dx +.
      ( (-.pt0) +. pt2 )
     ) *. dx
     +. (pt1 +. pt1)
    ) /. 2.0

  let interpolate_cubic_gimp src sx sy xfrac yfrac gamma =
    let rows : float array array = Array.init 4 (fun ry ->
        (* instead of allocating four arrays of rgba we need to just
           unroll them into four arrays of 16 pixel values TODO *)
        let arr = Array.make 16 0. in
        for rx = 0 to 3 do
          let rx = rx land 3 (* aka modulo 4 *) in
          let rgba = get_rgba (sx - 1 + rx) (sy -1 + ry) gamma src in
          Array.blit rgba 0 arr (rx*4) 4
        done ; arr
      ) in
    let s0, s1, s2, s3 = rows.(0), rows.(1), rows.(2), rows.(3) in
    let p0 = cubic_spline_fit xfrac s0.(3) s0.(7) s0.(11) s0.(15) in
    let p1 = cubic_spline_fit xfrac s1.(3) s1.(7) s1.(11) s1.(15) in
    let p2 = cubic_spline_fit xfrac s2.(3) s2.(7) s2.(11) s2.(15) in
    let p3 = cubic_spline_fit xfrac s3.(3) s3.(7) s3.(11) s3.(15) in

    let alphasum = cubic_spline_fit yfrac p0 p1 p2 p3 in
    (* alphasum goes from 0.0 to 1.0, it may be NaN *)
    Array.init 4 (fun b -> (* expression forms a rgba8888 array *)
        let p0 = cubic_spline_fit
            xfrac (s0.(0 + b)  *. s0.( 3)) (s0.( 4 + b)  *. s0.( 7))
            (s0.(8 + b)  *. s0.(11))     (s0.(12 + b) *. s0.(15)) in
        let p1 = cubic_spline_fit xfrac
            (s1.(0 + b) *. s1.( 3)) (s1.( 4 + b) *. s1.(7))
            (s1.(8 + b) *. s1.(11)) (s1.(12 + b) *. s1.(15)) in
        let p2 = cubic_spline_fit
            xfrac
            (s2.(0 + b) *. s2.( 3)) (s2.( 4 + b) *. s2.(7))
            (s2.(8 + b) *. s2.(11)) (s2.(12 + b) *. s2.(15)) in
        let p3 = cubic_spline_fit xfrac
            (s3.(0 + b) *. s3.( 3)) (s3.( 4 + b) *. s3.(7))
            (s3.(8 + b) *. s3.(11)) (s3.(12 + b) *. s3.(15)) in
        let sum = cubic_spline_fit yfrac p0 p1 p2 p3 /. alphasum in
        clamp sum 0. 255.
      )


  let linear2exp linear gamma =
    let maxvalf = 255. in (* todo *)
    (Float.pow linear (1.0 /. gamma)) *. maxvalf

  let rgb2s rgba gamma =
    Array.map (fun e -> int_of_float @@ linear2exp e gamma)rgba

  let set_rgba ~x ~y rgba gamma (region:image) =
    (*
    if layer.has_alpha:
        region[x, y] = rgba2s(rgba, gamma)
    else:*)
    (*region[x, y] = rgb2s(rgba[0:3], gamma)*)
    let c = rgb2s rgba gamma in
    let rgba = Array.map (fun v -> v) c in
    match rgba with
      [| r; g; b |] -> write_rgb region x y r g b
    | [| r ; g; b ; a |] ->
      write_rgba region x y r g b a
    | _ -> failwith "rgb2s"


  let scale_copy_layer (dst:image) ~(src:image) (*interpol*) gamma =
    let _ =  interpolate_bilinear_gimp , interpolate_cubic_gimp in
    let interpol =
      interpolate_cubic_gimp in
    let scalex = float src.width /. float dst.width in
    let scaley = float src.height /. float dst.height in

    for y = 0 to dst.height -1 do
      for x = 0 to dst.width -1 do
        let yfrac = (float y +. 0.5) *. scaley -. 0.5 in
        let sy = int_of_float yfrac in
        let yfrac = yfrac -. float sy in

        let xfrac = (float x +. 0.5) *. scalex -. 0.5 in
        let sx = int_of_float xfrac in
        let xfrac = xfrac -. float sx in

        let rgba = interpol src sx sy xfrac yfrac gamma  in

        set_rgba ~x ~y rgba gamma dst

      done
    done ;
    dst
end
