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
 * Copyright (C) 2014-2024 Rodolphe Lepigre.
 *)

(* A pure OCaml JPEG (JFIF) codec.

   The decoder implements ISO/IEC 10918-1 baseline sequential DCT
   (SOF0/SOF1) and progressive DCT (SOF2), with arbitrary chroma
   subsampling, restart intervals, and 8-bit sample precision.  The
   encoder produces baseline sequential (SOF0) images at 8 bits.

   Not supported: arithmetic coding, lossless modes, 12-bit precision and
   4-component (CMYK/YCCK) images, for which {!Image.Not_yet_implemented}
   is raised.
*)

open Stdlib
open ImageUtil
open Image


(* ------------------------------------------------------------------ *)
(* Constants and tables.                                               *)
(* ------------------------------------------------------------------ *)

(* [zigzag.(k)] is the natural (raster) index of the k-th DCT coefficient
   when coefficients are enumerated in zig-zag order. *)
let zigzag =
  [| 0;  1;  8; 16;  9;  2;  3; 10;
    17; 24; 32; 25; 18; 11;  4;  5;
    12; 19; 26; 33; 40; 48; 41; 34;
    27; 20; 13;  6;  7; 14; 21; 28;
    35; 42; 49; 56; 57; 50; 43; 36;
    29; 22; 15; 23; 30; 37; 44; 51;
    58; 59; 52; 45; 38; 31; 39; 46;
    53; 60; 61; 54; 47; 55; 62; 63 |]

(* [dct_basis.(u * 8 + x)] is the (u, x) entry of the orthogonal DCT-II /
   DCT-III basis matrix [B], with [B.(u).(x) = c(u) * cos ((2x+1) u pi / 16)]
   where [c(0) = sqrt 2 / 4] and [c(u) = 1 / 2] otherwise.  The matrix is
   orthogonal and symmetric, so it serves both the forward (encoder) and the
   inverse (decoder) transform; the two-dimensional transform is obtained by
   applying it separably on rows and columns. *)
let dct_basis =
  Array.init 64 (fun i ->
    let u = i / 8 and x = i mod 8 in
    let c = if u = 0 then 0.25 *. Float.sqrt 2. else 0.5 in
    c *. Float.cos (Float.pi *. float_of_int ((2 * x + 1) * u) /. 16.))

let ceil_div a b = (a + b - 1) / b

let [@inline] clamp lo hi v = if v < lo then lo else if v > hi then hi else v

(* [clamp255] rounds a float to the nearest integer and saturates to the
   8-bit range. *)
let clamp255 (f : float) : int =
  let i = int_of_float (Float.round f) in
  if i < 0 then 0 else if i > 255 then 255 else i


(* ------------------------------------------------------------------ *)
(* Input: a small random-access reader over a [chunk_reader].          *)
(* ------------------------------------------------------------------ *)

(* Markers and entropy-coded data are both consumed one byte at a time.
   [chunk_reader] only guarantees that a request for [n] bytes succeeds if
   exactly [n] bytes are available, so over-requesting would desynchronise a
   channel-backed reader (it closes the channel on a short read).  Byte-at-a-
   time requests are therefore the only safe option here; they run at a
   comfortable multiple of the cost of the IDCT below. *)
type reader = {
  r_ich  : chunk_reader ;
  mutable r_eof : bool ;
  mutable r_buf : string ;
  mutable r_pos : int ;
}

let reader_of_chunk_reader ich = {
  r_ich = ich ; r_eof = false ; r_buf = "" ; r_pos = 0 ;
}

(* [r_byte] returns the next byte, or [-1] at end of input. *)
let r_byte (r : reader) : int =
  if r.r_pos >= String.length r.r_buf then begin
    if r.r_eof then -1
    else begin
      match r.r_ich (`Bytes 1) with
      | Ok s -> r.r_buf <- s ; r.r_pos <- 1 ; Char.code s.[0]
      | Error _ -> r.r_eof <- true ; -1
    end
  end else begin
    let c = Char.code r.r_buf.[r.r_pos] in
    r.r_pos <- r.r_pos + 1 ; c
  end

(* [r_bytes r n] reads exactly [n] bytes; it returns [None] if the input ends
   prematurely. *)
let r_bytes (r : reader) (n : int) : string option =
  if n <= 0 then Some ""
  else begin
    let b = Bytes.create n in
    let rec loop i =
      if i = n then Some (Bytes.unsafe_to_string b)
      else begin
        let c = r_byte r in
        if c < 0 then None
        else begin Bytes.set b i (Char.chr c) ; loop (i + 1) end
      end
    in loop 0
  end

let r_u16 (r : reader) : int option =
  match r_byte r, r_byte r with
  | hi, lo when hi >= 0 && lo >= 0 -> Some ((hi lsl 8) lor lo)
  | _ -> None

(* [r_next_marker] positions the reader at the next marker segment and
   returns its marker byte, skipping padding bits and fill bytes.  This is used
   to resume marker parsing after an entropy-coded segment: the segment is
   terminated by a marker that the bit reader may not have reached yet. *)
let r_next_marker (r : reader) : int =
  let rec loop () =
    let b = r_byte r in
    if b < 0 then -1
    else if b <> 0xff then loop ()
    else
      match r_byte r with
      | c when c < 0 -> -1
      | 0xff -> loop ()                     (* fill byte *)
      | 0x00 -> loop ()                     (* stray stuffed byte *)
      | c -> c
  in
  loop ()

(* [r_marker] consumes a marker: a 0xFF prefix, any number of 0xFF fill
   bytes, then the marker byte.  [-1] is returned at end of input, [-2] when
   the 0xFF prefix is missing. *)
let r_marker (r : reader) : int =
  let b = r_byte r in
  if b <> 0xff then -2
  else begin
    let rec skip_fill () =
      let c = r_byte r in
      if c = 0xff then skip_fill () else c
    in
    skip_fill ()
  end

(* [r_segment] reads the payload of a marker segment: a 16-bit length
   (inclusive of the two length bytes) followed by the payload. *)
let r_segment (r : reader) : string option =
  match r_u16 r with
  | None -> None
  | Some len when len < 2 -> None
  | Some len -> r_bytes r (len - 2)


(* ------------------------------------------------------------------ *)
(* Huffman tables.                                                     *)
(* ------------------------------------------------------------------ *)

(* Canonical Huffman decoding tables, following the procedure of ISO/IEC
   10918-1 Annex C: [h_mincode], [h_maxcode] and [h_valptr] are indexed by
   code length (1 .. 16). *)
type huff = {
  h_mincode : int array ;
  h_maxcode : int array ;
  h_valptr  : int array ;
  h_values  : int array ;
}

(* [counts.(l)] is the number of codes of length [l], for [l = 1 .. 16]. *)
let build_huff (counts : int array) (values : int array) : huff =
  let mincode = Array.make 17 0
  and maxcode = Array.make 17 (-1)
  and valptr  = Array.make 17 0 in
  let total = ref 0 and code = ref 0 and k = ref 0 in
  for l = 1 to 16 do
    let n = counts.(l - 1) in
    if n < 0 || n > 256 then raise (Corrupted_image "Invalid Huffman counts") ;
    total := !total + n ;
    if n > 0 then begin
      (* Codes are assigned in increasing order of length; the codes of length
         [l] must all fit within [l] bits. *)
      valptr.(l) <- !k ;
      mincode.(l) <- !code ;
      code := !code + n ;
      if !code > (1 lsl l) then
        raise (Corrupted_image "Over-subscribed Huffman table") ;
      maxcode.(l) <- !code - 1 ;
      k := !k + n
    end ;
    (* Codes of the next length start where the previous range ends, doubled.
       The per-length test above already rejects over-subscribed tables. *)
    code := !code lsl 1
  done ;
  if !total <> Array.length values then
    raise (Corrupted_image "Inconsistent Huffman table") ;
  { h_mincode = mincode ; h_maxcode = maxcode ;
    h_valptr = valptr ; h_values = values }


(* ------------------------------------------------------------------ *)
(* Bit reader for entropy-coded data.                                  *)
(* ------------------------------------------------------------------ *)

type bitstream = {
  bs_r   : reader ;
  mutable bs_acc  : int ;      (* bit accumulator, valid bits in low end *)
  mutable bs_cnt  : int ;      (* number of valid bits in [bs_acc] *)
  mutable bs_mark : int ;      (* pending marker terminating the scan *)
}

let bitstream_of_reader r = { bs_r = r ; bs_acc = 0 ; bs_cnt = 0 ; bs_mark = 0 }

(* [bs_bit] returns the next bit of the entropy-coded segment, MSB first.
   0xFF bytes are byte-stuffed: a 0xFF followed by 0x00 denotes a literal
   0xFF.  Any other 0xFF prefix terminates the segment; the marker is
   recorded in [bs_mark] and zero bits are supplied in its place. *)
let rec bs_bit (bs : bitstream) : int =
  if bs.bs_cnt = 0 then begin
    let b = bs_byte bs in
    if b >= 0 && b <> 0xff then begin bs.bs_acc <- b ; bs.bs_cnt <- 8 end
    else begin
      (* A stuffed 0xFF, or a marker (recorded by [bs_byte]) padded with
         zero bits, or end of input, which is also padded. *)
      bs.bs_acc <- if b = 0xff then 0xff else 0 ;
      bs.bs_cnt <- 8
    end
  end ;
  let bit = (bs.bs_acc lsr (bs.bs_cnt - 1)) land 1 in
  bs.bs_cnt <- bs.bs_cnt - 1 ;
  bit

(* [bs_byte] reads one byte of entropy-coded data.  A 0xFF followed by 0x00
   is a stuffed literal 0xFF; a 0xFF followed by anything else is the marker
   terminating the scan, which is recorded in [bs_mark] and a filler byte is
   returned.  At end of input a filler byte is returned without a marker. *)
and bs_byte (bs : bitstream) : int =
  let b0 = r_byte bs.bs_r in
  if b0 <> 0xff then b0
  else
    let b1 = r_byte bs.bs_r in
    if b1 = 0x00 then 0xff
    else begin if b1 >= 0 then bs.bs_mark <- b1 ; 0xff end

let bs_bits (bs : bitstream) (n : int) : int =
  if n <= 0 then 0
  else begin
    let v = ref 0 in
    for _ = 1 to n do v := (!v lsl 1) lor bs_bit bs done ;
    !v
  end

(* [bs_align] discards a partially consumed byte, as required at restart
   intervals. *)
let bs_align (bs : bitstream) : unit = bs.bs_cnt <- 0

(* [bs_restart] consumes a restart marker if one is present, and reports
   whether it did.  It must be called on a byte boundary. *)
let bs_restart (bs : bitstream) : bool =
  if bs.bs_mark <> 0 then begin
    let m = bs.bs_mark in
    if m >= 0xd0 && m <= 0xd7 then (bs.bs_mark <- 0 ; true) else false
  end else begin
    let b0 = r_byte bs.bs_r in
    if b0 <> 0xff then false
    else begin
      let b1 = r_byte bs.bs_r in
      if b1 >= 0xd0 && b1 <= 0xd7 then true
      else begin if b1 >= 0 then bs.bs_mark <- b1 ; false end
    end
  end

(* [bs_decode] decodes one Huffman-coded symbol. *)
let bs_decode (bs : bitstream) (h : huff) : int =
  let code = ref (bs_bit bs) and l = ref 1 in
  while !code > h.h_maxcode.(!l) do
    if !l >= 16 then raise (Corrupted_image "Invalid Huffman code") ;
    incr l ;
    code := (!code lsl 1) lor bs_bit bs
  done ;
  let idx = h.h_valptr.(!l) + !code - h.h_mincode.(!l) in
  if idx < 0 || idx >= Array.length h.h_values then
    raise (Corrupted_image "Huffman code out of range") ;
  h.h_values.(idx)

(* [bs_extend] reads [s] additional bits and sign-extends them, as specified
   for DC differences and AC coefficients. *)
let bs_extend (bs : bitstream) (s : int) : int =
  if s <= 0 then 0
  else begin
    let v = bs_bits bs s in
    if v < (1 lsl (s - 1)) then v - (1 lsl s) + 1 else v
  end


(* ------------------------------------------------------------------ *)
(* Frame description.                                                  *)
(* ------------------------------------------------------------------ *)

(* Description of a component as read from a SOF segment. *)
type sof_comp = {
  comp_id : int ;
  comp_h  : int ;
  comp_v  : int ;
  comp_tq : int ;
}

type comp = {
  c_id  : int ;
  c_h   : int ;                       (* horizontal sampling factor *)
  c_v   : int ;                       (* vertical sampling factor *)
  c_tq  : int ;                       (* quantisation table selector *)
  c_dw  : int ;                       (* downsampled width *)
  c_dh  : int ;                       (* downsampled height *)
  c_bw  : int ;                       (* blocks across *)
  c_bh  : int ;                       (* blocks down *)
  mutable c_coef : int array ;        (* [c_bw * c_bh * 64] coefficients *)
}

type frame = {
  f_progressive : bool ;
  f_precision   : int ;
  f_width       : int ;
  f_height      : int ;
  f_max_h       : int ;
  f_max_v       : int ;
  f_comps       : comp array ;
}

(* Per-component state for the duration of one scan. *)
type scan_comp = {
  sc_comp : comp ;
  sc_dc   : huff ;
  sc_ac   : huff option ;   (* absent in a DC-only scan *)
  mutable sc_pred   : int ;   (* DC predictor, reset at restart intervals *)
  mutable sc_eobrun : int ;
}

type ctx = {
  mutable qtables : int array option array ;
  mutable dc_tables : huff option array ;
  mutable ac_tables : huff option array ;
  mutable frame : frame option ;
  mutable restart_interval : int ;
  mutable adobe_transform : int ;
}

(* ------------------------------------------------------------------ *)
(* Segment parsers.                                                    *)
(* ------------------------------------------------------------------ *)

let parse_dqt (ctx : ctx) (b : string) : unit =
  let n = String.length b in
  let p = ref 0 in
  while !p < n do
    let pq = Char.code b.[!p] lsr 4
    and tq = Char.code b.[!p] land 15 in
    incr p ;
    if tq > 3 then raise (Corrupted_image "Invalid quantisation table id") ;
    if pq > 1 then raise (Corrupted_image "Invalid quantisation precision") ;
    let bytes_per = if pq = 0 then 1 else 2 in
    if !p + 64 * bytes_per > n then
      raise (Corrupted_image "Truncated DQT segment") ;
    let q = Array.make 64 0 in
    for k = 0 to 63 do
      let v =
        if pq = 0 then Char.code b.[!p + k]
        else (Char.code b.[!p + 2 * k] lsl 8) lor Char.code b.[!p + 2 * k + 1]
      in
      (* The segment stores values in zig-zag order. *)
      q.(zigzag.(k)) <- v ;
      if v = 0 then raise (Corrupted_image "Zero quantisation value")
    done ;
    p := !p + 64 * bytes_per ;
    ctx.qtables.(tq) <- Some q
  done

let parse_dht (ctx : ctx) (b : string) : unit =
  let n = String.length b in
  let p = ref 0 in
  while !p < n do
    let tc = Char.code b.[!p] lsr 4
    and th = Char.code b.[!p] land 15 in
    incr p ;
    if th > 3 then raise (Corrupted_image "Invalid Huffman table id") ;
    if !p + 16 > n then raise (Corrupted_image "Truncated DHT segment") ;
    let counts = Array.init 16 (fun i -> Char.code b.[!p + i]) in
    p := !p + 16 ;
    let total = Array.fold_left ( + ) 0 counts in
    if total > 256 || !p + total > n then
      raise (Corrupted_image "Truncated DHT segment") ;
    let values = Array.init total (fun i -> Char.code b.[!p + i]) in
    p := !p + total ;
    let tbl = build_huff counts values in
    if tc = 0 then ctx.dc_tables.(th) <- Some tbl
    else ctx.ac_tables.(th) <- Some tbl
  done

let parse_dri (ctx : ctx) (b : string) : unit =
  if String.length b < 2 then raise (Corrupted_image "Truncated DRI segment") ;
  ctx.restart_interval <-
    (Char.code b.[0] lsl 8) lor Char.code b.[1]

(* [parse_app14] reads an Adobe APP14 marker: it declares the colour
   transform of 3- and 4-component images. *)
let parse_app14 (ctx : ctx) (b : string) : unit =
  if String.length b >= 12 &&
     b.[0] = 'A' && b.[1] = 'd' && b.[2] = 'o' && b.[3] = 'b' && b.[4] = 'e'
  then ctx.adobe_transform <- Char.code b.[11]

let parse_sof ~(progressive : bool) (b : string) : frame =
  if String.length b < 6 then raise (Corrupted_image "Truncated SOF segment") ;
  let precision = Char.code b.[0] in
  let height = (Char.code b.[1] lsl 8) lor Char.code b.[2]
  and width = (Char.code b.[3] lsl 8) lor Char.code b.[4]
  and ncomp = Char.code b.[5] in
  if precision <> 8 then
    raise (Not_yet_implemented
             (Printf.sprintf "JPEG sample precision %d" precision)) ;
  if width = 0 || height = 0 then
    raise (Corrupted_image "Zero-sized JPEG image") ;
  if ncomp < 1 || ncomp > 4 then
    raise (Corrupted_image "Unsupported number of JPEG components") ;
  if ncomp = 4 then
    raise (Not_yet_implemented "4-component (CMYK/YCCK) JPEG") ;
  if String.length b < 6 + 3 * ncomp then
    raise (Corrupted_image "Truncated SOF segment") ;
  let tmp =
    Array.init ncomp (fun i ->
      let o = 6 + 3 * i in
      let id = Char.code b.[o]
      and sampling = Char.code b.[o + 1]
      and tq = Char.code b.[o + 2] in
      (* The two sampling factors are packed in one byte, high nibble first. *)
      let h = sampling lsr 4
      and v = sampling land 15 in
      if h < 1 || h > 4 || v < 1 || v > 4 then
        raise (Corrupted_image "Invalid sampling factor") ;
      if tq > 3 then
        raise (Corrupted_image "Invalid quantisation table selector") ;
      { comp_id = id ; comp_h = h ; comp_v = v ; comp_tq = tq })
  in
  let ids = Array.map (fun c -> c.comp_id) tmp in
  for i = 0 to ncomp - 1 do
    for j = i + 1 to ncomp - 1 do
      if ids.(i) = ids.(j) then
        raise (Corrupted_image "Duplicate component id")
    done
  done ;
  let max_h = Array.fold_left (fun acc c -> max acc c.comp_h) 1 tmp
  and max_v = Array.fold_left (fun acc c -> max acc c.comp_v) 1 tmp in
  let comps =
    Array.map (fun c ->
      let id = c.comp_id and h = c.comp_h and v = c.comp_v and tq = c.comp_tq in
      (* Number of blocks needed to cover the component's samples, per the
         sampling factors. *)
      let bw = ceil_div (width * h) (max_h * 8)
      and bh = ceil_div (height * v) (max_v * 8) in
      { c_id = id ; c_h = h ; c_v = v ; c_tq = tq ;
        c_dw = ceil_div (width * h) max_h ;
        c_dh = ceil_div (height * v) max_v ;
        c_bw = bw ; c_bh = bh ;
        c_coef = Array.make (bw * bh * 64) 0 })
      tmp
  in
  (* Guard against absurd allocations from a hostile header. *)
  let nblocks = ref 0 in
  Array.iter (fun c -> nblocks := !nblocks + c.c_bw * c.c_bh) comps ;
  if !nblocks > 1 lsl 24 then
    raise (Corrupted_image "Implausibly large JPEG image") ;
  { f_progressive = progressive ; f_precision = precision ;
    f_width = width ; f_height = height ;
    f_max_h = max_h ; f_max_v = max_v ; f_comps = comps }



(* ------------------------------------------------------------------ *)
(* Scan decoding.                                                      *)
(* ------------------------------------------------------------------ *)

let find_comp (fr : frame) (id : int) : comp =
  let found = ref None in
  Array.iter (fun c -> if c.c_id = id then found := Some c) fr.f_comps ;
  match !found with
  | Some c -> c
  | None -> raise (Corrupted_image "Scan refers to unknown component")

let get_table (a : 'a option array) (i : int) (what : string) : 'a =
  if i < 0 || i > 3 then raise (Corrupted_image "Invalid table selector") ;
  match a.(i) with
  | Some v -> v
  | None -> raise (Corrupted_image (what ^ " table not defined"))

(* [decode_block] decodes one 8x8 block of the scan [bs].  The block lives in
   [blk] starting at [off]; passing a scratch array with [off = 0] decodes
   into that scratch.  [ss], [se], [ah] and [al] are the successive
   approximation parameters of the scan, already validated by
   {!decode_scan}: [ah = al = 0] with [ss = 0] and [se = 63] is the baseline
   sequential case. *)
let decode_block (bs : bitstream) (sc : scan_comp) (blk : int array) (off : int)
    ~(ss : int) ~(se : int) ~(ah : int) ~(al : int) : unit =
  (* The DC coefficient is only present when the band starts at it; a scan with
     [ss > 0] codes no DC symbol at all.  It is either a coded difference from
     the predictor or, in a DC refinement scan, a single correction bit. *)
  if ss = 0 then begin
    if ah = 0 then begin
      let cat = bs_decode bs sc.sc_dc in
      let diff = bs_extend bs cat in
      let v = sc.sc_pred + (diff lsl al) in
      sc.sc_pred <- v ;
      blk.(off) <- v
    end else begin
      (* DC refinement: the coded bit is simply the next bit of the two's
         complement DC value, so it is merged in place; no sign test is
         involved (unlike the AC refinement, which corrects a magnitude). *)
      if bs_bit bs = 1 then blk.(off) <- blk.(off) lor (1 lsl al)
    end
  end ;
  (* A band ending at coefficient 0 contains only the DC, which is coded above;
     its AC table selector is not significant and must not be looked up. *)
  if se = 0 then ()
  else begin
    (* Coefficient 0 is the DC, which is coded separately above, so the AC
       procedure always starts at index 1 even when [ss] is 0. *)
    let k0 = if ss = 0 then 1 else ss in
    if ah = 0 then begin
      (* First AC pass, used both by baseline sequential coding and by the
         first progressive pass over the band.  End of band is coded as a
         run. *)
      if sc.sc_eobrun > 0 then sc.sc_eobrun <- sc.sc_eobrun - 1
      else begin
        let ac = match sc.sc_ac with
          | Some a -> a
          | None -> raise (Corrupted_image "AC Huffman table not defined")
        in
        let k = ref k0 and fin = ref false in
      while (not !fin) && !k <= se do
        let rs = bs_decode bs ac in
        let s = rs land 15 and r = rs lsr 4 in
        if s = 0 then begin
          if r < 15 then begin
            sc.sc_eobrun <- (1 lsl r) - 1 ;
            if r > 0 then sc.sc_eobrun <- sc.sc_eobrun + bs_bits bs r ;
            fin := true
          end else k := !k + 16  (* run of sixteen zeroes *)
        end else begin
          k := !k + r ;
          if !k > 63 then
            raise (Corrupted_image "AC coefficient index out of range") ;
          blk.(off + zigzag.(!k)) <- bs_extend bs s lsl al ;
          incr k
        end
      done
    end
  end else begin
    (* AC refinement pass: the magnitude of coefficients already made nonzero
       by an earlier scan is corrected one bit at a time, and the scan may
       introduce new nonzero coefficients. *)
    let p1 = 1 lsl al in
    let m1 = (-1) lsl al in
    let refine idx =
      if blk.(off + idx) <> 0 && bs_bit bs = 1 then
        blk.(off + idx) <-
          if blk.(off + idx) >= 0
          then blk.(off + idx) + p1 else blk.(off + idx) + m1
    in
    let k = ref k0 in
    if sc.sc_eobrun <= 0 then begin
      let ac = match sc.sc_ac with
        | Some a -> a
        | None -> raise (Corrupted_image "AC Huffman table not defined")
      in
      let fin = ref false in
      while (not !fin) && !k <= se do
        let rs = bs_decode bs ac in
        let s = rs land 15 and r = rs lsr 4 in
        (* [news] is the value of a coefficient this scan makes nonzero, or 0. *)
        let news = ref 0 in
        if s <> 0 then
          news := (if bs_bit bs = 1 then p1 else m1)
        else if r <> 15 then begin
          sc.sc_eobrun <- 1 lsl r ;
          if r > 0 then sc.sc_eobrun <- sc.sc_eobrun + bs_bits bs r ;
          fin := true
        end ;
        if not !fin then begin
          (* Advance over the coefficients that a previous scan already made
             nonzero, emitting a correction bit for each, then over the [r]
             zero coefficients that follow them. *)
          let rr = ref r and cont = ref true in
          while !cont && !k <= se do
            let idx = zigzag.(!k) in
            if blk.(off + idx) <> 0 then (refine idx ; incr k)
            else begin
              decr rr ;
              if !rr >= 0 then incr k else cont := false
            end
          done ;
          (* The new nonzero coefficient, if any, lands at the current
             position. *)
          if !news <> 0 && !k <= 63 then
            blk.(off + zigzag.(!k)) <- !news ;
          incr k
        end
      done
    end ;
    if sc.sc_eobrun > 0 then begin
      (* Every remaining coefficient of the band takes a correction bit. *)
      while !k <= se do
        refine zigzag.(!k) ;
        incr k
      done ;
      sc.sc_eobrun <- sc.sc_eobrun - 1
    end
  end
  end

(* [decode_scan] decodes one scan, accumulating coefficients into the
   component buffers, and returns the bitstream so that the caller can resume
   marker parsing. *)
let decode_scan (ctx : ctx) (fr : frame) (b : string) (rd : reader) :
    bitstream =
  if String.length b < 1 then raise (Corrupted_image "Truncated SOS segment") ;
  let ns = Char.code b.[0] in
  if ns < 1 || ns > Array.length fr.f_comps then
    raise (Corrupted_image "Invalid number of scan components") ;
  if String.length b < 1 + 2 * ns + 3 then
    raise (Corrupted_image "Truncated SOS segment") ;
  let ss = Char.code b.[1 + 2 * ns]
  and se = Char.code b.[2 + 2 * ns]
  and ahal = Char.code b.[3 + 2 * ns] in
  let ah = ahal lsr 4 and al = ahal land 15 in
  if ss < 0 || ss > 63 || se < ss || se > 63 || ah < 0 || al < 0
     || ah > 13 || al > 13
  then raise (Corrupted_image "Invalid scan spectral selection") ;
  if (not fr.f_progressive) && (ah <> 0 || al <> 0 || ss <> 0 || se <> 63) then
    raise (Corrupted_image
            "Invalid scan parameters for a sequential frame") ;
  (* A scan covering only the DC coefficient ([se = 0]) does not use the AC
     Huffman table, whose selector is then not significant. *)
  let specs =
    Array.init ns (fun i ->
      let cs = Char.code b.[1 + 2 * i] in
      let td = Char.code b.[2 + 2 * i] lsr 4
      and ta = Char.code b.[2 + 2 * i] land 15 in
      let c = find_comp fr cs in
      { sc_comp = c ;
        sc_dc = get_table ctx.dc_tables td "DC Huffman" ;
        sc_ac = (if se > 0
                then Some (get_table ctx.ac_tables ta "AC Huffman")
                else None) ;
        sc_pred = 0 ; sc_eobrun = 0 })
  in
  let nsc = Array.length specs in
  (* In a non-interleaved scan an MCU is a single block; in an interleaved
     scan it holds one block per sampling unit of every component. *)
  let eh sc = if nsc = 1 then 1 else sc.sc_comp.c_h
  and ev sc = if nsc = 1 then 1 else sc.sc_comp.c_v in
  let mcu_w = ref 1 and mcu_h = ref 1 in
  Array.iter (fun sc ->
    mcu_w := max !mcu_w (ceil_div sc.sc_comp.c_bw (eh sc)) ;
    mcu_h := max !mcu_h (ceil_div sc.sc_comp.c_bh (ev sc))) specs ;
  let bs = bitstream_of_reader rd in
  (* Blocks that fall outside the component's own block grid (along the
     right and bottom edges) are decoded for entropy-coding synchronisation
     but discarded. *)
  let scratch = Array.make 64 0 in
  let ri = ctx.restart_interval in
  let counter = ref 0 in
  for my = 0 to !mcu_h - 1 do
    for mx = 0 to !mcu_w - 1 do
      if ri > 0 && !counter = ri then begin
        bs_align bs ;
        ignore (bs_restart bs) ; (* a missing restart marker is tolerated *)
        Array.iter (fun sc -> sc.sc_pred <- 0) specs ;
        counter := 0
      end ;
      Array.iter (fun sc ->
        let c = sc.sc_comp in
        for v = 0 to ev sc - 1 do
          for h = 0 to eh sc - 1 do
            let bx = (mx * eh sc) + h and by = (my * ev sc) + v in
            let inside = bx < c.c_bw && by < c.c_bh in
            let blk, off =
              if inside then (c.c_coef, ((by * c.c_bw) + bx) * 64)
              else (scratch, 0)
            in
            if not inside then Array.fill scratch 0 64 0 ;
            decode_block bs sc blk off ~ss ~se ~ah ~al
          done
        done
      ) specs ;
      incr counter
    done
  done ;
  bs


(* ------------------------------------------------------------------ *)
(* Inverse DCT and image reconstruction.                               *)
(* ------------------------------------------------------------------ *)

(* [idct_into coef scratch out] computes the 8x8 inverse DCT of the 64
   coefficients [coef] (natural order, already dequantised), applies the
   level shift and writes 64 saturated 8-bit samples into [out] (natural
   order).  [scratch] is a 64-element float array reused between calls.
   Writing into [coef] is not supported, so the same routine serves the
   decoder. *)
let idct_into (coef : int array) (scratch : float array) (out : int array) :
    unit =
  (* A coefficient is addressed by its position [n] in the natural (raster)
     order of the block, where [n = v * 8 + u]: the row index is the vertical
     frequency and the column index the horizontal one. *)
  (* Pass 1: for each horizontal frequency, combine the vertical ones. *)
  for u = 0 to 7 do
    for y = 0 to 7 do
      let s = ref 0.0 in
      for v = 0 to 7 do
        s := !s +. (float_of_int coef.((v lsl 3) + u) *. dct_basis.((v lsl 3) + y))
      done ;
      scratch.((u lsl 3) + y) <- !s
    done
  done ;
  (* Pass 2: for each sample column, combine the horizontal frequencies and
     undo the level shift. *)
  for x = 0 to 7 do
    for y = 0 to 7 do
      let s = ref 0.0 in
      for u = 0 to 7 do
        s := !s +. scratch.((u lsl 3) + y) *. dct_basis.((u lsl 3) + x)
      done ;
      out.((y lsl 3) + x) <- clamp255 (!s +. 128.0)
    done
  done

(* [comp_plane] dequantises and inverse transforms every block of a component,
   producing its samples at the component's own resolution. *)
let comp_plane (ctx : ctx) (c : comp) : int array =
  let qt =
    get_table ctx.qtables c.c_tq "Quantisation"
  in
  let plane = Array.make (c.c_dw * c.c_dh) 0 in
  let coef = Array.make 64 0
  and scratch = Array.make 64 0.0
  and block = Array.make 64 0 in
  for by = 0 to c.c_bh - 1 do
    for bx = 0 to c.c_bw - 1 do
      let base = ((by * c.c_bw) + bx) * 64 in
      for i = 0 to 63 do coef.(i) <- c.c_coef.(base + i) * qt.(i) done ;
      idct_into coef scratch block ;
      (* Store only the samples that exist; the rest of the plane is padding
         used by the upsampler for chroma replication. *)
      for y = 0 to 7 do
        let py = (by * 8) + y in
        if py < c.c_dh then
          for x = 0 to 7 do
            let px = (bx * 8) + x in
            if px < c.c_dw then plane.((py * c.c_dw) + px) <- block.(y * 8 + x)
          done
      done
    done
  done ;
  plane

(* [upsample] expands a component plane from its own resolution to the full
   image resolution.  Replication factors [rw] and [rh] are the ratios between
   the frame's maximum sampling factor and the component's, i.e. 1, 2 or 4.
   A factor of 2 is handled with the triangle ("fancy") filter used by
   libjpeg; other factors replicate.  The filter is separable, so each axis is
   expanded in turn. *)
let upsample ~(rw : int) ~(rh : int) ~(src : int array) ~(sw : int) ~(sh : int) :
    int array =
  (* At 1:1 the mapping is the identity, so hand the plane back rather than
     making two full-size copies of it.  This is the common case: a 4:4:4
     image upsamples nothing, and it dominated decoding otherwise. *)
  if rw = 1 && rh = 1 then src
  else begin
  let dw = sw * rw and dh = sh * rh in
  let get sx sy = src.(clamp 0 (sw - 1) sx + (clamp 0 (sh - 1) sy * sw)) in
  (* Horizontal expansion. *)
  let tmp = Array.make (dw * sh) 0 in
  for sy = 0 to sh - 1 do
    let doff = sy * dw in
    if rw = 1 then
      for x = 0 to sw - 1 do tmp.(doff + x) <- get x sy done
    else if rw = 2 then
      for x = 0 to dw - 1 do
        (* Triangle filter anchored on source sample [x / 2]: the even
           output samples blend it with the preceding one, the odd ones with
           the following one. *)
        let i = x / 2 in
        let n = if x land 1 = 0 then i - 1 else i + 1 in
        tmp.(doff + x) <- ((3 * get i sy) + get n sy + 2) / 4
      done
    else
      for x = 0 to dw - 1 do tmp.(doff + x) <- get (x / rw) sy done
  done ;
  (* Vertical expansion. *)
  let dst = Array.make (dw * dh) 0 in
  for y = 0 to dh - 1 do
    let doff = y * dw in
    if rh = 1 then
      Array.blit tmp (y * dw) dst doff dw
    else if rh = 2 then
      let i = clamp 0 (sh - 1) (y / 2) in
      let n = clamp 0 (sh - 1) (if y land 1 = 0 then i - 1 else i + 1) in
      for x = 0 to dw - 1 do
        dst.(doff + x) <- ((3 * tmp.((i * dw) + x)) + tmp.((n * dw) + x) + 2) / 4
      done
    else
      Array.blit tmp ((y / rh) * dw) dst doff dw
  done ;
  dst
  end

(* [build_image] turns the decoded frame into an {!Image.image}. *)
let build_image (ctx : ctx) (fr : frame) : image =
  let w = fr.f_width and h = fr.f_height in
  let ncomp = Array.length fr.f_comps in
  (* Upsampled components, each with its own row stride: expanding a subsampled
     component yields [c_dw * replication] samples per row, which is generally
     wider than the image. *)
  let planes =
    Array.map (fun c ->
      let p = comp_plane ctx c in
      let rw = fr.f_max_h / c.c_h and rh = fr.f_max_v / c.c_v in
      (rw * c.c_dw, upsample ~rw ~rh ~src:p ~sw:c.c_dw ~sh:c.c_dh))
      fr.f_comps
  in
  if ncomp = 1 || ncomp = 3 then begin
    Array.iter (fun (stride, p) ->
      if stride < w || Array.length p < stride * h then
        raise (Corrupted_image "Inconsistent component dimensions")) planes
  end ;
  match ncomp with
  | 1 ->
    let img = Image.create_grey w h in
    let stride, p = planes.(0) in
    for y = 0 to h - 1 do
      for x = 0 to w - 1 do
        Image.write_grey img x y p.((y * stride) + x)
      done
    done ;
    img
  | 3 ->
    let img = Image.create_rgb w h in
    let s0, u0 = planes.(0)
    and s1, u1 = planes.(1)
    and s2, u2 = planes.(2) in
    (* An Adobe APP14 marker with transform 0 declares that the three
       components are already R, G and B; otherwise they are Y, Cb and Cr. *)
    let rgb = (ctx.adobe_transform = 0) in
    for y = 0 to h - 1 do
      for x = 0 to w - 1 do
        let a = u0.((y * s0) + x)
        and b = u1.((y * s1) + x)
        and c = u2.((y * s2) + x) in
        if rgb then Image.write_rgb img x y a b c
        else begin
          let cb = float_of_int (b - 128)
          and cr = float_of_int (c - 128)
          and yy = float_of_int a in
          Image.write_rgb img x y
            (clamp255 (yy +. (1.402 *. cr)))
            (clamp255 (yy -. (0.34414 *. cb) -. (0.71414 *. cr)))
            (clamp255 (yy +. (1.772 *. cb)))
        end
      done
    done ;
    img
  | _ -> assert false


(* ------------------------------------------------------------------ *)
(* Marker-level parsing.                                               *)
(* ------------------------------------------------------------------ *)

let parse_jpeg (rd : reader) : image =
  let ctx = {
    qtables = [| None ; None ; None ; None |] ;
    dc_tables = [| None ; None ; None ; None |] ;
    ac_tables = [| None ; None ; None ; None |] ;
    frame = None ;
    restart_interval = 0 ;
    (* Absent Adobe marker: assume the usual YCbCr transform. *)
    adobe_transform = 1 ;
  } in
  let pending = ref 0 in
  let soi = r_marker rd in
  if soi <> 0xd8 then raise (Corrupted_image "JPEG does not start with SOI") ;
  let fin = ref false in
  while not !fin do
    let m =
      if !pending <> 0 then (let p = !pending in pending := 0 ; p)
      else r_marker rd
    in
    if m < 0 then fin := true
    else begin
      match m with
      | 0xd9 -> fin := true                            (* EOI *)
      | 0xd8 -> ()                                     (* stray SOI *)
      | 0xda -> (                                       (* SOS *)
        match ctx.frame with
        | None -> raise (Corrupted_image "SOS before SOF")
        | Some fr ->
          (match r_segment rd with
           | None -> raise (Corrupted_image "Truncated SOS segment")
           | Some seg ->
             let bs = decode_scan ctx fr seg rd in
             (* If the bit reader never reached the marker terminating the
                scan, look for it now; otherwise it has already consumed the
                prefix and the marker byte. *)
             pending := (if bs.bs_mark <> 0 then bs.bs_mark else r_next_marker rd)))
      | 0xdb ->                                        (* DQT *)
        (match r_segment rd with
         | None -> raise (Corrupted_image "Truncated DQT segment")
         | Some seg -> parse_dqt ctx seg)
      | 0xc4 ->                                        (* DHT *)
        (match r_segment rd with
         | None -> raise (Corrupted_image "Truncated DHT segment")
         | Some seg -> parse_dht ctx seg)
      | 0xdd ->                                        (* DRI *)
        (match r_segment rd with
         | None -> raise (Corrupted_image "Truncated DRI segment")
         | Some seg -> parse_dri ctx seg)
      | 0xc0 | 0xc1 | 0xc2 ->                          (* SOF0/1/2 *)
        (match r_segment rd with
         | None -> raise (Corrupted_image "Truncated SOF segment")
         | Some seg ->
           ctx.frame <- Some (parse_sof ~progressive:(m = 0xc2) seg))
      | 0xc3 | 0xc5 | 0xc6 | 0xc7 | 0xc9 | 0xca | 0xcb
      | 0xcd | 0xce | 0xcf ->                          (* unsupported SOF/DAC *)
        raise (Not_yet_implemented
                 (Printf.sprintf "JPEG marker 0x%02x" m))
      | m when m >= 0xd0 && m <= 0xd7 -> ()                (* stray restart *)
      | 0xff -> ()                                     (* fill byte *)
      | _ ->                                           (* APPn, COM, ... *)
        if m >= 0xe0 && m <= 0xef then
          (match r_segment rd with
           | None -> fin := true
           | Some seg -> if m = 0xee then parse_app14 ctx seg)
        else
          match r_segment rd with
          | None -> fin := true
          | Some _ -> ()
    end
  done ;
  match ctx.frame with
  | None -> raise (Corrupted_image "JPEG contains no frame")
  | Some fr -> build_image ctx fr


(* ------------------------------------------------------------------ *)
(* Public decoder API.                                                 *)
(* ------------------------------------------------------------------ *)

module ReadJPG : ReadImage = struct
  let extensions = [".jpg"; ".jpeg"; ".jpe"; ".jif"; ".jfif"; ".jfi"]

  (* Read the size of a JPEG image without decoding the entropy-coded data.
   * Arguments:
   *   - ich : the input chunk_reader.
   * Returns a couple (width, height).
   * Note: the image is not checked for inconsistency; only the signature and
   * the frame header are read.  This shares the marker reader with
   * {!parsefile}, so a truncated or malformed file is reported rather than
   * raising an arbitrary exception from the underlying reader.
   *)
  let size ich =
    let rd = reader_of_chunk_reader ich in
    if r_marker rd <> 0xd8 then
      raise (Corrupted_image "First marker should be SOI...") ;
    let result = ref None in
    while !result = None do
      let m = r_marker rd in
      (* 0xc4 is DHT and 0xcc is DAC; every other marker in this range is a
         frame header. *)
      if m >= 0xc0 && m <= 0xcf && m <> 0xc4 && m <> 0xcc then begin
        match r_segment rd with
        | Some sof when String.length sof >= 5 ->
          let height = (Char.code sof.[1] lsl 8) lor Char.code sof.[2]
          and width = (Char.code sof.[3] lsl 8) lor Char.code sof.[4] in
          result := Some (width, height)
        | _ -> raise (Corrupted_image "Truncated SOF segment")
      end else if m < 0 || m = 0xda then
        (* End of input, or a scan header with no preceding frame header. *)
        raise (Corrupted_image "No SOFn marker...")
      else
        match r_segment rd with
        | None -> raise (Corrupted_image "No SOFn marker...")
        | Some _ -> ()                  (* skip this segment and carry on *)
    done ;
    close_chunk_reader ich ;
    match !result with
    | Some wh -> wh
    | None -> assert false

  let parsefile ich =
    let rd = reader_of_chunk_reader ich in
    let img = parse_jpeg rd in
    close_chunk_reader ich ;
    img
end

(* ------------------------------------------------------------------ *)
(* Forward DCT and encoding.                                           *)
(* ------------------------------------------------------------------ *)

(* [fdct_into samples scratch out qt] computes the forward DCT of the 64
   samples [samples] (natural order, indexed [y * 8 + x]) and writes the
   quantised coefficients into [out], also in natural order.  The level shift
   (subtracting 128) is applied here. *)
let fdct_into (samples : int array) (scratch : float array) (out : int array)
    (qt : int array) : unit =
  (* Pass 1: for each sample column, combine the vertical frequencies. *)
  for x = 0 to 7 do
    for v = 0 to 7 do
      let s = ref 0.0 in
      for y = 0 to 7 do
        s := !s
             +. (float_of_int (samples.((y lsl 3) + x) - 128)
                *. dct_basis.((v lsl 3) + y))
      done ;
      scratch.((v lsl 3) + x) <- !s
    done
  done ;
  (* Pass 2: for each coefficient row, combine the horizontal frequencies and
     quantise. *)
  for v = 0 to 7 do
    for u = 0 to 7 do
      let s = ref 0.0 in
      for x = 0 to 7 do
        s := !s +. (scratch.((v lsl 3) + x) *. dct_basis.((u lsl 3) + x))
      done ;
      let q = float_of_int qt.((v lsl 3) + u) in
      out.((v lsl 3) + u) <- int_of_float (Float.round (!s /. q))
    done
  done

(* The example quantisation tables and Huffman tables of ISO/IEC 10918-1
   Annex K, which a baseline encoder uses unless asked to optimise for size. *)

(* Annex K.1, in natural order. *)
let std_luma_quant =
  [| 16; 11; 10; 16;  24;  40;  51;  61;
     12; 12; 14; 19;  26;  58;  60;  55;
     14; 13; 16; 24;  40;  57;  69;  56;
     14; 17; 22; 29;  51;  87;  80;  62;
     18; 22; 37; 56;  68; 109; 103;  77;
     24; 35; 55; 64;  81; 104; 113;  92;
     49; 64; 78; 87; 103; 121; 120; 101;
     72; 92; 95; 98; 112; 100; 103;  99 |]

(* Annex K.2, in natural order. *)
let std_chroma_quant =
  [| 17; 18; 24; 47; 99; 99; 99; 99;
     18; 21; 26; 66; 99; 99; 99; 99;
     24; 26; 56; 99; 99; 99; 99; 99;
     47; 66; 99; 99; 99; 99; 99; 99;
     99; 99; 99; 99; 99; 99; 99; 99;
     99; 99; 99; 99; 99; 99; 99; 99;
     99; 99; 99; 99; 99; 99; 99; 99;
     99; 99; 99; 99; 99; 99; 99; 99 |]

(* Scales an Annex K table to a quality setting the way libjpeg does: the
   factor is 5000 / quality below quality 50 and 200 - 2 * quality above. *)
let scale_quant_table ~(quality : int) (base : int array) : int array =
  let q = clamp 1 100 quality in
  let factor = if q < 50 then 5000 / q else 200 - (2 * q) in
  Array.map (fun v -> clamp 1 255 (((v * factor) + 50) / 100)) base

let dc_luma_counts = [| 0; 1; 5; 1; 1; 1; 1; 1; 1; 0; 0; 0; 0; 0; 0; 0 |]
let dc_luma_values = Array.init 12 (fun i -> i)
let dc_chroma_counts = [| 0; 3; 1; 1; 1; 1; 1; 1; 1; 1; 1; 0; 0; 0; 0; 0 |]
let dc_chroma_values = Array.init 12 (fun i -> i)

let ac_luma_counts = [| 0; 2; 1; 3; 3; 2; 4; 3; 5; 5; 4; 4; 0; 0; 1; 125 |]
let ac_luma_values =
  [| 0x01; 0x02; 0x03; 0x00; 0x04; 0x11; 0x05; 0x12;
     0x21; 0x31; 0x41; 0x06; 0x13; 0x51; 0x61; 0x07;
     0x22; 0x71; 0x14; 0x32; 0x81; 0x91; 0xa1; 0x08;
     0x23; 0x42; 0xb1; 0xc1; 0x15; 0x52; 0xd1; 0xf0;
     0x24; 0x33; 0x62; 0x72; 0x82; 0x09; 0x0a; 0x16;
     0x17; 0x18; 0x19; 0x1a; 0x25; 0x26; 0x27; 0x28;
     0x29; 0x2a; 0x34; 0x35; 0x36; 0x37; 0x38; 0x39;
     0x3a; 0x43; 0x44; 0x45; 0x46; 0x47; 0x48; 0x49;
     0x4a; 0x53; 0x54; 0x55; 0x56; 0x57; 0x58; 0x59;
     0x5a; 0x63; 0x64; 0x65; 0x66; 0x67; 0x68; 0x69;
     0x6a; 0x73; 0x74; 0x75; 0x76; 0x77; 0x78; 0x79;
     0x7a; 0x83; 0x84; 0x85; 0x86; 0x87; 0x88; 0x89;
     0x8a; 0x92; 0x93; 0x94; 0x95; 0x96; 0x97; 0x98;
     0x99; 0x9a; 0xa2; 0xa3; 0xa4; 0xa5; 0xa6; 0xa7;
     0xa8; 0xa9; 0xaa; 0xb2; 0xb3; 0xb4; 0xb5; 0xb6;
     0xb7; 0xb8; 0xb9; 0xba; 0xc2; 0xc3; 0xc4; 0xc5;
     0xc6; 0xc7; 0xc8; 0xc9; 0xca; 0xd2; 0xd3; 0xd4;
     0xd5; 0xd6; 0xd7; 0xd8; 0xd9; 0xda; 0xe1; 0xe2;
     0xe3; 0xe4; 0xe5; 0xe6; 0xe7; 0xe8; 0xe9; 0xea;
     0xf1; 0xf2; 0xf3; 0xf4; 0xf5; 0xf6; 0xf7; 0xf8;
     0xf9; 0xfa |]

let ac_chroma_counts = [| 0; 2; 1; 2; 4; 4; 3; 4; 7; 5; 4; 4; 0; 1; 2; 119 |]
let ac_chroma_values =
  [| 0x00; 0x01; 0x02; 0x03; 0x11; 0x04; 0x05; 0x21;
     0x31; 0x06; 0x12; 0x41; 0x51; 0x07; 0x61; 0x71;
     0x13; 0x22; 0x32; 0x81; 0x08; 0x14; 0x42; 0x91;
     0xa1; 0xb1; 0xc1; 0x09; 0x23; 0x33; 0x52; 0xf0;
     0x15; 0x62; 0x72; 0xd1; 0x0a; 0x16; 0x24; 0x34;
     0xe1; 0x25; 0xf1; 0x17; 0x18; 0x19; 0x1a; 0x26;
     0x27; 0x28; 0x29; 0x2a; 0x35; 0x36; 0x37; 0x38;
     0x39; 0x3a; 0x43; 0x44; 0x45; 0x46; 0x47; 0x48;
     0x49; 0x4a; 0x53; 0x54; 0x55; 0x56; 0x57; 0x58;
     0x59; 0x5a; 0x63; 0x64; 0x65; 0x66; 0x67; 0x68;
     0x69; 0x6a; 0x73; 0x74; 0x75; 0x76; 0x77; 0x78;
     0x79; 0x7a; 0x82; 0x83; 0x84; 0x85; 0x86; 0x87;
     0x88; 0x89; 0x8a; 0x92; 0x93; 0x94; 0x95; 0x96;
     0x97; 0x98; 0x99; 0x9a; 0xa2; 0xa3; 0xa4; 0xa5;
     0xa6; 0xa7; 0xa8; 0xa9; 0xaa; 0xb2; 0xb3; 0xb4;
     0xb5; 0xb6; 0xb7; 0xb8; 0xb9; 0xba; 0xc2; 0xc3;
     0xc4; 0xc5; 0xc6; 0xc7; 0xc8; 0xc9; 0xca; 0xd2;
     0xd3; 0xd4; 0xd5; 0xd6; 0xd7; 0xd8; 0xd9; 0xda;
     0xe2; 0xe3; 0xe4; 0xe5; 0xe6; 0xe7; 0xe8; 0xe9;
     0xea; 0xf2; 0xf3; 0xf4; 0xf5; 0xf6; 0xf7; 0xf8;
     0xf9; 0xfa |]

(* Encoding tables.  The canonical codes are assigned in the same order as in
   {!build_huff}, so that any conformant decoder reads back what we write. *)
type enc_tbl = { e_code : int array ; e_len : int array }

let build_enc_tbl (counts : int array) (values : int array) : enc_tbl =
  let e_code = Array.make 256 0 and e_len = Array.make 256 0 in
  let code = ref 0 and k = ref 0 in
  for l = 1 to 16 do
    for _ = 1 to counts.(l - 1) do
      e_code.(values.(!k)) <- !code ;
      e_len.(values.(!k)) <- l ;
      incr k ; incr code
    done ;
    code := !code lsl 1
  done ;
  { e_code ; e_len }

(* Bit writer for entropy-coded data; 0xFF bytes are followed by a stuffed 0. *)
type bitw = {
  b_buf : Buffer.t ;
  mutable b_acc : int ;
  mutable b_cnt : int ;
}

let bw_create () = { b_buf = Buffer.create 4096 ; b_acc = 0 ; b_cnt = 0 }

let bw_put (w : bitw) (v : int) (n : int) : unit =
  if n > 0 then begin
    w.b_acc <- (w.b_acc lsl n) lor (v land ((1 lsl n) - 1)) ;
    w.b_cnt <- w.b_cnt + n ;
    while w.b_cnt >= 8 do
      let byte = (w.b_acc lsr (w.b_cnt - 8)) land 0xff in
      Buffer.add_char w.b_buf (Char.chr byte) ;
      if byte = 0xff then Buffer.add_char w.b_buf '\000' ;
      w.b_cnt <- w.b_cnt - 8
    done ;
    w.b_acc <- w.b_acc land ((1 lsl w.b_cnt) - 1)
  end

let bw_emit (w : bitw) (t : enc_tbl) (sym : int) : unit =
  bw_put w t.e_code.(sym) t.e_len.(sym)

(* Pads the last byte with one bits, as the standard requires. *)
let bw_flush (w : bitw) : unit =
  if w.b_cnt > 0 then bw_put w ((1 lsl (8 - w.b_cnt)) - 1) (8 - w.b_cnt)

(* Number of bits needed to hold the magnitude of [v]. *)
let num_bits v =
  let a = if v < 0 then -v else v in
  let n = ref 0 and x = ref a in
  while !x > 0 do incr n ; x := !x lsr 1 done ;
  !n

(* [encode_block] writes one block of quantised coefficients.  [pred] is the
   DC predictor of the current component. *)
let encode_block (w : bitw) (dc : enc_tbl) (ac : enc_tbl) (pred : int ref)
    (blk : int array) : unit =
  (* DC: the difference from the predictor, category then bits. *)
  let diff = blk.(0) - !pred in
  pred := blk.(0) ;
  let s = num_bits diff in
  bw_emit w dc s ;
  if s > 0 then bw_put w (if diff < 0 then diff + (1 lsl s) - 1 else diff) s ;
  (* AC: runs of zeroes interleaved with the nonzero coefficients. *)
  let lastnz = ref 0 in
  for k = 1 to 63 do
    if blk.(zigzag.(k)) <> 0 then lastnz := k
  done ;
  let run = ref 0 in
  for k = 1 to !lastnz do
    let v = blk.(zigzag.(k)) in
    if v = 0 then incr run
    else begin
      while !run >= 16 do bw_emit w ac 0xf0 ; run := !run - 16 done ;
      let s = num_bits v in
      bw_emit w ac ((!run lsl 4) lor s) ;
      bw_put w (if v < 0 then v + (1 lsl s) - 1 else v) s ;
      run := 0
    end
  done ;
  if !lastnz < 63 then bw_emit w ac 0x00     (* end of block *)

type subsampling = Four_four_four | Four_two_two | Four_two_zero

(* Horizontal and vertical sampling factors of the luma component; the chroma
   components are always 1x1.  In 4:4:4 every component is 1x1, which is what
   makes the luma factors 1x1 as well. *)
let subsampling_of = function
  | Four_four_four -> (1, 1)
  | Four_two_two -> (2, 1)
  | Four_two_zero -> (2, 2)

type enc_comp = {
  e_id : int ;
  e_h  : int ;
  e_v  : int ;
  e_tq : int ;
  e_td : int ;
  e_ta : int ;
  e_pw : int ;               (* plane width *)
  e_ph : int ;               (* plane height *)
  e_data : int array ;       (* plane samples, row major *)
}

(* Halves one axis of a sample plane by averaging pairs, replicating the last
   sample when the dimension is odd. *)
let downsample_x2 (src : int array) (sw : int) (sh : int) : int array =
  let dw = ceil_div sw 2 in
  let dst = Array.make (dw * sh) 0 in
  for y = 0 to sh - 1 do
    for x = 0 to dw - 1 do
      let x0 = 2 * x and x1 = min ((2 * x) + 1) (sw - 1) in
      dst.((y * dw) + x) <- (src.((y * sw) + x0) + src.((y * sw) + x1) + 1) / 2
    done
  done ;
  dst

let downsample_y2 (src : int array) (sw : int) (sh : int) : int array =
  let dh = ceil_div sh 2 in
  let dst = Array.make (sw * dh) 0 in
  for y = 0 to dh - 1 do
    let y0 = 2 * y and y1 = min ((2 * y) + 1) (sh - 1) in
    for x = 0 to sw - 1 do
      dst.((y * sw) + x) <- (src.((y0 * sw) + x) + src.((y1 * sw) + x) + 1) / 2
    done
  done ;
  dst

let write_marker (och : chunk_writer) (m : int) : unit =
  chunk_write_char och '\255' ;
  chunk_write_char och (Char.chr m)

let write_u16 (och : chunk_writer) (n : int) : unit =
  chunk_write_char och (Char.chr ((n lsr 8) land 0xff)) ;
  chunk_write_char och (Char.chr (n land 0xff))

let write_app0_jfif (och : chunk_writer) : unit =
  write_marker och 0xe0 ;
  write_u16 och 16 ;
  chunk_write och "JFIF\000" ;
  chunk_write_char och '\001' ;              (* version 1.1 *)
  chunk_write_char och '\001' ;
  chunk_write_char och '\000' ;              (* density units: none *)
  write_u16 och 1 ;                          (* horizontal density *)
  write_u16 och 1 ;                          (* vertical density *)
  chunk_write_char och '\000' ;              (* thumbnail width *)
  chunk_write_char och '\000'               (* thumbnail height *)

let write_dqt (och : chunk_writer) (tables : (int * int array) list) : unit =
  write_marker och 0xdb ;
  write_u16 och (2 + (65 * List.length tables)) ;
  List.iter (fun (id, q) ->
    chunk_write_char och (Char.chr id) ;
    for k = 0 to 63 do
      chunk_write_char och (Char.chr q.(zigzag.(k)))
    done) tables

let write_sof0 (och : chunk_writer) (comps : enc_comp array) w h : unit =
  let n = Array.length comps in
  write_marker och 0xc0 ;
  write_u16 och (8 + (3 * n)) ;
  chunk_write_char och '\008' ;              (* sample precision *)
  write_u16 och h ;
  write_u16 och w ;
  chunk_write_char och (Char.chr n) ;
  Array.iter (fun c ->
    chunk_write_char och (Char.chr c.e_id) ;
    chunk_write_char och (Char.chr ((c.e_h lsl 4) lor c.e_v)) ;
    chunk_write_char och (Char.chr c.e_tq)) comps

let write_dht (och : chunk_writer)
    (tables : (int * int * int array * int array) list) : unit =
  let len =
    List.fold_left (fun acc (_, _, _, v) -> acc + 17 + Array.length v) 2 tables
  in
  write_marker och 0xc4 ;
  write_u16 och len ;
  List.iter (fun (class_, id, counts, values) ->
    chunk_write_char och (Char.chr ((class_ lsl 4) lor id)) ;
    Array.iter (fun c -> chunk_write_char och (Char.chr c)) counts ;
    Array.iter (fun v -> chunk_write_char och (Char.chr v)) values) tables

let write_sos (och : chunk_writer) (comps : enc_comp array) : unit =
  let n = Array.length comps in
  write_marker och 0xda ;
  write_u16 och (6 + (2 * n)) ;
  chunk_write_char och (Char.chr n) ;
  Array.iter (fun c ->
    chunk_write_char och (Char.chr c.e_id) ;
    chunk_write_char och (Char.chr ((c.e_td lsl 4) lor c.e_ta))) comps ;
  chunk_write_char och '\000' ;              (* Ss *)
  (* Note: OCaml character escapes are decimal, so the spectral end is
     written as 63, not 077. *)
  chunk_write_char och '\063' ;              (* Se *)
  chunk_write_char och '\000'               (* Ah / Al *)

(* Converts an image plane to 8-bit samples, scaling if necessary. *)
let scale_of_max_val (max_val : int) : int -> int =
  if max_val = 255 then fun v -> v
  else fun v -> ((v * 255) + (max_val / 2)) / max_val

(* [write_jpg ?quality ?subsampling och img] encodes [img] as a baseline
   sequential (SOF0) 8-bit JPEG and writes it to [och].

   [quality] ranges from 1 to 100 and scales the example quantisation tables
   the way libjpeg does; it defaults to 75.  [subsampling] selects the chroma
   resolution and defaults to {!Four_four_four}, i.e. no chroma subsampling.

   Alpha channels, if any, are ignored.  Images whose [max_val] exceeds 255 are
   scaled down to the 8-bit range. *)
let write_jpg ?(quality = 75) ?(subsampling = Four_four_four)
    (och : chunk_writer) (img : image) : unit =
  let w = img.width and h = img.height in
  if w <= 0 || h <= 0 then
    raise (Invalid_argument "write_jpg: image has a non-positive dimension") ;
  let luma_h, luma_v = subsampling_of subsampling in
  let grey =
    match img.pixels with
    | Grey _ | GreyA _ -> true
    | RGB _ | RGBA _ -> false
  in
  (* The maximum sampling factor must match the components actually written;
     a greyscale image has a single 1x1 component whatever [subsampling] says. *)
  let max_h = if grey then 1 else luma_h
  and max_v = if grey then 1 else luma_v in
  let scale = scale_of_max_val img.max_val in
  let luma_q = scale_quant_table ~quality std_luma_quant
  and chroma_q = scale_quant_table ~quality std_chroma_quant in
  let dc_luma = build_enc_tbl dc_luma_counts dc_luma_values
  and dc_chroma = build_enc_tbl dc_chroma_counts dc_chroma_values
  and ac_luma = build_enc_tbl ac_luma_counts ac_luma_values
  and ac_chroma = build_enc_tbl ac_chroma_counts ac_chroma_values in
  (* Build the component planes at their own resolutions. *)
  let comps =
    if grey then begin
      let data = Array.make (w * h) 0 in
      for y = 0 to h - 1 do
        for x = 0 to w - 1 do
          let v = ref 0 in
          Image.read_grey img x y (fun g -> v := g) ;
          data.((y * w) + x) <- clamp 0 255 (scale !v)
        done
      done ;
      [| { e_id = 1 ; e_h = 1 ; e_v = 1 ; e_tq = 0 ; e_td = 0 ; e_ta = 0 ;
           e_pw = w ; e_ph = h ; e_data = data } |]
    end else begin
      let yy = Array.make (w * h) 0
      and cb = Array.make (w * h) 0
      and cr = Array.make (w * h) 0 in
      for y = 0 to h - 1 do
        for x = 0 to w - 1 do
          Image.read_rgb img x y (fun r g b ->
            let rf = float_of_int (clamp 0 255 (scale r))
            and gf = float_of_int (clamp 0 255 (scale g))
            and bf = float_of_int (clamp 0 255 (scale b)) in
            let i = (y * w) + x in
            yy.(i) <-
              clamp255 ((0.299 *. rf) +. (0.587 *. gf) +. (0.114 *. bf)) ;
            cb.(i) <-
              clamp255
                ((-0.168736 *. rf) -. (0.331264 *. gf) +. (0.5 *. bf)
                 +. 128.) ;
            cr.(i) <-
              clamp255
                ((0.5 *. rf) -. (0.418688 *. gf) -. (0.081312 *. bf)
                 +. 128.))
        done
      done ;
      (* Reduce the chroma planes to the subsampled resolution. *)
      let dw = ceil_div w max_h and dh = ceil_div h max_v in
      let reduce data =
        let t, tw = if dw < w then (downsample_x2 data w h, dw)
          else (data, w) in
        if dh < h then (downsample_y2 t tw h, tw) else (t, tw)
      in
      let cb_d, cb_w = reduce cb and cr_d, cr_w = reduce cr in
      let chroma id data pw td =
        { e_id = id ; e_h = 1 ; e_v = 1 ; e_tq = 1 ; e_td = td ; e_ta = td ;
          e_pw = pw ; e_ph = dh ; e_data = data }
      in
      [| { e_id = 1 ; e_h = luma_h ; e_v = luma_v ; e_tq = 0 ; e_td = 0 ;
           e_ta = 0 ; e_pw = w ; e_ph = h ; e_data = yy } ;
         chroma 2 cb_d cb_w 1 ;
         chroma 3 cr_d cr_w 1 |]
    end
  in
  (* Entropy-coded segment: one interleaved scan over every component. *)
  let mcu_w = ceil_div w (max_h * 8) and mcu_h = ceil_div h (max_v * 8) in
  let out = bw_create () in
  let samples = Array.make 64 0
  and scratch = Array.make 64 0.0
  and coef = Array.make 64 0 in
  let tbl c =
    if c.e_tq = 0
    then (luma_q, dc_luma, ac_luma)
    else (chroma_q, dc_chroma, ac_chroma)
  in
  let preds = Array.init (Array.length comps) (fun _ -> ref 0) in
  for my = 0 to mcu_h - 1 do
    for mx = 0 to mcu_w - 1 do
      Array.iteri
        (fun ci c ->
          let qt, dc, ac = tbl c in
          for v = 0 to c.e_v - 1 do
            for hx = 0 to c.e_h - 1 do
              let bx = (mx * c.e_h) + hx and by = (my * c.e_v) + v in
              (* Gather the block; samples beyond the plane are replicated. *)
              for y = 0 to 7 do
                let sy = min ((by lsl 3) + y) (c.e_ph - 1) in
                for x = 0 to 7 do
                  let sx = min ((bx lsl 3) + x) (c.e_pw - 1) in
                  samples.((y lsl 3) + x) <- c.e_data.((sy * c.e_pw) + sx)
                done
              done ;
              fdct_into samples scratch coef qt ;
              encode_block out dc ac preds.(ci) coef
            done
          done)
        comps
    done
  done ;
  bw_flush out ;
  (* Header segments. *)
  write_marker och 0xd8 ;                     (* SOI *)
  write_app0_jfif och ;
  write_dqt och
    (if grey then [ (0, luma_q) ] else [ (0, luma_q) ; (1, chroma_q) ]) ;
  write_sof0 och comps w h ;
  write_dht och
    (if grey
     then [ (0, 0, dc_luma_counts, dc_luma_values) ;
            (1, 0, ac_luma_counts, ac_luma_values) ]
     else [ (0, 0, dc_luma_counts, dc_luma_values) ;
            (1, 0, ac_luma_counts, ac_luma_values) ;
            (0, 1, dc_chroma_counts, dc_chroma_values) ;
            (1, 1, ac_chroma_counts, ac_chroma_values) ]) ;
  write_sos och comps ;
  chunk_write och (Buffer.contents out.b_buf) ;
  write_marker och 0xd9                       (* EOI *)

let bytes_of_jpg ?quality ?subsampling (img : image) : Bytes.t =
  let buf = Buffer.create ((img.width * img.height) / 2) in
  let och = chunk_writer_of_buffer buf in
  write_jpg ?quality ?subsampling och img ;
  close_chunk_writer och ;
  (* [Buffer.to_bytes] would copy the result; avoid that. *)
  Bytes.unsafe_of_string (Buffer.contents buf)

(* [write] matches {!Image.WriteImage}: it uses the default quality and
   subsampling. *)
let write (och : chunk_writer) (img : image) : unit = write_jpg och img

include ReadJPG

