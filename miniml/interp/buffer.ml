(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*    Pierre Weis and Xavier Leroy, projet Cristal, INRIA Rocquencourt    *)
(*                                                                        *)
(*   Copyright 1999 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Extensible buffers *)

type t =
 {mutable buffer : bytes;
  mutable position : int;
  mutable length : int;
  initial_buffer : bytes}

let create n =
 let n = if n < 1 then 1 else n in
 let s = Bytes.create n in
 {buffer = s; position = 0; length = n; initial_buffer = s}

let contents b = Bytes.sub_string b.buffer 0 b.position
let to_bytes b = Bytes.sub b.buffer 0 b.position

let sub b ofs len =
  if ofs < 0 || len < 0 || ofs > b.position - len
  then invalid_arg "Buffer.sub"
  else Bytes.sub_string b.buffer ofs len


let blit src srcoff dst dstoff len =
  if len < 0 || srcoff < 0 || srcoff > src.position - len
             || dstoff < 0 || dstoff > (Bytes.length dst) - len
  then invalid_arg "Buffer.blit"
  else
    Bytes.unsafe_blit src.buffer srcoff dst dstoff len


let nth b ofs =
  if ofs < 0 || ofs >= b.position then
   invalid_arg "Buffer.nth"
  else Bytes.unsafe_get b.buffer ofs


let length b = b.position

let clear b = b.position <- 0

let reset b =
  b.position <- 0; b.buffer <- b.initial_buffer;
  b.length <- Bytes.length b.buffer

let rec make_new_len new_len x =
  if x <= !new_len then () else begin new_len := 2 * !new_len; make_new_len new_len x end

let resize b more =
  let len = b.length in
  let new_len = ref len in
  (* while b.position + more > !new_len do new_len := 2 * !new_len done; *)
  make_new_len new_len (b.position + more);
  let new_buffer = Bytes.create !new_len in
  (* PR#6148: let's keep using [blit] rather than [unsafe_blit] in
     this tricky function that is slow anyway. *)
  Bytes.blit b.buffer 0 new_buffer 0 b.position;
  b.buffer <- new_buffer;
  b.length <- !new_len

let add_char b c =
  let pos = b.position in
  if pos >= b.length then resize b 1;
  Bytes.unsafe_set b.buffer pos c;
  b.position <- pos + 1

 let add_utf_8_uchar b u = let u = Uchar.to_int u in
 if u < 0 then assert false
 else if u <= 127 then
     add_char b (Char.unsafe_chr u)
 else if u <= 2047 then (*
     let pos = b.position in
     if pos + 2 > b.length then resize b 2;
     Bytes.unsafe_set b.buffer (pos    )
       (Char.unsafe_chr (0xC0 lor (u lsr 6)));
     Bytes.unsafe_set b.buffer (pos + 1)
       (Char.unsafe_chr (0x80 lor (u land 0x3F)));
     b.position <- pos + 2
 else if u <= 65535 then
     let pos = b.position in
     if pos + 3 > b.length then resize b 3;
     Bytes.unsafe_set b.buffer (pos    )
       (Char.unsafe_chr (0xE0 lor (u lsr 12)));
     Bytes.unsafe_set b.buffer (pos + 1)
       (Char.unsafe_chr (0x80 lor ((u lsr 6) land 0x3F)));
     Bytes.unsafe_set b.buffer (pos + 2)
       (Char.unsafe_chr (0x80 lor (u land 0x3F)));
     b.position <- pos + 3
 else if u <= 1114111 then
     let pos = b.position in
     if pos + 4 > b.length then resize b 4;
     Bytes.unsafe_set b.buffer (pos    )
       (Char.unsafe_chr (0xF0 lor (u lsr 18)));
     Bytes.unsafe_set b.buffer (pos + 1)
       (Char.unsafe_chr (0x80 lor ((u lsr 12) land 0x3F)));
     Bytes.unsafe_set b.buffer (pos + 2)
       (Char.unsafe_chr (0x80 lor ((u lsr 6) land 0x3F)));
     Bytes.unsafe_set b.buffer (pos + 3)
       (Char.unsafe_chr (0x80 lor (u land 0x3F)));
     b.position <- pos + 4
 else assert false *) assert false

let add_substring b s offset len =
  if offset < 0 || len < 0 || offset > String.length s - len
  then invalid_arg "Buffer.add_substring/add_subbytes";
  let new_position = b.position + len in
  if new_position > b.length then resize b len;
  Bytes.blit_string s offset b.buffer b.position len;
  b.position <- new_position

let add_subbytes b s offset len =
  add_substring b (Bytes.unsafe_to_string s) offset len

let add_string b s =
  let len = String.length s in
  let new_position = b.position + len in
  if new_position > b.length then resize b len;
  Bytes.blit_string s 0 b.buffer b.position len;
  b.position <- new_position

let add_bytes b s = add_string b (Bytes.unsafe_to_string s)

let add_buffer b bs =
  add_subbytes b bs.buffer 0 bs.position

(* read up to [len] bytes from [ic] into [b]. *)
let rec add_channel_rec b ic len =
  if len > 0 then (
    let n = input ic b.buffer b.position len in
    b.position <- b.position + n;
    if n = 0 then raise End_of_file
    else add_channel_rec b ic (len-n)   (* n <= len *)
  )

let add_channel b ic len =
  if len < 0 then   (* PR#5004 *)
    invalid_arg "Buffer.add_channel";
  if b.position + len > b.length then resize b len;
  add_channel_rec b ic len

let output_buffer oc b =
  output oc b.buffer 0 b.position


