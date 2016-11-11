(*
 * gcmap.ml
 * 
 * This file is part of the Oxford Oberon-2 compiler
 * Copyright (c) 2006--2016 J. M. Spivey
 * All rights reserved
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 * 3. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
 * OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
 * OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
 * ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *)

open Eval
open Print
open Symtab

type gcmap = gcitem list
and gcitem =
    GC_Offset of int		(* A pointer *)
  | GC_Repeat of int * int * int * gcmap
				(* Repeat (base, count, stride, map) *)
  | GC_Block of int * int	(* Pointer block (base, count) *)
  | GC_Flex of int * int * int * gcmap
				(* Flex parameter (addr, ndim, stride, map) *)

let null_map = []

let ptr_map = [GC_Offset 0]

let flex_map o flex stride m = 
  if m = null_map then null_map 
  else [GC_Flex (o, flex, stride, m)]

let rec shift k m = List.map (shift_item k) m

and shift_item k =
  function
      GC_Offset o -> GC_Offset (o+k)
    | GC_Repeat (b, n, s, m) -> GC_Repeat (b+k, n, s, m)
    | GC_Block (b, n) -> GC_Block (b+k, n)
    | _ -> failwith "shift_item"

let is_block stride =
  function
      [GC_Offset 0] -> stride = 4
    | [GC_Block (0, count)] -> 4*count = stride
    | _ -> false

let repeat count stride m = 
  if m = null_map || count = 0 then 
    null_map
  else if count = 1 then
    m
  else if is_block stride m then 
    [GC_Block (0, count * stride / 4)]
  else 
    [GC_Repeat (0, count, stride, m)]

let join m1 m2 = m1 @ m2

let union ms = List.concat ms

let combine xs = List.fold_left Int32.logor Int32.zero xs

let bit origin n =
  let n' = origin + n/4 in
  if n' < 0 || n' >= 31 then raise Not_found;
  Int32.shift_left Int32.one n'

let rec bit_item origin =
  function
      GC_Offset o -> bit origin o
    | GC_Repeat (base, count, stride, m) ->
	combine (List.map
	  (fun i -> bitmap origin (shift (base+i*stride) m))
	  (Util.range 0 (count-1)))
    | GC_Block (base, count) ->
	bit_item origin (GC_Repeat (base, count, 4, [GC_Offset 0]))
    | _  -> raise Not_found

and bitmap origin m = 
  combine (List.map (bit_item origin) m)

let make_bitmap origin m = 
  Int32.logor (Int32.shift_left (bitmap origin m) 1) Int32.one

let rec put_map m = 
  try 
    (* Don't use a bitmap for a single item *)
    (match m with [GC_Offset o] -> raise Not_found | _ -> ());
    let bm = make_bitmap 0 m in 
    put_sym "GC_MAP"; put_sym (Util.hex_of_int32 bm)
  with 
    Not_found -> List.iter put_item m

and put_item =
  function
      GC_Offset o -> 
	put_int o
    | GC_Repeat (base, count, stride, m) ->
	put_sym "GC_REPEAT"; put_int base; put_int count; put_int stride;
	put_map m;
	put_sym "GC_END"
    | GC_Block (base, count) ->
	put_sym "GC_BLOCK"; put_int base; put_int count
    | GC_Flex (offset, ndim, stride, m) ->
	put_sym "GC_FLEX"; put_int offset; put_int ndim; put_int stride;
	put_map m;
	put_sym "GC_END"

let put_varmap lab m =
  put_sym "GC_BASE"; put_sym lab; put_map m


let maps = ref []

let save_map lab m = 
  maps := (lab, m) :: !maps

(* put_maps -- output saved pointer maps *)
let put_maps () =
  (* Don't use a bitmap, in case this is a frame map where
     a bitmap would be interpreted specially.  After all, 
     we're only here in any case because an earlier attempt
     to make a bitmap failed *)
  if !maps <> [] then begin
    put "! Pointer maps" [];
    List.iter (function (lab, m) -> 
	put "DEFINE $" [fSym lab]; 
	List.iter put_item m; 
	put_sym "GC_END"; put "" []) 
      (List.rev !maps)
  end
