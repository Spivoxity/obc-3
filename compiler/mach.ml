(*
 * mach.ml
 * 
 * This file is part of the Oxford Oberon-2 compiler
 * Copyright (c) 2006 J. M. Spivey
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

(* Machine parameters for the bytecode machine *)

open Dict
open Eval

(* metrics -- target representation of data object *)
type metrics = 
  { m_size: int; 		(* Size of object *)
    m_align: int }		(* Address must be multiple of this *)

(* If increasing any m_align value above 4, beware of the fact that
   heap blocks will be offset by the one-word descriptor.  This is no
   problem at present, because the bytecode interpreter does not use
   double-word loads and stores. *)

let int_rep  = { m_size = 4; m_align = 4 }
and short_rep = { m_size = 2; m_align = 2 }
and long_rep = { m_size = 8; m_align = 4 }
and char_rep = { m_size = 1; m_align = 1 }
and bool_rep = { m_size = 1; m_align = 1 }
and float_rep = { m_size = 4; m_align = 4 }
and double_rep = { m_size = 8; m_align = 4 }
and void_rep = { m_size = 0; m_align = 4 }
and addr_rep = { m_size = 4; m_align = 4 }
and param_rep = { m_size = 4; m_align = 4 }

let word_size = int_rep.m_size
let max_align = 4
let set_size = 32

(* 
Frame layout:

	arg n
	...
	arg 1
bp+2:   current cp               \  
bp+1:   return addr               > frame head = 3 or 4 words
bp:	dynamic link             /
bp-1:   static link (optional)
	local 1
	...
	local m

	alloca space

	expr stack
	 |     |
sp:      V     V
*)

let frame_head = 12
let dyn_link = 0
let stat_link = -4
let frame_shift = 4*16

(*
Record descriptor layout:

desc:	gc map
	inheritance depth
        ancestors
	method 1
	...
	method n

ancestors:
	ancestor 1
	...
	ancestor n


Flex descriptor layout:

desc:	gc map
        bound 1
	...
	bound k
*)

let desc_map = 0
let depth_offset = 4
let ancestor_offset = 8
let method_offset = 12
let bound_offset = 4

(*
Constant pool layout

cp:	INTERP
	entry point
	frame size
	exception table
	frame map
	stack map table
	constant 1
	...
	constant n
*)

let minchar = integer 0
let maxchar = integer 0xff
let minshort = integer (-32768)
let maxshort = integer 32767
let maxint = integer_of_string "0x7fffffff"
let minint = integer_sub (integer_neg maxint) (integer 1)
let maxlong = integer_of_string "0x7fffffffffffffff"
let minlong = integer_sub (integer_neg maxlong) (integer 1)

let maxreal = 3.40282346638528859812e+38
(* assert (maxreal = float_of_hex "0x7f7fffff") *)
let minreal = ~-. maxreal

let maxdouble = 1.79769313486231570815e+308
(* assert (maxdouble = double_of_hex "0x7fefffffffffffff") *)
let mindouble = ~-. maxdouble
