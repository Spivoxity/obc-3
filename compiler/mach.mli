(*
 * mach.mli
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

(*
This module contains a number of constants that define the layout of
data in the target machine.  The |metrics| records |int_rep|, etc.,
define the representation of the primitive types of Oberon;
|param_rep| defines the representation of procedure parameters, and
|proc_rep| defines the representation of closures.

|addr_size| is the size of an address in the target machine, and
|max_align| is the maximum alignment constraint of any primitive type,
used to round up the size of stack frames.  |frame_head level| is the
size of the fixed part of a procedure frame for static level |level|.
*)

open Eval

(* metrics -- target representation of data object *)
type metrics = 
  { m_size: int; 			(* Size of object *)
    m_align: int }			(* Address must be multiple of this *)

(* Data representations *)
val int_rep : metrics			(* integer type *)
val short_rep : metrics			(* short type *)
val long_rep : metrics			(* long type *)
val char_rep : metrics			(* char type *)
val bool_rep : metrics			(* boolean type *)
val float_rep : metrics			(* real type *)
val double_rep : metrics		(* longreal type *)
val void_rep : metrics			(* void type *)
val addr_rep : metrics			(* all addresses *)
val param_rep : metrics			(* procedure parameters *)

val word_size : int
val max_align : int
val set_size : int

val frame_head : int			(* Size of frame head *)
val dyn_link : int   			(* Offset of dynamic link *)
val stat_link : int			(* Offset of static link *)
val frame_shift: int			(* Amount frame map is shifted *)

val depth_offset : int			(* Offset of inheritance depth *)
val ancestor_offset : int		(* Offset of ancestor table *)
val method_offset : int			(* Offset of method table *)
val bound_offset :  int			(* Offset of bounds for flex array *)

(* Min and max values of each type *)
val minchar: integer
val maxchar: integer
val minshort : integer
val maxshort : integer
val minint: integer
val maxint: integer
val minlong: integer
val maxlong: integer
val minreal: float
val maxreal: float
val mindouble: float
val maxdouble: float
