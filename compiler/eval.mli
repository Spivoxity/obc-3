(*
 * eval.mli
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

open Symtab

type integer

val integer : int -> integer
val int_of_integer : integer -> int
val integer_of_int32 : int32 -> integer
val int32_of_integer : integer -> int32
val integer_of_int64 : int64 -> integer
val int64_of_integer : integer -> int64
val char_of_integer : integer -> char
val integer_of_string : string -> integer
val float_of_integer : integer -> float
val fInteger : integer -> Print.arg

val integer_add : integer -> integer -> integer
val integer_sub : integer -> integer -> integer
val integer_mul : integer -> integer -> integer
val integer_mod : integer -> integer -> integer
val integer_div : integer -> integer -> integer
val integer_bitor : integer -> integer -> integer
val integer_bitand : integer -> integer -> integer
val integer_lsl : integer -> int -> integer
val integer_asr : integer -> int -> integer
val integer_lsr : integer -> int -> integer
val integer_neg : integer -> integer

val int_monop : op -> integer -> integer
val int_binop : op -> integer -> integer -> integer
val bit_range : integer -> integer -> integer

val signext : int -> integer -> integer


type value =
    IntVal of integer
  | FloVal of float

val do_monop : op -> value -> value
val do_binop : op -> value -> value -> value

val make_zero : kind -> value

(* flo_value -- extract float *)
val flo_value : value -> float

(* int_value -- extract int *)
val int_value : value -> integer

(* intval -- shortcut for creating small integer values *)
val intval : int -> value

val widen : value -> value
val narrow : value -> value

val fVal : value -> Print.arg
