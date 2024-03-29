(*
 * util.mli
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

(* copy -- n copies of a value *)
val copy : int -> 'a -> 'a list

(* take -- first n elements of list *)
val take : int -> 'a list -> 'a list

(* drop -- all but first n elements of list *)
val drop : int -> 'a list -> 'a list

(* split -- cleave initial segment that satisfies a test *)
val split : ('a -> bool) -> 'a list -> ('a list * 'a list)

(* lexico -- lexical product of orderings, represented as 'compare' functions *)
val lexico : int -> int -> int

(* group_sort -- sort a list of pairs and group by first component *)
val group_sort : ('a -> 'a -> int) -> ('a * 'b) list -> ('a * 'b list) list

(* ungroup -- flatten a grouped list of pairs *)
val ungroup : ('a * 'b list) list -> ('a * 'b) list

(* commonest -- find commonest element of list with comparison function *)
val commonest : ('a -> 'a -> int) -> 'a list -> 'a

(* flat_map -- map function over list and concatenate results *)
val flat_map : ('a -> 'b list) -> 'a list -> 'b list

(* assoc_eq -- version of assoc with explicit equality test *)
val assoc_eq : ('a -> 'a -> bool) -> 'a -> ('a * 'b) list -> 'b

val range : int -> int -> int list

val split_at : char -> string -> string * string
val split_string : string -> string list

val hex_of_int : int -> string
val hex_of_int32 : int32 -> string
val float_as_string : float -> string

val make_hash : int -> ('a * 'b) list -> ('a, 'b) Hashtbl.t

val search_path : string -> string list -> string

val can : ('a -> 'b) -> 'a -> bool

(* offset -- add base address and offset *)
val offset : int32 -> int -> int32

val toupper : char -> char

val strlower : string -> string
