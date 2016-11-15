(*
 * error.mli
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

val err_count : int ref

type location = int * int

(* no_loc -- unidentified location *)
val no_loc : location

(* join_locs -- range spanned by two locations *)
val join_locs : location -> location -> location

(* here -- location of current lookahead token *)
val here : unit -> location

(* syn_error -- report syntax error at specified location *)
val syn_error : string -> Print.arg list -> location -> unit

(* syn_error2 -- syntax error with extra location *)
val syn_error2 : string -> Print.arg list -> location -> location -> unit

(* sem_error -- report semantic error *)
val sem_error : string -> Print.arg list -> location -> unit

(* sem_context -- give corroborative detail *)
val sem_context : string -> Print.arg list -> unit

(* sem_warn -- report semantic warning *)
val sem_warn : string -> Print.arg list -> location -> unit

(* sem_extend -- warn about use of language extensions *)
val sem_extend : string -> Print.arg list -> location -> unit

(* Token -- printf argument for current token *)
val fToken : Print.arg

(* source_line -- fetch line of source file *)
val source_line : int -> string

(* line_num -- map location to starting line number *)
val line_num : location -> int

val end_loc: location -> location

(* num_lines -- return number of source lines *)
val num_lines : unit -> int

val init_errors : string -> in_channel -> Lexing.lexbuf -> unit
val note_line : int -> unit
val set_line : int -> string -> unit
val set_loc : location -> unit
