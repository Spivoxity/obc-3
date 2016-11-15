(*
 * procio.mli
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

(* This module manages communication with the debugging monitor *)

(* init -- start up the debugging monitor *)
val init : string list -> unit

(* send -- send a command *)
val send : string -> Print.arg list -> unit

(* receive -- wait for a response *)
val receive : unit -> string

(* close -- delete the communication socket after the monitor has exited *) 
val close : unit -> unit

(* wake_child -- send interrupt to child process *)
val wake_child : unit -> unit

(* peek n addr -- fetch integer (n = 1, 2 or 4 bytes) *)
val peek : int -> int32 -> int32

(* peek_float, etc -- fetch or store other data types *)
val peek_float : int32 -> float
val peek_double : int32 -> float 
val peek_int64: int32 -> int64
val peek_string : int32 -> string

(* flush_cache -- discard cache of peeked values *)
val flush_cache : unit -> unit

(* trace -- control tracing of interaction with the monitor *)
val trace : bool ref

(* interp -- location of monitor *)
val interp : string ref
