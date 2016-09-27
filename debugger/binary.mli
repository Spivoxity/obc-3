(*
 * binary.mli
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

(* This module keeps tables of information about procedures and line numbers
   in the binary program.  The tables are filled in from data that
   comes from the symbol table in the bytecode file, and is sent over
   the pipe by the debugging monitor at the start of the session. *)

(* Data types *)

(* regs -- machine register state *)
type regs = { cp: int32; bp: int32; pc: int32 }

type proc_data =
  { s_name: string;			(* Name, in the form Module.Proc *)
    s_addr: int32;			(* Address of the CP  *)
    s_code: int32;			(* Address of the bytecode *)
    s_size: int }			(* Size of bytecode *)

type var_data =
  { v_name: string;			(* Name, in the form Module.var *)
    v_addr: int32 }			(* Address *)

type line_data =
  { l_module: Symtab.ident;		(* Module name *)
    l_num: int;				(* Line number *)
    l_addr: int32 }			(* Bytecode address *)


(* Interface for filling in tables *)

val clear: unit -> unit
val def_module : Symtab.ident -> int -> unit
val def_proc : proc_data -> unit
val def_var : var_data -> unit
val def_line : line_data -> unit

val all_modules: unit -> Symtab.ident list
val checksum : Symtab.ident -> int

(* Look up objects from names *)
val proc_lookup : string -> proc_data
val var_lookup : string -> int32
val line_lookup : Symtab.ident -> int -> int32 list

(* curr_proc -- procedure from CP *)
val curr_proc : int32 -> proc_data

(* addr_lookup -- name of proc or var from address *)
val addr_lookup : int32 -> string

(* prev_line -- line number from bytecode address *)
val prev_line : int32 -> line_data


(* Source locations *)

type source_loc = 
    NoLoc 				(* Unknown *)
  | Proc of string 			(* Somewhere in named proc *)
  | Line of Symtab.ident * string * int	(* On a specific line 
					    (module, proc, line) *)

(* find_loc -- find source location from register state *)
val find_loc : regs -> source_loc
