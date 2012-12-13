(*
 * symtab.mli
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

(* ident -- type of identifiers *)
type ident

val intern : string -> ident
val extern : ident -> string
val anon : ident

val fId : ident -> Print.arg
val fQual : ident * ident -> Print.arg

(* |kind| -- basic types *)
type kind = 
    NumT | ShortT | IntT | LongT | FloatT | DoubleT  
				(* Numerics in order of width *)
  | CharT | BoolT | SetT | PtrT | ByteT
				(* Non-numerics *)
  | VoidT | ErrT		(* Fictitious types *)

(* op -- type of operators *)
type op = 
    Plus | Minus | Times | Over | Div | Mod | Eq | Uminus | Uplus 
  | Lt | Gt | Leq | Geq | Neq | And | Or | Not | PlusA
  | In | BitAnd | BitOr | BitNot | BitXor | BitSub 
  | Inc | Dec | Bit | Lsl | Lsr | Asr

val fOp : op -> Print.arg

(* commute -- find operator with commuted operands *)
val commute : op -> op

(* opposite -- negate a comparison operator *)
val opposite : op -> op

(* libid -- type of Oberon library procedures *)
type libid = 
    ChrFun | OrdFun | OddFun | NewProc | LenFun | AbsFun 
  | IncProc | DecProc | Assert | Entier | Short | Long
  | MinFun | MaxFun | AshFun | SizeFun | InclProc | ExclProc 
  | AdrFun | ValFun | BitFun | GetProc | PutProc
  | LslFun | LsrFun | AsrFun

type symbol = string

val nosym : symbol
val gensym : unit -> string
val genlab : unit -> symbol
val proc_name : ident -> int -> ident -> symbol

val fSym : symbol -> Print.arg

type codelab

(* label -- generate a code label *)
val label : unit -> codelab

val nolab : codelab

val fLab : codelab -> Print.arg

(* current -- name of the module being compiled *)
val current : ident ref

(* put -- output assembly directive *)
val put : string -> Print.arg list -> unit

(* put_int -- output integer constant *)
val put_int : int -> unit

(* put_sym -- output symbolic constant *)
val put_sym : symbol -> unit

(* save_string -- store a string constant *)
val save_string : string -> symbol

(* put_strings -- generate table of string constants *)
val put_strings : unit -> unit
