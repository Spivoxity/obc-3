(*
 * icode.mli
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

open Symtab
open Eval

type check =
    NullPtr			(* Pointer is non-null *)
  | GlobProc			(* Procedure assigned is global *)
  | DivZero of kind		(* Divisor is non-zero *)

(* icode -- type of intermediate instructions *)
type icode =
    CONST of integer 	 	(* Push constant (value) *)
  | HEXCONST of int32		(* Push hex constant (value) *)
  | TCONST of kind * value	(* Typed constant *)
  | GLOBAL of symbol		(* Push global addr (value) *)
  | LOCAL of int		(* Push local address (offset) *)
  | LOAD of kind		(* Load *)
  | STORE of kind		(* Store *)
  | FIXCOPY			(* Copy multiple values *)
  | FLEXCOPY			(* Copy open array param *)
  | DUP of int			(* Duplicate n'th value on stack (n) *)
  | POP of int			(* Pop value (count) *)
  | SWAP			(* Swap top two values on stack *)
  | STATLINK			(* Pass static link *)
  | STKMAP of symbol		(* Stack map for call point *)
  | CALL of int * kind		(* Global call (pcount, result size) *)
  | RETURN 			(* Return from procedure (rsize) *)
  | MONOP of kind * op		(* Unary operation (type, op) *)
  | BINOP of kind * op		(* Binary operation *)
  | OFFSET			(* Add address and offset *)
  | CONV of kind * kind		(* Type conversion *)
  | ALIGN of kind		(* Align parameter (size) *)
  | BOUND of int		(* Array bound check (line) *)
  | CHECK of check * int	(* Runtime check (kind, line) *)
  | ERROR of symbol * int	(* Runtime error (kind, line) *)
  | JUMP of codelab		(* Unconditional branch (dest) *)
  | JUMPC of kind * op * codelab  (* Conditional branch *)
  | JUMPN of kind * op * codelab  (* Negated condx branch *)
  | JCASE of codelab list       (* Case jump *)
  | JRANGE of codelab		(* Range jump *)
  | LABEL of codelab		(* Set code label *)
  | LINE of int			(* Line number *)

  | ADJUST of int		(* CONST n/OFFSET *)
  | INDEX of int		(* CONST n/BINOP Times/BINOP PlusA *)
  | LDL of kind * int		(* LOCAL n/LOAD s *)
  | STL of kind * int		(* LOCAL n/STORE s *)
  | LDG of kind * symbol	(* SYMBOL x/LOAD s *)
  | STG of kind * symbol	(* SYMBOL x/STORE s *)
  | LDI of kind			(* INDEX s/LOAD s *)
  | STI of kind			(* INDEX s/STORE s *)
  | LDN of kind * int		(* CONST n/LDI s *)
  | STN of kind * int		(* CONST n/STI s *)
  | INCL of int			(* LDLW n/INC/STLW n *)
  | DECL of int			(* LDLW n/DEC/STLW n *)
  | JUMPCZ of op * codelab      (* CONST 0/JUMPC *)
  | TESTGEQ of codelab		(* Case split = DUP 1/JUMPC Lt *)

  | MARK			(* Mark needed here *)
  | SEQ of icode list		(* Sequence *)
  | NOP				(* No-op *)

(* canon -- flatten icode into a list without SEQ or NOP *)
val canon : icode -> icode list

val fType : kind -> Print.arg
val fType1 : kind -> Print.arg
val fOpcode : op -> Print.arg

(* Inst -- printf format for instructions *)
val fInst : icode -> Print.arg

val put_line : int -> unit

val output : int -> icode list -> unit
