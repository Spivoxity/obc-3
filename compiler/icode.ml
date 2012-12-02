(*
 * icode.ml
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

open Print
open Symtab
open Eval

type check =
    NullPtr			(* Pointer is non-null *)
  | GlobProc			(* Procedure assigned is global *)
  | DivZero of kind		(* Divisor is non-zero *)

(* icode -- type of intermediate instructions *)
type icode =
    CONSTn of integer 	 	(* Push constant (value) *)
  | CONSTx of symbol		(* Push constant (value) *)
  | TCONST of kind * value	(* Typed constant *)
  | LOCAL of int		(* Push address (offset) *)
  | LOAD of kind		(* Load *)
  | STORE of kind		(* Store *)
  | FIXCOPY			(* Copy multiple values *)
  | FLEXCOPY			(* Copy open array param *)
  | DUP of int			(* Duplicate n'th value on stack (n) *)
  | POP of int			(* Pop value (count) *)
  | SWAP			(* Swap top two values on stack *)
  | STKMAP of symbol		(* Stack map for call point *)
  | CALL of int * kind		(* Proc call (pcount, result size) *)
  | LINK			(* Pass static link *)
  | SAVELINK			(* Save static link in frame *)
  | RETURN of kind		(* Return from procedure (rsize) *)
  | MONOP of kind * op		(* Unary operation (type, op) *)
  | BINOP of kind * op		(* Binary operation *)
  | CONV of kind * kind		(* Type conversion *)
  | ALIGN of kind		(* Align parameter (size) *)
  | BOUND of int		(* Array bound check (line) *)
  | CHECK of check * int	(* Runtime check (kind, line) *)
  | ERROR of symbol * int	(* Runtime error (kind, line) *)
  | EASSERT of int		(* Assertion failed (line) *)
  | JUMP of codelab		(* Unconditional branch (dest) *)
  | JUMPB of bool * codelab	(* Jump on boolean *)
  | JCASE of codelab list       (* Case jump *)
  | JRANGE of codelab		(* Range jump *)
  | TYPETEST of int		(* Type test (level) *)
  | LABEL of codelab		(* Set code label *)
  | LINE of int			(* Line number *)
  | NOP				(* No-op *)

  | INDEX of int		(* CONST n/BINOP Times/BINOP PlusA *)
  | LDL of kind * int		(* LOCAL n/LOAD s *)
  | STL of kind * int		(* LOCAL n/STORE s *)
  | LDG of kind * symbol	(* SYMBOL x/LOAD s *)
  | STG of kind * symbol	(* SYMBOL x/STORE s *)
  | LDI of kind			(* INDEX s/LOAD s *)
  | STI of kind			(* INDEX s/STORE s *)
  | LDNW of int			(* CONST n/LDI 4 *)
  | STNW of int			(* CONST n/STI 4 *)
  | LDEW of int			(* LDLW -4/LDNW n *)
  | STEW of int			(* LDLW -4/STNW n *)
  | INCL of int			(* LDLW n/INC/STLW n *)
  | DECL of int			(* LDLW n/DEC/STLW n *)
  | JUMPC of kind * op * codelab  (* op/JUMPB *)
  | JUMPCZ of op * codelab      (* CONST 0/JUMPC *)
  | TESTGEQ of codelab		(* Case split = DUP 1/JUMPC Lt *)

let opcode =
  function 
      Plus -> "PLUS" | Minus -> "MINUS" | Times -> "TIMES" 
    | Over -> "OVER" | Div -> "DIV" | Mod -> "MOD" | Eq -> "EQ" 
    | Uminus -> "UMINUS" | Lt -> "LT" | Gt -> "GT" 
    | Leq -> "LEQ" | Geq -> "GEQ" | Neq -> "NEQ" 
    | And -> "AND" | Or -> "OR" | Not -> "NOT" | PlusA -> "PLUSA"
    | BitAnd -> "BITAND" | BitOr -> "BITOR" 
    | BitNot -> "BITNOT" | BitXor -> "BITXOR" | BitSub -> "BITSUB"
    | Inc -> "INC" | Dec -> "DEC" | Bit -> "BIT" 
    | Lsl -> "LSL" | Lsr -> "LSR" | Asr -> "ASR"
    | w -> failwith (sprintf "opcode $" [fOp w])

let fOpcode w = fStr (opcode w)

let fType =
  function
      FloatT -> fStr "F"
    | DoubleT -> fStr "D"
    | LongT -> fStr "Q"
    | _ -> fStr ""

let fType1 =
  function
      CharT -> fStr "C"
    | ShortT -> fStr "S"
    | IntT -> fStr "N"
    | LongT -> fStr "Q"
    | FloatT -> fStr "F"
    | DoubleT -> fStr "D"
    | _ -> fStr "?"

let fKind =
  function
      VoidT -> fStr ""
    | (CharT | BoolT | ByteT) -> fStr "C"
    | ShortT -> fStr "S"
    | (IntT | PtrT | SetT) -> fStr "W"
    | LongT -> fStr "Q"
    | FloatT -> fStr "F"
    | DoubleT -> fStr "D"
    | _ -> fStr "?"

let fWidth =
  function
      1 -> fStr "C" | 2 -> fStr "S" | 4 -> fStr "W" | 8 -> fStr "D"
    | _ -> fStr "?"

(* Inst -- formatting function for use with printf *)
let fInst =
  function
      CONSTn n ->	fMeta "CONST $" [fInteger n]
    | CONSTx x ->	fMeta "CONST $" [fSym x]
    | TCONST (k, x) ->	fMeta "$CONST $" [fType k; fVal x]
    | LOCAL o ->	fMeta "LOCAL $" [fNum o]
    | LOAD s ->		fMeta "LOAD$" [fKind s]
    | STORE s ->	fMeta "STORE$" [fKind s]
    | FIXCOPY ->	fStr "FIXCOPY"
    | FLEXCOPY ->	fStr "FLEXCOPY"
    | DUP n ->		fMeta "DUP $" [fNum n]
    | SWAP ->		fStr "SWAP"
    | POP n ->		fMeta "POP $" [fNum n]
    | STKMAP x ->	fMeta "STKMAP $" [fSym x]
    | CALL (n, s) ->	fMeta "CALL$ $" [fKind s; fNum n]
    | LINK ->		fStr "LINK"
    | SAVELINK ->	fStr "SAVELINK"
    | RETURN s -> 	fMeta "RETURN$" [fKind s]
    | MONOP (t, w) ->  	fMeta "$$" [fType t; fOpcode w]
    | BINOP (t, w) ->  	fMeta "$$" [fType t; fOpcode w]
    | CONV (k1, k2) ->	fMeta "CONV$$" [fType1 k1; fType1 k2]
    | ALIGN s ->      	fMeta "ALIGN$" [fKind s]
    | BOUND ln ->	fMeta "BOUND $" [fNum ln]
    | CHECK (NullPtr, ln) ->
			fMeta "NCHECK $" [fNum ln]
    | CHECK (GlobProc, ln) ->	
			fMeta "GCHECK $" [fNum ln]
    | CHECK (DivZero t, ln) ->	
			fMeta "$ZCHECK $" [fType t; fNum ln]
    | ERROR (k, ln) ->	fMeta "ERROR $ $" [fSym k; fNum ln]
    | EASSERT ln ->     fMeta "EASSERT $" [fNum ln]
    | JUMP lab ->	fMeta "JUMP $" [fLab lab]
    | JUMPB (b, lab) -> 
	fMeta "JUMP$ $" [fStr (if b then "T" else "F"); fLab lab]
    | JUMPC (t, w, lab) ->	
	fMeta "$J$ $" [fType t; fOpcode w; fLab lab]
    | TESTGEQ lab ->	fMeta "TESTGEQ $" [fLab lab]
    | JCASE labs ->	fMeta "JCASE $" [fNum (List.length labs)]
    | JRANGE lab ->	fMeta "JRANGE $" [fLab lab]
    | TYPETEST lev ->	fMeta "TYPETEST $" [fNum lev]
    | INCL n ->		fMeta "INCL $" [fNum n]
    | DECL n ->		fMeta "DECL $" [fNum n]
    | JUMPCZ (w, lab) -> fMeta "J$Z $" [fOpcode w; fLab lab]
    | LABEL lab ->	fMeta "LABEL $" [fLab lab]
    | LINE n -> 	fMeta "LINE $" [fNum n]
    | NOP ->		fStr "NOP"

    | INDEX n ->	fMeta "INDEX$" [fWidth n]
    | LDL (s, n) ->	fMeta "LDL$ $" [fKind s; fNum n]
    | STL (s, n) ->	fMeta "STL$ $" [fKind s; fNum n]
    | LDG (s, x) ->	fMeta "LDG$ $" [fKind s; fSym x]
    | STG (s, x) ->	fMeta "STG$ $" [fKind s; fSym x]
    | LDI s ->		fMeta "LDI$" [fKind s]
    | STI s ->		fMeta "STI$" [fKind s]
    | LDNW n ->		fMeta "LDNW $" [fNum n]
    | STNW n ->		fMeta "STNW $" [fNum n]
    | LDEW n ->		fMeta "LDEW $" [fNum n]
    | STEW n ->		fMeta "STEW $" [fNum n]
