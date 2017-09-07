(*
 * icode.ml
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

open Print
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
  | OFFSET			(* Add address and offset *)
  | CONV of kind * kind		(* Type conversion *)
  | ALIGN of kind		(* Align parameter (size) *)
  | BOUND of int		(* Array bound check (line) *)
  | CHECK of check * int	(* Runtime check (kind, line) *)
  | ERROR of symbol * int	(* Runtime error (kind, line) *)
  | EASSERT of int		(* Assertion failed (line) *)
  | JUMP of codelab		(* Unconditional branch (dest) *)
  | JUMPC of kind * op * codelab  (* Conditional branch *)
  | JUMPN of kind * op * codelab  (* Negated condx branch *)
  | JCASE of codelab list       (* Case jump *)
  | JRANGE of codelab		(* Range jump *)
  | LABEL of codelab		(* Set code label *)
  | LINE of int			(* Line number *)

  | INDEX of int		(* CONST n/BINOP Times/BINOP PlusA *)
  | LDL of kind * int		(* LOCAL n/LOAD s *)
  | STL of kind * int		(* LOCAL n/STORE s *)
  | LDG of kind * symbol	(* CONST x/LOAD s *)
  | STG of kind * symbol	(* CONST x/STORE s *)
  | LDI of kind			(* INDEX s/LOAD s *)
  | STI of kind			(* INDEX s/STORE s *)
  | LDNW of int			(* CONST n/LDI 4 *)
  | STNW of int			(* CONST n/STI 4 *)
  | INCL of int			(* LDLW n/INC/STLW n *)
  | DECL of int			(* LDLW n/DEC/STLW n *)
  | JUMPCZ of op * codelab      (* CONST 0/JUMPC *)
  | TESTGE of codelab		(* Case split = DUP 1/JUMPC Lt *)

  | XMARK			(* Mark needed here *)
  | XSTKMAP of int		(* Stack map needed here *)
  | SEQ of icode list		(* Sequence *)
  | NOP				(* No-op *)

let mark_line n ys =
  if n = 0 then ys else
    match ys with
	[] | LINE _ :: _ -> ys
      | _ -> LINE n :: ys

let canon code =
  let rec walk x ys =
    match x with
	NOP -> ys
      | SEQ xs -> List.fold_right walk xs ys
      | LINE n -> mark_line n ys
      | _ -> x :: ys in
  walk code []

let opcode =
  function 
      Plus -> "PLUS" | Minus -> "MINUS" | Times -> "TIMES" 
    | Over -> "OVER" | Div -> "DIV" | Mod -> "MOD" | Eq -> "EQ" 
    | Uminus -> "UMINUS" | Lt -> "LT" | Gt -> "GT" 
    | Leq -> "LE" | Geq -> "GE" | Neq -> "NE" 
    | And -> "AND" | Or -> "OR" | Not -> "NOT"
    | BitAnd -> "BITAND" | BitOr -> "BITOR" 
    | BitNot -> "BITNOT" | BitXor -> "BITXOR" | BitSub -> "BITSUB"
    | Inc -> "INC" | Dec -> "DEC"
    | Lsl -> "LSL" | Lsr -> "LSR" | Asr -> "ASR" | Ror -> "ROR"
    | w -> failwith (sprintf "opcode $" [fOp w])

let fOpcode w = fStr (opcode w)

let fType =
  function
      FloatT -> fStr "F"
    | DoubleT -> fStr "D"
    | LongT | LongPtrT -> fStr "Q"
    | _ -> fStr ""

let fType1 =
  function
      CharT|ByteT -> fStr "C"
    | ShortT -> fStr "S"
    | IntT -> fStr "N"
    | LongT|LongPtrT -> fStr "Q"
    | FloatT -> fStr "F"
    | DoubleT -> fStr "D"
    | _ -> fStr "?"

let fKind =
  function
      VoidT -> fStr ""
    | CharT|ByteT -> fStr "C"
    | ShortT -> fStr "S"
    | IntT -> fStr "W"
    | LongT|LongPtrT -> fStr "Q"
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
      CONST n ->	fMeta "CONST $" [fInteger n]
    | HEXCONST n ->	fMeta "CONST $" [fHex32 n]
    | TCONST (k, x) ->	fMeta "$CONST $" [fType k; fVal x]
    | GLOBAL x ->	fMeta "GLOBAL $" [fSym x]
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
    | OFFSET ->		fStr "OFFSET"
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
    | JUMPC (t, w, lab) ->	
	fMeta "$J$ $" [fType t; fOpcode w; fLab lab]
    | JUMPN (t, w, lab) ->	
	fMeta "$JN$ $" [fType t; fOpcode w; fLab lab]
    | TESTGE lab ->	fMeta "TESTGE $" [fLab lab]
    | JCASE labs ->	fMeta "JCASE $" [fNum (List.length labs)]
    | JRANGE lab ->	fMeta "JRANGE $" [fLab lab]
    | INCL n ->		fMeta "INCL $" [fNum n]
    | DECL n ->		fMeta "DECL $" [fNum n]
    | JUMPCZ (w, lab) -> fMeta "J$Z $" [fOpcode w; fLab lab]
    | LABEL lab ->	fMeta "LABEL $" [fLab lab]
    | LINE n -> 	fMeta "LINE $" [fNum n]

    | INDEX n ->	fMeta "INDEX$" [fWidth n]
    | LDL (s, n) ->	fMeta "LDL$ $" [fKind s; fNum n]
    | STL (s, n) ->	fMeta "STL$ $" [fKind s; fNum n]
    | LDG (s, x) ->	fMeta "LDG$ $" [fKind s; fSym x]
    | STG (s, x) ->	fMeta "STG$ $" [fKind s; fSym x]
    | LDI s ->		fMeta "LDI$" [fKind s]
    | STI s ->		fMeta "STI$" [fKind s]
    | LDNW n ->		fMeta "LDNW $" [fNum n]
    | STNW n ->		fMeta "STNW $" [fNum n]

    | XMARK ->		fStr "XMARK"
    | XSTKMAP n ->	fMeta "XSTKMAP $" [fNum n]
    | SEQ _ ->		fStr "SEQ ..."
    | NOP ->		fStr "NOP"

let put_line n =
  printf "! $\n" [fStr (Error.source_line n)];
  if !Config.linecount then printf "$\n" [fInst (LINE n)]

let output ln code =
  let line = ref ln in
  List.iter (function
      LINE n ->
	if n <> 0 && n <> !line then begin
	  line := n; put_line n
	end
    | JCASE labs ->
	printf "$\n" [fInst (JCASE labs)];
	List.iter (fun lab -> printf "CASEL $\n" [fLab lab]) labs
    | x -> 
	printf "$\n" [fInst x]) code

