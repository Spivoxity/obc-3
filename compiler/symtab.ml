(*
 * symtab.ml
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
  
type ident = int

let atom_table = Hashtbl.create 1000
let atom_vec = Growvect.create 100

let extern x = Growvect.get atom_vec x

let intern x = 
  try Hashtbl.find atom_table x with
    Not_found ->
      let y = Growvect.size atom_vec in
      Hashtbl.add atom_table x y;
      Growvect.append atom_vec x;
      y

let intern_sys x =
   intern (if !Config.lcflag then Util.strlower x else x)

let anon = intern "*anon*"

let current = ref anon

let fId x = fStr (extern x)

let fQual (m, x) = 
   if m = !current || m = anon then fId x else fMeta "$.$" [fId m; fId x]

(* libid -- type of Oberon library procedures *)
type libid = 
    ChrFun | OrdFun | OddFun | NewProc | LenFun | AbsFun 
  | IncProc | DecProc | Assert | Entier | Short | Long
  | MinFun | MaxFun | AshFun | SizeFun | InclProc | ExclProc 
  | AdrFun | ValFun | BitFun | GetProc | PutProc
  | LslFun | LsrFun | AsrFun | RorFun | FltFun
  | PackProc | UnpkProc

type symbol = string

let nosym = "*nosym*"

let fSym s = fStr s

let sym_count = ref 0

let gensym () =
  incr sym_count; 
  sprintf "%$" [fNum !sym_count]

let genlab () = sprintf "$.$" [fId !current; fStr (gensym ())]

let proc_name m level x =
  if level = 0 then sprintf "$.$" [fId m; fId x]
  else sprintf "$.$.$" [fId m; fSym (gensym ()); fId x]

type codelab = int

(* label -- allocate a label *)
let label () = incr sym_count; !sym_count

let nolab = 0

let fLab n = fMeta "L$" [fNum n]

(* put -- generate a directive *)
let put fmt args = printf "$\n" [fMeta fmt args]

let put_int n = put "WORD $" [fNum n]
let put_sym s = put "WORD $" [fSym s]

(* We store a list of the string constants from the source program.
   It's stored as a backwards list to make it cheap to add a new
   string; the whole thing can be reversed in linear time when the
   time comes to generate code -- though the order hardly matters anyway. *)

(* strtbl -- table of string constants from source program *)
let strtbl = ref []
let strhash = Hashtbl.create 128

(* save_string -- store a string constant *)
let save_string s =
  try Hashtbl.find strhash s with
    Not_found ->
      let lab = genlab () in
      strtbl := (lab, s)::!strtbl; 
      Hashtbl.add strhash s lab;
      lab

(* put_string -- generate a string constant *)
let put_string (lab, s) = 
  let s' = s ^ "\000" in
  put "! String \"$\"" [fStr (String.escaped s)];
  put "DEFINE $" [fSym lab];
  let hex = "0123456789ABCDEF" in
  let n = String.length s' and r = ref 0 in
  while !r < n do
    let k = min (n - !r) 32 in
    printf "STRING " [];
    for i = !r to !r+k-1 do
      let c = int_of_char s'.[i] in
      printf "$$" [fChr (hex.[c / 16]); fChr (hex.[c mod 16])]
    done;
    printf "\n" [];
    r := !r + k
  done;
  printf "\n" []

let put_strings () =
  List.iter put_string (List.rev !strtbl)


(* |kind| -- basic types *)
type kind = 
    NumT | ByteT | ShortT | IntT | LongT | FloatT | DoubleT  
				(* Numerics in order of width *)
  | CharT | BoolT | SetT | PtrT | LongPtrT | SysByteT
				(* Non-numerics *)
  | VoidT | ErrT		(* Fictitious types *)

(* op -- type of Oberon operators *)
type op = 
    Plus | Minus | Times | Over | Div | Mod | Eq | Uminus | Uplus 
  | Lt | Gt | Leq | Geq | Neq | And | Or | Not
  | In | BitAnd | BitOr | BitNot | BitXor | BitSub
  | Inc | Dec | Lsl | Lsr | Asr | Ror

(* opposite -- negate a comparison operator *)
let opposite = 
  function Eq -> Neq | Neq -> Eq | Lt  -> Geq
    | Leq -> Gt | Gt  -> Leq | Geq -> Lt
    | _ -> raise Not_found

let commute =
  function
      Plus | Times | Eq | Neq | BitAnd | BitOr | BitXor | And | Or as w -> w
    | Lt -> Gt | Leq -> Geq | Gt -> Lt | Geq -> Leq
    | _ -> raise Not_found

(* (DEBUG-- *)
(* op_name -- name of an operator *)
let op_name = 
  function Plus -> "+" | Minus -> "-" | Times -> "*" 
    | Over -> "/" | Div -> "DIV" | Mod -> "MOD" | Eq -> "=" 
    | Uminus -> "unary -" | Uplus -> "unary +" | Lt -> "<" | Gt -> ">" 
    | Leq -> "<=" | Geq -> ">=" | Neq -> "#" | And -> "&" | Or -> "OR" 
    | Not -> "~" | BitAnd -> "*BITAND*" | BitOr -> "*BITOR*"
    | BitNot -> "*BITNOT*" | BitXor -> "*BITXOR*" | BitSub -> "*BITSUB*"
    | In -> "IN" | Inc -> "INC" | Dec -> "DEC" 
    | Asr -> "ASR" | Lsr -> "LSR" | Lsl -> "LSL" | Ror -> "ROR"
(* --DEBUG) *)

let fOp w = fStr (op_name w)
