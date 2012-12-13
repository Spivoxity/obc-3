(*
 * eval.ml
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
open Print

type integer = int64

let integer n = Int64.of_int n
let int_of_integer n = Int64.to_int n
let integer_of_int32 n = Int64.of_int32 n
let int32_of_integer n = Int64.to_int32 n
let integer_of_int64 n = n
let int64_of_integer n = n
let char_of_integer n = char_of_int (int_of_integer n)
let integer_of_string s = Int64.of_string s
let float_of_integer n = Int64.to_float n
let fInteger n = fStr (Int64.to_string n)

(* Mathematically correct div and mod *)
let divmod a b =
  let q = Int64.div a b and r = Int64.rem a b in
  if (b > Int64.zero && r < Int64.zero) 
      || (b < Int64.zero && r > Int64.zero) then 
    (Int64.sub q Int64.one, Int64.add r b) 
  else 
    (q, r)

let integer_add a b = Int64.add a b
let integer_sub a b = Int64.sub a b
let integer_mul a b = Int64.mul a b
let integer_div a b = fst (divmod a b)
let integer_mod a b = snd (divmod a b)
let integer_lsr a b = Int64.shift_right_logical a b
let integer_asr a b = Int64.shift_right a b
let integer_lsl a b = Int64.shift_left a b
let integer_bitand a b = Int64.logand a b
let integer_bitor a b = Int64.logor a b
let integer_bitxor a b = Int64.logxor a b

let integer_neg a = Int64.neg a
let integer_bitnot a = Int64.lognot a

let signext w a =
  let b = 64 - w in
  Int64.shift_right (Int64.shift_left a b) b


type value =
    IntVal of integer
  | FloVal of float

let intval n = IntVal (integer n)

(* int_value -- extract integer value *)
let int_value =
  function
      IntVal n -> n
    | FloVal x -> 
	(* Fudge it in case type errors lead us here *)
	integer 999

let flo_value =
  function
      FloVal x -> x
    | IntVal n -> Int64.to_float n

let widen v = FloVal (flo_value v)

let narrow =
  function
      IntVal n -> IntVal n
    | FloVal x -> IntVal (Int64.of_float x)

let is_float = function FloVal _ -> true | _ -> false

let integer_of_bool b = if b then Int64.one else Int64.zero

exception Bound_error

(* int_monop -- evaluate unary operators *)
let int_monop w x =
    match w with
	Uplus -> x
      | Uminus -> integer_neg x
      | Not -> integer_of_bool (x = integer 0)
      | BitNot -> integer_bitnot x
      | Inc -> integer_add x (integer 1)
      | Dec -> integer_sub x (integer 1)
      | Bit -> 
	  if x < integer 0 || x >= integer 32 then integer 0
	  else integer_lsl (integer 1) (int_of_integer x)
      | _ -> failwith (sprintf "int_monop $" [fOp w])

(* int_binop -- evaluate binary operators *)
let int_binop w x y =
  match w with
      Plus -> integer_add x y
    | Minus -> integer_sub x y
    | Times -> integer_mul x y
    | Div -> 
	if y = integer 0 then raise Division_by_zero; 
	integer_div x y
    | Mod -> if y = integer 0 then raise Division_by_zero; 
	integer_mod x y
    | Eq -> integer_of_bool (x = y)
    | Lt -> integer_of_bool (x < y)
    | Gt -> integer_of_bool (x > y)
    | Leq -> integer_of_bool (x <= y)
    | Geq -> integer_of_bool (x >= y)
    | Neq -> integer_of_bool (x <> y)
    | And -> integer_of_bool (x <> integer 0 && y <> integer 0)
    | Or -> integer_of_bool (x <> integer 0 || y <> integer 0)
    | BitAnd -> integer_bitand x y
    | BitOr -> integer_bitor x y
    | BitXor -> integer_bitxor x y
    | BitSub -> integer_bitand x (integer_bitnot y)
    | Lsl -> integer_lsl x (int_of_integer y)
    | Lsr -> integer_lsr x (int_of_integer y)
    | Asr -> integer_asr x (int_of_integer y)
    | In -> 
	if x < integer 0 || x >= integer 32 then
	  raise Bound_error;
	integer_of_bool 
	  (integer_bitand y (integer_lsl (integer 1) (int_of_integer x)) 
	    <> integer 0)
    | _ -> failwith (sprintf "int_binop $" [fOp w])

(* flo_monop -- evaluate unary floating-point operators *)
let flo_monop w x =
  match w with
      Uplus -> FloVal x
    | Uminus -> FloVal (-. x)
    | _ -> failwith (sprintf "flo_monop $" [fOp w])

(* flo_binop -- evaluate binary floating-point operators *)
let flo_binop w x y =
  match w with
      Plus -> FloVal (x +. y)
    | Minus -> FloVal (x -. y)
    | Times -> FloVal (x *. y)
    | Over -> if y = 0.0 then raise Division_by_zero; FloVal (x /. y)
    | Eq -> IntVal (integer_of_bool (x = y))
    | Lt -> IntVal (integer_of_bool (x < y))
    | Gt -> IntVal (integer_of_bool (x > y))
    | Leq -> IntVal (integer_of_bool (x <= y))
    | Geq -> IntVal (integer_of_bool (x >= y))
    | Neq -> IntVal (integer_of_bool (x <> y))
    | _ -> failwith (sprintf "flo_binop $" [fOp w])

(* do_monop -- polymorphic unary operators *)
let do_monop w x =
  if is_float x then 
    flo_monop w (flo_value x)
  else 
    IntVal (int_monop w (int_value x))

(* do_binop -- polymorphic binary operators *)
let do_binop w x y =
  if is_float x || is_float y then
    flo_binop w (flo_value x) (flo_value y)
  else
    IntVal (int_binop w (int_value x) (int_value y))

(* make_zero -- polymorphic zero *)
let make_zero =
  function
      (IntT | PtrT) -> IntVal (integer 0)
    | (FloatT | DoubleT) -> FloVal 0.0
    | _ -> failwith "make_zero"

let fVal =
  function
      IntVal n -> fInteger n
    | FloVal x -> fFlo x

let bit_range lo32 hi32 =
  let lo = int_of_integer lo32 and hi = int_of_integer hi32 in
  let x = if lo >= 32 then integer 0
    else integer_lsl (integer (-1)) lo
  and y = if hi >= 31 then integer 0
    else integer_lsl (integer (-1)) (hi+1) in
  integer_bitand x (integer_bitnot y)
