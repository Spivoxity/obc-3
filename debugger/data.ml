(*
 * data.ml
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
open Dict
open Eval
open Print
open Procio
open Mach
open Binary
open Util

(* Type of dynamic values *)
type dynvalue = Dict.otype * dynrep

and dynrep =
    Simple of Eval.value		(* A simple value, integer or float *)
  | Address of int32			(* The address of something *)

let type_of (t, r) = t

let get_addr =
  function
      Address a -> a
    | _ -> failwith "value is not address"

let void_value = (voidtype, Simple (intval 0))

let fetch t a =
  if not (scalar t) then
    (t, Address a)
  else begin
    let r = match t.t_guts with
	BasicType (IntT | ByteT | CharT | BoolT | SetT | SysByteT) 
            | EnumType _ ->
	  Simple (IntVal (integer_of_int32 (peek t.t_rep.m_size a)))
      | BasicType ShortT ->
	  Simple (IntVal (signext 16 (integer_of_int32 (peek 2 a))))
      | BasicType LongT ->
	  Simple (IntVal (integer_of_int64 (peek_int64 a)))
      | BasicType FloatT ->
	  Simple (FloVal (peek_float a))
      | BasicType DoubleT ->
	  Simple (FloVal (peek_double a))
      | BasicType VoidT ->
	  Simple (IntVal (integer 0))
      | PointerType _ | BasicType PtrT | ProcType _ ->
	  Address (peek 4 a)
      | _ ->
	failwith "fetching unknown type" in
    (t, r)
  end

(* var_addr -- compute address of variable *)
let var_addr base d =
  if d.d_level = 0 then
    try Binary.var_lookup d.d_lab with Not_found -> failwith "no address"
  else 
    (* Assume local to current procedure *)
    offset base d.d_offset

let null_pointer (t, v) = is_pointer t && v = Address Int32.zero

(* fix_flex -- convert flex array to fixed array type *)
let fix_flex t0 bound_addr =
  let bounds =
    List.map 
      (fun i -> Int32.to_int (peek 4 (offset bound_addr (4*i))))
      (Util.range 0 (flexity t0 - 1)) in
  List.fold_right 
    (fun n t -> (new_type 0 (row n t))) 
    bounds (flex_base t0)

(* dynamic_record -- find dynamic type of record from descriptor *)
let dynamic_record desc =
  let sym = Binary.addr_lookup desc in
  let def = Info.get_debug sym in
  if def.d_kind <> TypeDef then begin
    printf "? not a type\n" [];
    raise Not_found
  end
  else if is_record def.d_type then 
    def.d_type
  else begin
    printf "? not a record" [];
    raise Not_found
  end

(* def_value -- compute value from definition *)
let def_value base d =
  match d.d_kind with
      ParamDef -> 
	let addr = var_addr base d in
	if is_flex d.d_type then
	  fetch (fix_flex d.d_type (offset addr 4)) (peek 4 addr)
	else
	  fetch d.d_type addr
    | VParamDef ->
	let addr = var_addr base d in
	if scalar d.d_type then
	  fetch d.d_type (peek 4 addr)
	else if is_record d.d_type then
	  let t1 = 
	    try dynamic_record (peek 4 (offset addr 4)) with
	      Not_found -> d.d_type in
	  fetch t1 (peek 4 addr)
	else if is_flex d.d_type then
	  fetch (fix_flex d.d_type (offset addr 4)) (peek 4 addr)
	else
	  fetch d.d_type (peek 4 addr)
    | VarDef ->
	fetch d.d_type (var_addr base d)
    | StringDef ->
	let addr = var_addr base d in
	fetch d.d_type addr
    | ConstDef v ->
	(d.d_type, Simple v)
    | ProcDef ->
	let p = proc_lookup d.d_lab in
	(d.d_type, Address p.s_addr)
    | _ -> 
	failwith (sprintf "$ does not have a value" [fId d.d_tag])

let deref_type t r =
  let a = get_addr r in
  if a = Int32.zero then 
    failwith "null pointer error";
  let t0 = base_type t in
  if is_flex t0 then
    let desc = peek 4 (offset a (-4)) in
    fix_flex t0 (offset desc bound_offset) 
  else if is_record t0 then
    let desc = peek 4 (offset a (-4)) in
    try dynamic_record desc with Not_found -> t0
  else 
    t0

let deref (t, r) = (deref_type t r, r)

let select (t, r) d =
  let a = get_addr r in
  fetch d.d_type (offset a d.d_offset)

let subscript (t, r) i =
  let t0 = base_type t in
  let a = get_addr r in
  fetch t0 (offset a (i * t0.t_rep.m_size))

let fEnum t v =
  try let d = Info.find_enum t v in fId d.d_tag with
    Not_found -> fMeta "<enum $>" [fVal v]

let fDynVal =
  function
      (t, Simple v) -> 
	begin match t.t_guts with
	    BasicType CharT ->
	      fMeta "'$'" [fStr (Char.escaped (char_of_integer (int_value v)))]
	  | BasicType BoolT ->
	      fStr (if int_value v <> integer 0 then "TRUE" else "FALSE")
	  | BasicType FloatT ->
	      fStr (Printf.sprintf "%.6G" (flo_value v))
	  | BasicType DoubleT ->
	      fStr (Printf.sprintf "%.12G" (flo_value v))
	  | BasicType VoidT ->
	      fStr "-"
	  | EnumType _ ->
	      fEnum t v
	  | _ -> fVal v
	end
    | (t, Address a) ->
	if is_string t then
	  fMeta "\"$\"" [fStr (String.escaped (peek_string a))]
	else if is_pointer t && a = Int32.zero then 
	  fStr "NIL"
	else if is_proc t then begin
	  try
	    let p = Binary.addr_lookup a in fStr p
	  with Not_found ->
	    fHex32 a
	end
	else
	  fHex32 a

let rec fField v d = 
  fMeta "$=$" [fId d.d_tag; fLongVal (select v d)]

and fIndex v i = fLongVal (subscript v i)

and fLongVal (t, y) =
  if is_record t then
    let r = get_record t in
    fMeta "{ $ }" [fSeq(fField (t, y), "; ") r.r_fields]
  else if is_string t then
    fMeta "\"$\"" [fStr (String.escaped (peek_string (get_addr y)))]
  else if is_array t then
    let b = bound t in
    let n = min (bound t) 10 in
    fMeta "{ $$ }" 
      [fSeq (fIndex (t, y), ", ") (Util.range 0 (n-1));
        if n < b then fStr ", ..." else fStr ""]
  else
    fDynVal (t, y)


let fCall pname regs =
  try
    let d = Info.get_debug pname in
    let p = get_proc d.d_type in
    let args = 
      List.map (fun d -> (d.d_tag, def_value regs.bp d)) p.p_fparams in
    fMeta "$($)" 
      [fStr pname; 
	fList(fun (name, value) -> 
	  fMeta "$=$" [fId name; fDynVal value]) args]
  with Not_found ->
    fMeta "$(no param info)" [fStr pname]

let fFrame regs =
  match find_loc regs with
      Line (m, p, n) -> 
	fMeta "$ [Module $, line $]" 
	  [fCall p regs; fId m; fNum n];
    | Proc p ->
	fCall p regs
    | _ ->
	fStr "???(???)"


(* Printing types *)

(* This is modified from the thing in dict.ml *)

let rec fType1 d t =
  if t.t_name <> anon then 
    fQual (t.t_module, t.t_name)
  else begin
    match t.t_guts with
	BasicType _ -> 
	  fStr "?basic-type?"
      | EnumType n ->
	  fStr "<enumerated type>"
      | PointerType def ->
	  fMeta "POINTER TO $" [fType1 d def.d_type]
      | ArrayType (n, t1) ->
	  fMeta "ARRAY $ OF $" [fNum n; fType1 d t1]
      | FlexType t1 ->
	  fMeta "ARRAY OF $" [fType1 d t1]
      | RecordType r ->
	  if d > 0 then
	    fStr "RECORD ..."
	  else
	    fMeta "RECORD $ END" [fSeq(fFieldType (d+1), "; ") r.r_fields]
      | ProcType p ->
	  fStr "PROCEDURE"
      | _ ->
	  fStr "<a secret internal type>"
  end

and fFieldType d def = 
  fMeta "$: $" [fId def.d_tag; fType1 d def.d_type]

let fType t = fType1 0 t
