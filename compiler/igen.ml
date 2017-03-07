(*
 * igen.ml
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
open Dict
open Gcmap
open Tree
open Mach
open Icode
open Eval
open Error
open Print

let const n = CONST (integer n)

(* Pointer maps *)

let map_name x = sprintf "$.%map" [fSym x]

let make_map sh lab m =
  if m = null_map then "0" else
    try Util.hex_of_int32 (make_bitmap sh m) with
      Not_found -> save_map lab m; lab

let frame_map d =
  make_map Mach.frame_shift (map_name d.d_lab) d.d_map

let type_map t =
  make_map 0 (map_name t.t_desc) t.t_map

let push_map t =
  if t.t_map = null_map then
    const 0
  else if t.t_map = ptr_map then
    (* A pointer type *)
    const 3
  else begin
    try HEXCONST (make_bitmap 0 t.t_map) with
      Not_found ->
	if t.t_desc <> nosym then
	  GLOBAL (map_name t.t_desc)
	else
	  GLOBAL (make_map 0 (genlab ()) t.t_map)
  end


(* Code generation *)

let level = ref 0			(* nesting level of current proc *)

(* size_of -- calculate size of expression in bytes *)
let size_of e = e.e_type.t_rep.m_size

(* count_of -- calculate size of type in words for parameter *)
let count_of t = (t.t_rep.m_size + word_size - 1) / word_size

let roundup n = (n+3)/4*4

(* is_const -- test if expression is a constant *)
let is_const e = 
  match e.e_guts with (Const _ | Nil) -> true | _ -> false

let is_deref e =
  match e.e_guts with Deref _ -> true | _ -> false

(* value_of -- get value of constant *)
let value_of e =
  match e.e_guts with
      Const (v, t) -> v
    | _ -> failwith "value_of"

let expr_line e = line_num e.e_loc

(* op_kind -- machine type for operation *)
let op_kind t = 
  let k = kind_of t in
  match k with 
      FloatT | DoubleT | LongT | BoolT | VoidT -> k 
    | _ -> IntT

(* mem_kind -- machine type for load or store *)
let mem_kind t =
  let k = kind_of t in
  match k with
      (BoolT|SysByteT) -> CharT
    | (PtrT|SetT) -> IntT
    | _ -> k

let load_addr = SEQ [LOAD IntT; XMARK]

let offset n = SEQ [const n; BINOP (PtrT, PlusA)]

let rec schain n =
  if n = 0 then
    LOCAL 0
  else if n = 1 then
    SEQ [LOCAL stat_link; load_addr]
  else
    SEQ [schain (n-1); offset stat_link; load_addr]

(* local -- instructions to push local address *)
let local l o =
  if !level = l then
    LOCAL o
  else
    SEQ [schain (!level - l); offset o]

(* typejump -- code to jump on record type *)
let typejump t tlab flab =
  (* Expects the location of the descriptor address on the stack. *)
  let r = get_record t in
  let lab1 = label () in
  SEQ [LOAD IntT; DUP 0; 
    offset Mach.depth_offset; LOAD IntT;
    const r.r_depth; JUMPC (IntT, Geq, lab1);
    POP 1; JUMP flab;
    LABEL lab1; 
    offset Mach.ancestor_offset; load_addr;
    offset (r.r_depth * word_size); load_addr;
    GLOBAL t.t_desc; JUMPC (PtrT, Eq, tlab); JUMP flab]

let typecheck t e =
 let lab1 = label () and lab2 = label () in
 SEQ [typejump t lab1 lab2;
   LABEL lab2; ERROR ("E_CAST", expr_line e);
   LABEL lab1]

(* convert -- code for type conversion *)
let convert t1 t2 =
  let rec conv k1 k2 =
    if k1 = k2 then 
      NOP
    else begin
      match k1, k2 with
	(* Treat short via integer *)
	  ShortT, _ -> conv IntT k2
	| _, ShortT -> SEQ [conv k1 IntT; CONV (IntT, ShortT)]

        (* Likewise byte *)
        | ByteT, _ -> conv IntT k2
        | _, ByteT -> SEQ [conv k1 IntT; CONV (IntT, ByteT)]

	(* Convert from long to float via double *)
	| LongT, FloatT ->
	    SEQ [CONV (LongT, DoubleT); CONV (DoubleT, FloatT)]

	(* Otherwise convert directly *)
	| (IntT, (LongT | FloatT | DoubleT)) 
        | (LongT, (IntT | DoubleT)) 
        | (FloatT, DoubleT) | (DoubleT, FloatT) 
          -> CONV (k1, k2)

	| _, _ -> failwith "conv" 
    end in
  match t1.t_guts, t2.t_guts with
      BasicType k1, BasicType k2 -> conv k1 k2
    | _, _ -> failwith "convert"

let mark_type t = if is_pointer t then XMARK else NOP

let gen_call pcount rtype =
  SEQ [XSTKMAP pcount; CALL (pcount, op_kind rtype); mark_type rtype]

let call_proc lab pcount rtype =
  SEQ [GLOBAL lab; gen_call pcount rtype]

(* conditional -- test if an expression requires jumping code *)
let rec conditional e =
  match e.e_guts with
    | Monop (Not, e1) -> conditional e1
    | Binop ((And|Or), e1, e2) -> 
	not (safe e2) || conditional e1 || conditional e2
    | TypeTest (e1, tn) -> true
    | _ -> false

(* null_check -- check value of expression is not null *)
let null_check e =
  match e.e_guts with
      Cast (e1, t) -> NOP  (* Casts are automatically non-null *)
    | _ -> CHECK (NullPtr, expr_line e)

(* constant -- code to push a constant *)
let constant k v =
  match k with
      NumT | ShortT | IntT | CharT | BoolT -> CONST (int_value v)
    | FloatT | DoubleT | LongT -> TCONST (k, v)
    | _ -> failwith (sprintf "constant $" [fType1 k])

let check code =
  if !Config.boundchk then code else NOP

let is_vparam x =
  let d = get_def x in d.d_kind = VParamDef

let int_of_bool = function true -> 1 | false -> 0

(* gen_addr -- code to push the address of a variable *)
let rec gen_addr v = 
  match v.e_guts with
      Name x ->
	let d = get_def x in
	(match d.d_kind with
	    VarDef ->
	      if d.d_level = 0 then
		GLOBAL d.d_lab
	      else
		local d.d_level d.d_offset
	  | ParamDef ->
	      if is_flex d.d_type then 
		SEQ [local d.d_level d.d_offset; load_addr]
	      else
		local d.d_level d.d_offset
	  | CParamDef ->
	      if scalar d.d_type then
		local d.d_level d.d_offset
	      else
		SEQ [local d.d_level d.d_offset; load_addr]
	  | VParamDef ->
	      SEQ [local d.d_level d.d_offset; load_addr]
	  | ProcDef ->
	      (* This is needed when a procedure is passed as a parameter
		 of type ARRAY OF SYSTEM.BYTE *)
	      GLOBAL d.d_lab
	  | _ -> failwith "gen_addr")

    | Deref p ->
	SEQ [gen_expr p; check (null_check p)]

    | Sub (_, _) ->
	let e0 = sub_base v in
	let es = subscripts v in
	SEQ [gen_addr e0; gen_subscript e0 es; BINOP (PtrT, PlusA)]

    | Select (r, x) ->
	let d = get_def x in
        SEQ [gen_addr r; offset d.d_offset]

    | String (lab, n) ->
	GLOBAL lab

    | Cast (e1, tn) ->
	let d = get_def tn in
	if not (is_record d.d_type) then failwith "addr of cast";
	begin match e1.e_guts with
	    Name x ->
	      let dx = get_def x in
              if dx.d_kind <> VParamDef then failwith "addr of cast 3";
	      SEQ [
		check (SEQ [
		  local dx.d_level (dx.d_offset + word_size);
                  typecheck d.d_type v]);
		local dx.d_level dx.d_offset; load_addr]

          | Deref p ->
              SEQ [gen_expr p;
                check (SEQ [DUP 0; null_check p;
                  CONST (integer (-word_size)); BINOP (PtrT, PlusA);
                  typecheck (base_type d.d_type) v])]

	  | _ -> failwith "addr of cast 2"
	end

    | _ -> failwith "gen_addr"

(* gen_bound -- generate code to push k'th bound of array *)
and gen_bound k e0 =
  let rec bound i t =
    match t.t_guts with
	ArrayType (n, t1) ->
	  if i = 0 then n else bound (i-1) t1
      | _ -> failwith "gen_bound 2" in
  
  (* Expect the address of e0 on the stack *)
  let f = flexity e0.e_type in
  if k >= f then
    SEQ [POP 1; const (bound (k-f) (flex_base e0.e_type))]
  else begin
    match e0.e_guts with
	Name x ->
	  (* An open array parameter *)
	  let d = get_def x in
	  SEQ [POP 1; 
	    local d.d_level (d.d_offset + (k+1) * word_size); LOAD IntT]
      | Deref p ->
	  SEQ [
	    (* Get descriptor address *)
	    offset (-word_size); load_addr;
	    (* Fetch k'th dimension *)
	    offset (bound_offset + k * word_size); LOAD IntT]
      | _ -> failwith "gen_bound"
  end    

and gen_subscript e0 us =
  let rec loop i ys t =
    match ys with
	[] -> 
	  if not (is_flex t) then
	    SEQ [const t.t_rep.m_size; BINOP (IntT, Times)]
	  else begin
	    SEQ [DUP 1; gen_bound i e0; BINOP (IntT, Times);
	      loop (i+1) [] (base_type t)]
	  end
      | x::xs ->
	  SEQ [DUP 1; gen_bound i e0; BINOP (IntT, Times);
	    gen_expr x;
	    check (SEQ [DUP 2; gen_bound i e0; BOUND (expr_line x)]);
	    BINOP (IntT, Plus);
	    loop (i+1) xs (base_type t)] in
  match us with
      [] -> const 0
    | x::xs ->
	SEQ [gen_expr x; 
	  check (SEQ [DUP 1; gen_bound 0 e0; BOUND (expr_line x)]);
	  loop 1 xs (base_type e0.e_type)]

(* gen_expr -- generate code to push the value of an expression *)
and gen_expr e = 
  if conditional e then
    gen_condval true e
  else begin
    match e.e_guts with
	Const (v, t) -> constant (op_kind t) v

      | Name x ->
	  let d = get_def x in
	  begin match d.d_kind with
	      ProcDef ->
		GLOBAL d.d_lab
	    | _ -> 
		SEQ [gen_addr e; LOAD (mem_kind e.e_type); mark_type e.e_type]
	  end

      | Sub _ | Select _ ->
	  SEQ [gen_addr e; LOAD (mem_kind e.e_type); mark_type e.e_type]

      | Monop (w, e1) ->
	  SEQ [gen_expr e1;
	    if w = Uplus then NOP else MONOP (op_kind e.e_type, w)]

      | Binop ((Eq | Neq | Lt | Leq | Gt | Geq) as w, e1, e2) ->
	  let set_leq e1' e2' =
	    SEQ [gen_expr e1'; gen_expr e2'; 
	      MONOP (IntT, BitNot); BINOP (IntT, BitAnd);
	      const 0; BINOP (IntT, Eq)] in

	  if is_string e1.e_type then
	    SEQ [gen_flexarg strtype e2; gen_flexarg strtype e1;
	      call_proc "COMPARE" 4 boolean;
	      const 0; BINOP (IntT, w)]
	  else if w = Leq && same_types e1.e_type settype then
	    set_leq e1 e2
	  else if w = Geq && same_types e1.e_type settype then
	    set_leq e2 e1
	  else begin
	    let k = op_kind e1.e_type in
	    if is_const e1 && not (is_const e2) then
	      SEQ [gen_expr e2; gen_expr e1; BINOP (k, commute w)]
	    else
	      SEQ [gen_expr e1; gen_expr e2; BINOP (k, w)]
	  end

      | Binop ((Div | Mod) as w, e1, e2) ->
	  let t = op_kind e.e_type in
	  SEQ [gen_expr e1; gen_expr e2;
	    check (CHECK (DivZero t, expr_line e));
	    BINOP (t, w)]

      | Binop (Over, e1, e2) ->
	  let t = op_kind e.e_type in
	  SEQ [gen_expr e1; gen_expr e2;
	    check (CHECK (DivZero t, expr_line e));
	    BINOP (t, Div)]

      | Binop (In, e1, e2) ->
	  SEQ [const 1; gen_expr e1;
	    check (SEQ [const set_size; BOUND (expr_line e)]);
	    BINOP (IntT, Lsl); gen_expr e2;
	    BINOP (IntT, BitAnd); const 0; BINOP (IntT, Neq)]

      | Binop (w, e1, e2) ->
	  let gen_it w' e1' e2' =
	    SEQ [gen_expr e1'; gen_expr e2'; 
	      BINOP (op_kind e.e_type, w')] in
	  if is_const e1 && not (is_const e2) then
	    try let w' = commute w in gen_it w' e2 e1 with 
	      Not_found -> gen_it w e1 e2
	  else
	    gen_it w e1 e2

      | Nil ->
	  const 0

      | Convert e1 ->
	  SEQ [gen_expr e1; convert e1.e_type e.e_type]

      | FuncCall (p, args) -> 
	  gen_proccall p args

      | MethodCall (x, m, args) ->
	  gen_message x m args

      | Cast (e1, tn) ->
	  let d = get_def tn in
	  if not (is_pointer d.d_type) then failwith "val of cast";
	  SEQ [gen_expr e1;
	    check (SEQ [DUP 0; null_check e1; offset (-word_size);
              typecheck (base_type d.d_type) e])]

      | Set els ->
	  if els = [] then
	    const 0
	  else
	    SEQ [gen_element (List.hd els);
	      SEQ (List.map
		(function el -> SEQ [gen_element el; BINOP (IntT, BitOr)])
		(List.tl els))]

      | _ -> failwith "gen_expr"
  end

(* gen_proccall -- generate code to call a procedure *)
and gen_proccall f args =
  match f.e_type.t_guts with
      ProcType p ->
	SEQ [
	  SEQ (List.map2 gen_arg (List.rev p.p_fparams) (List.rev args));
	  gen_funarg LINK f; 
	  check (CHECK (NullPtr, expr_line f));
	  gen_call p.p_pcount p.p_result]

    | BuiltinType b ->
	gen_builtin b args

    | _ -> failwith "gen_proccall"

(* gen_message -- generate code for a method call *)
and gen_message r m args =
  let d = get_def m in
  let p = get_proc d.d_type in
  let rcvr = List.hd p.p_fparams in
  SEQ [
    SEQ (List.map2 gen_arg (List.rev (List.tl p.p_fparams)) (List.rev args));
    (match rcvr.d_kind with
	VParamDef ->
	  SEQ [gen_arg rcvr r;			    (* addr+desc *)
	    DUP 1]				    (* desc *)
      | (ParamDef | CParamDef) ->
	  SEQ [gen_addr r;			    (* addr *)
	    DUP 0; offset (-word_size); LOAD IntT]  (* desc *)
      | _ -> failwith "method receiver");
    offset (method_offset + word_size * d.d_offset); LOAD IntT;
    gen_call p.p_pcount p.p_result]

(*
This table shows how the three kinds of parameters are passed and how
they are treated in the procedure preamble.  Non-scalar value
parameters are passed by address, then copied in the preamble using
the FIXCOPY and FLEXCOPY instructions.

Const parameters are a language extension, implemented here for a long time,
but made official by Wirth in Oberon-07: they are equivalent to
value parameters, except that no copy is made of non-scalar
parameters, and they are not assignable in the procedure body.  The
programmer must ensure that they are not affected by aliasing.

+----------+--------+---------+-------------+---------------+-----------+
|          | SCALAR | ARRAY   | RECORD      | FLEX          | PROC      |
+----------+--------+---------+-------------+---------------+-----------+
| VALUE    | value  | addr    | addr        | addr+bound    | code+stat |
| (Param)  |        | FIXCOPY | FIXCOPY     | FLEXCOPY *    |           |
+----------+--------+---------+-------------+---------------+-----------+
| CONST    | value  | addr *  | addr *      | addr+bound *  | code+stat |
| (CParam) |        |         |             |               |           |
+----------+--------+---------+-------------+---------------+-----------+
| VAR      | addr * | addr *  | addr+desc * | addr+bound *  | addr *    |
| (VParam) |        |         |             |               |           |
+----------+--------+---------+-------------+---------------+-----------+

If the parameter is copied with FIXCOPY, then its definition contains
the offset of the copy.  With FLEXCOPY, the parameter definition still
points to the argument word, but that word is modified at runtime to
contain the address of the alloca'd space.  So the table cells marked
with * are the ones where gen_addr must generate a (LOAD IntT) 
instruction.  
*)

(* gen_arg -- generate code to push a procedure argument *)
and gen_arg f a = 
  if is_proc f.d_type then
    (match f.d_kind with
	ParamDef | CParamDef -> gen_funarg NOP a
      | VParamDef -> gen_addr a
      | _ -> failwith "gen_arg")
  else if scalar f.d_type then
    (match f.d_kind with
	(ParamDef | CParamDef) -> 
	  let s = mem_kind f.d_type in
	  SEQ [gen_expr a;
	    (match s with CharT | ShortT -> ALIGN s | _ -> NOP)]
      | VParamDef -> gen_addr a
      | _ -> failwith "gen_arg")
  else if is_record f.d_type then
    (match f.d_kind with
	ParamDef | CParamDef -> gen_addr a
      | VParamDef -> gen_recarg a
      | _ -> failwith "gen_arg")
  else if is_flex f.d_type then
    gen_flexarg f.d_type a
  else if is_array f.d_type then
    gen_addr a
  else
    failwith "gen_arg"

(* Push a (code, statlink) pair and use inst on the static link *)
and gen_funarg inst a =
  match a.e_guts with
      Name x ->
	let d = get_def x in
	begin match d.d_kind with
	    ProcDef ->
	      SEQ [if d.d_level = 0 then const 0 else local d.d_level 0;
		inst; GLOBAL d.d_lab]
	  | (ParamDef | CParamDef) ->
	      SEQ [local d.d_level (d.d_offset + word_size);
		load_addr; inst;
		local d.d_level d.d_offset; load_addr]
	  | _ ->
	      SEQ [const 0; inst; gen_expr a]
	end
    | _ ->
	SEQ [const 0; inst; gen_expr a]

(* gen_recarg -- push address and descriptor of record *)
and gen_recarg a =
  match a.e_guts with
      Name x when is_vparam x ->
	let d = get_def x in
        SEQ [local d.d_level (d.d_offset + word_size); load_addr;
	        local d.d_level d.d_offset; load_addr]
    | Deref p -> 
	SEQ [gen_expr p;
	  check (null_check p);
	  DUP 0; offset (-word_size); load_addr; SWAP]
    | _ -> 
	SEQ [GLOBAL a.e_type.t_desc; gen_addr a]

(* gen_flexarg -- push addr+bound for flex array arg *)
and gen_flexarg t a =
  if same_types (base_type t) sysbyte then begin
    if not (is_flex a.e_type) then 
      SEQ [const (size_of a); gen_addr a]
    else begin
      let t1 = flex_base a.e_type in
      SEQ [gen_addr a;
	const (t1.t_rep.m_size);
	SEQ (List.map 
	  (fun i -> SEQ [DUP 1; gen_bound i a; BINOP (IntT, Times)])
	  (Util.range 0 (flexity a.e_type - 1)));
        SWAP]
    end
  end else begin
    let e0 = sub_base a in
    let us = subscripts a in
    SEQ [gen_addr e0;
      SEQ (List.map
	(fun i -> SEQ [DUP 0; gen_bound (List.length us + i) e0; SWAP])
	(List.rev (Util.range 0 (flexity t - 1))));
      gen_subscript e0 us; BINOP (PtrT, PlusA)]
  end

(* gen_builtin -- generate code to call a built-in procedure *)
and gen_builtin q args =
  match q.b_id, args with
      ChrFun, [e1] -> SEQ [gen_expr e1; CONV (IntT, CharT)]
    | OrdFun, [e1] -> gen_expr e1
    | OddFun, [e1] -> SEQ [gen_expr e1; const 1; BINOP (IntT, BitAnd)]

    | AshFun, [e1; e2] -> 
        if is_const e2 then begin
          let n = int_of_integer (int_value (value_of e2)) in
          if n >= 0 then
            SEQ [gen_expr e1; const n; BINOP (IntT, Lsl)]
          else
            SEQ [gen_expr e1; const (-n); BINOP (IntT, Asr)]
        end
        else begin
          SEQ [gen_expr e2; gen_expr e1; call_proc "ASH" 2 inttype]
        end

    | LslFun, [e1; e2] -> SEQ [gen_expr e1; gen_expr e2; BINOP (IntT, Lsl)]
    | LsrFun, [e1; e2] -> SEQ [gen_expr e1; gen_expr e2; BINOP (IntT, Lsr)]
    | AsrFun, [e1; e2] -> SEQ [gen_expr e1; gen_expr e2; BINOP (IntT, Asr)]
    | RorFun, [e1; e2] -> SEQ [gen_expr e1; gen_expr e2; BINOP (IntT, Ror)]

    | NewProc, e1::es ->
	let t = base_type e1.e_type in
	begin match t.t_guts with
	    (RecordType _ | ArrayType _) -> 
	      SEQ [const t.t_rep.m_size;
		if t.t_desc = nosym then const 0 else GLOBAL t.t_desc;
                call_proc "NEW" 2 ptrtype; gen_addr e1; STORE IntT]
	  | FlexType _ -> 
 	      let n = flexity t in
 	      let t0 = flex_base t in
	      SEQ [SEQ (List.map gen_expr (List.rev es));
		const n; const t0.t_rep.m_size; push_map t0; 
		call_proc "NEWFLEX" (n+3) ptrtype;
                gen_addr e1; STORE IntT]
	  | _ -> failwith "NewProc"
	end

    | LenFun, v::_ ->
	let n = if List.length args = 1 then 0
	  else int_of_integer (int_value (value_of (List.nth args 1))) in
	let e0 = sub_base v in
	let us = subscripts v in
	let rec loop i ys =
	  match ys with
	      [] -> gen_bound (i+n) e0
	    | (x::xs) ->
		SEQ [gen_expr x; 
		  check (SEQ [DUP 1; gen_bound i e0; BOUND (expr_line x)]); 
		  POP 1; loop (i+1) xs] in
	SEQ [gen_addr e0; loop 0 us]

    | (IncProc | DecProc), e1::_ ->
	begin match op_kind e1.e_type with
	    IntT ->
	      let k = mem_kind e1.e_type in
	      SEQ [gen_addr e1; DUP 0; LOAD k;
		if List.length args = 1 then const 1 else 
		  gen_expr (List.nth args 1);
		if q.b_id = IncProc then 
		  BINOP (IntT, Plus)
		else 
		  BINOP (IntT, Minus);
	      SWAP; STORE k]
	  | LongT ->
	      SEQ [
		if List.length args = 1 then
		  TCONST (LongT, IntVal (integer 1))
		else
		  gen_expr (List.nth args 1);
		gen_addr e1;
		if q.b_id = IncProc then
		  call_proc "INCLONG" 3 longint
		else
		  call_proc "DECLONG" 3 longint]
	  | _ -> failwith "IncProc"
	end

    | (InclProc | ExclProc), [e1; e2] ->
	SEQ [gen_addr e1; DUP 0; LOAD IntT;
          const 1; gen_expr e2;
	  check (SEQ [const set_size; BOUND (expr_line e2)]);
	  BINOP (IntT, Lsl);
	  if q.b_id = InclProc then
	    BINOP (IntT, BitOr)
	  else
	    SEQ [MONOP (IntT, BitNot); BINOP (IntT, BitAnd)];
	  SWAP; STORE IntT]

    | AbsFun, [e1] ->
	let t = op_kind e1.e_type in
	begin match t with
	    IntT -> SEQ [gen_expr e1; call_proc "ABSINT" 1 inttype]
          | LongT -> SEQ [gen_expr e1; call_proc "ABSQUAD" 2 longint]
	  | FloatT -> SEQ [gen_expr e1; call_proc "ABSREAL" 1 realtype]
	  | DoubleT -> SEQ [gen_expr e1; call_proc "ABSLONG" 2 longreal]
	  | _ -> failwith "ABS"
	end

    | PackProc, [e1; e2] ->
        let p = 
          match op_kind e1.e_type with
              FloatT -> "PACK"
            | DoubleT -> "PACKLONG" 
            | _ -> failwith "PACK" in
        SEQ [gen_expr e2; gen_addr e1; call_proc p 2 voidtype]

    | UnpkProc, [e1; e2] ->
        let p = 
          match op_kind e1.e_type with
              FloatT -> "UNPK"
            | DoubleT -> "UNPKLONG" 
            | _ -> failwith "UNPK" in
        SEQ [gen_addr e2; gen_addr e1; call_proc p 2 voidtype]

    | Entier, [e1] ->
	let t = op_kind e1.e_type in
	begin match t with
	    FloatT -> SEQ [gen_expr e1; CONV (FloatT, IntT)]
	  | DoubleT -> SEQ [gen_expr e1; CONV (DoubleT, IntT)]
	  | _ -> failwith "ENTIER"
	end

    | Assert, e1::_ ->
	let lab1 = label () and lab2 = label () in
	check (SEQ [gen_cond lab2 lab1 e1; LABEL lab1;
	  if List.length args = 1 then const 0 else
	    gen_expr (List.nth args 1);
	  EASSERT (expr_line e1); LABEL lab2])

    | AdrFun, [e1] -> gen_addr e1

    | ValFun, [e1; e2] -> gen_expr e2

    | BitFun, [e1; e2] ->
	SEQ [gen_expr e1; LOAD IntT; 
          const 1; gen_expr e2; BINOP (IntT, Lsl); 
	  BINOP (IntT, BitAnd);  const 0; BINOP (IntT, Neq)]

    | GetProc, [e1; e2] ->
	let k = mem_kind e2.e_type in
	SEQ [gen_expr e1; LOAD k; gen_addr e2; STORE k]

    | PutProc, [e1; e2] ->
	let k = mem_kind e2.e_type in
	SEQ [gen_expr e2; gen_expr e1; STORE k]

    | _ -> 
	failwith (sprintf "can't generate code for $ with $ args"
	  [fStr q.b_name; fNum (List.length args)])

(* gen_cond -- generate code to branch on a condition *)
and gen_cond tlab flab test =
  match test.e_guts with
      Const (v, t) ->
	if int_value v <> integer 0 then JUMP tlab else JUMP flab

    | Monop (Not, e) ->
        gen_cond flab tlab e

    | Binop (And, e1, e2) ->
        let lab1 = label () in
	SEQ [gen_cond lab1 flab e1; LABEL lab1; gen_cond tlab flab e2]

    | Binop (Or, e1, e2) ->
	let lab1 = label () in
	SEQ [gen_cond tlab lab1 e1; LABEL lab1; gen_cond tlab flab e2]

    | TypeTest (e1, tn) ->
        SEQ [gen_desc e1; typejump (desc_type tn) tlab flab]

    | _ ->
        SEQ [gen_expr test; const 0; JUMPC (IntT, Neq, tlab); JUMP flab]

and gen_condval sense e =
  match e.e_guts with
      Binop (And, e1, e2) when not (conditional e2) ->
	(* No need to generate jumping code for e2 *)
	let lab0 = label() and lab1 = label () and lab2 = label () in
	SEQ [gen_cond lab1 lab0 e1; 
	  LABEL lab0; const (int_of_bool (not sense)); JUMP lab2;
	  LABEL lab1; gen_expr e2;
	  if sense then NOP else MONOP (BoolT, Not);
	  LABEL lab2]

    | Binop (Or, e1, e2) when not (conditional e2) ->
	(* No need to generate jumping code for e2 *)
	let lab0 = label () and lab1 = label () and lab2 = label () in
	SEQ [gen_cond lab0 lab1 e1;
	  LABEL lab0; const (int_of_bool sense); JUMP lab2;
	  LABEL lab1; gen_expr e2;
	  if sense then NOP else MONOP (BoolT, Not);
	  LABEL lab2]

    | Monop (Not, e1) ->
	gen_condval (not sense) e1

    | _ ->
	let lab0 = label () and lab1 = label () and lab2 = label () in
	SEQ [gen_cond lab0 lab1 e;
          LABEL lab0; const (int_of_bool sense); JUMP lab2;
	  LABEL lab1; const (int_of_bool (not sense)); LABEL lab2]

and gen_desc e =
  if is_pointer e.e_type then
    SEQ [gen_expr e; check (null_check e); offset (-word_size)]
  else if is_record e.e_type then
    (match e.e_guts with
	Name x ->
	  let dx = get_def x in
	  if dx.d_kind <> VParamDef then failwith "type test 2";
	  local dx.d_level (dx.d_offset + word_size)
      | Deref e1 ->
	  SEQ [gen_expr e1; check (null_check e1); offset (-word_size)]
      | _ -> 
	  failwith "type test")
  else
    failwith "type test 3"

and desc_type tn =
  let d = get_def tn in
  if is_pointer d.d_type then
    base_type d.d_type
  else
    d.d_type

and gen_element =
  function
      Single x ->
	SEQ [const 1; gen_expr x;
	  check (SEQ [const set_size; BOUND (expr_line x)]);
	  BINOP (IntT, Lsl)]
    | Range (x, y) ->
	(* {x..y} = {0..y} * {x..31} *)
	SEQ [const (-1); gen_expr x;
	  check (SEQ [const set_size; BOUND (expr_line x)]);
	  BINOP (IntT, Lsl);		(* {x..31} *)
          const (-2); gen_expr y;
	  check (SEQ [const set_size; BOUND (expr_line y)]);
	  BINOP (IntT, Lsl);		(* {y+1..31} *)
	  MONOP (IntT, BitNot);		(* {0..y} *)
	  BINOP (IntT, BitAnd)]

let check_assign v desc =
  let lab1 = label () in
  SEQ [load_addr; GLOBAL desc; JUMPC (PtrT, Eq, lab1);
    ERROR ("E_ASSIGN", expr_line v); LABEL lab1]

let gen_rec_addr v desc =
  match v.e_guts with
      Name x ->
	let d = get_def x in
	if d.d_kind <> VParamDef then 
	  gen_addr v
	else begin
	  SEQ [
	    check (SEQ [local d.d_level (d.d_offset + word_size);
              check_assign v desc]);
	    gen_addr v]
	end

    | Deref p ->
	SEQ [gen_expr p;
	  check (SEQ [check (null_check p);
	    DUP 0; offset (-word_size); check_assign v desc])]

    | _ ->
        gen_addr v

let is_param x =
  let d = get_def x in d.d_kind = ParamDef

(* proc_assign -- assignment to procedure variable *)
let proc_assign v e =
  match v.e_guts with
      Name x when is_param x ->
        SEQ [gen_funarg (check (CHECK (GlobProc, expr_line v))) e;
		gen_addr v; STORE IntT;
                const 0; gen_addr v; offset word_size; STORE IntT]
    | _ ->
        SEQ [gen_funarg (check (CHECK (GlobProc, expr_line v))) e;
		gen_addr v; STORE IntT]

(* gen_stmt -- generate code for a statement *)
let rec gen_stmt exit_lab s =
  let code =
    match s.s_guts with
	Assign (v, e) ->
	  let t = v.e_type in
	  if is_proc t then
            proc_assign v e
	  else if scalar t then
	    SEQ [gen_expr e; gen_addr v; STORE (mem_kind t)]
	  else if is_string_const e then
	    SEQ [gen_flexarg strtype v;
	      gen_flexarg strtype e; call_proc "COPY" 4 voidtype]
	  else if is_flex t then
	    (let t0 = flex_base t in
  	      SEQ [gen_flexarg t v; gen_flexarg t e; 
	        const (flexity t); const t0.t_rep.m_size;
	        call_proc "FLEXASSIGN" (4 + 2 * flexity t) voidtype])
          else if is_flex e.e_type then
            (* Oberon07 form array := flex *)
            (let t0 = flex_base e.e_type in
              SEQ [gen_flexarg e.e_type v; gen_flexarg e.e_type e;
                const 1; const t0.t_rep.m_size;
                call_proc "FLEXASSIGN" 6 voidtype])
	  else
	    SEQ [
	      if is_record t then
		gen_rec_addr v t.t_desc
	      else
		gen_addr v;
	      gen_addr e; const t.t_rep.m_size; FIXCOPY]

      | SimAssign pairs ->
          (* Compute all l-values and r-values first, before doing
             any of the assignments. Consider, e.g., i, a[i] := a[i], i *)
          SEQ [
            SEQ (List.map (fun (e1, e2) ->
              SEQ [gen_expr e2; gen_addr e1]) pairs);
            SEQ (List.map (fun (e1, e2) ->
              let t = e1.e_type in STORE (mem_kind t)) pairs)]

      | ProcCall e ->
	  SEQ [gen_expr e;
	    if size_of e = 0 then NOP else POP (count_of e.e_type)]

      | Return res ->
	  begin 
	    match res with 
		Some e -> 
		  SEQ [gen_expr e; RETURN (op_kind e.e_type)]
	      | None -> 
		  RETURN VoidT
	  end

      | IfStmt (arms, elsept) ->
	  let lab_end = label () in
	  SEQ [
	    SEQ (List.map (fun (cond, thenpt) ->
	      let lab0 = label() and lab1 = label () in
	      SEQ [LINE (expr_line cond); 
                gen_cond lab0 lab1 cond; LABEL lab0;
		gen_stmt exit_lab thenpt; JUMP lab_end;
		LABEL lab1]) arms);
	    gen_stmt exit_lab elsept;
	    LABEL lab_end]

      | CaseStmt (switch, arms, default) ->
	  let lab1 = label () and lab2 = label ()
	  and caselabs = List.map (function _ -> label ()) arms in

	  let make_cases lab (vs, body) =
	    let f =
		function 
		    Single e -> 
		      let v = int_value (value_of e) in (v, v, lab)
		  | Range (e1, e2) -> 
		      let v1 = int_value (value_of e1)
		      and v2 = int_value (value_of e2) in (v1, v2, lab) in
	    List.map f vs 

	  and gen_arm lab (vs, body) =
	    SEQ [LABEL lab; gen_stmt exit_lab body; JUMP lab2] in

	  SEQ [gen_expr switch;
	    Switch.switch 
	      (List.concat (List.map2 make_cases caselabs arms)) lab1;
	    SEQ (List.map2 gen_arm caselabs arms);
	    LABEL lab1;
            gen_else exit_lab "E_CASE" switch.e_loc default;
	    LABEL lab2]

      | WhileStmt arms ->
	  (* Not the best translation, but one that makes it easier
	     to get line numbers right for the debugger *)
	  let lab_top = label () in
	  SEQ [LABEL lab_top;
	    SEQ (List.map (fun (test, body) ->
		let lab1 = label () and lab2 = label () in
		SEQ [LINE (expr_line test); gen_cond lab1 lab2 test;
		  LABEL lab1; gen_stmt exit_lab body; JUMP lab_top;
		  LABEL lab2]) arms)]

      | RepeatStmt (body, test) ->
	  let lab1 = label () and lab2 = label () in
	  SEQ [LABEL lab1; gen_stmt exit_lab body;
	    LINE (expr_line test); gen_cond lab2 lab1 test; LABEL lab2]

      | LoopStmt body ->
	  let lab1 = label () and lab2 = label () in
	  SEQ [LABEL lab1; gen_stmt lab2 body; JUMP lab1; LABEL lab2]

      | ExitStmt ->
	  JUMP exit_lab

      | ForStmt (var, lo, hi, step, body, tmp) ->
	  let lab1 = label () and lab2 = label () in
	  let memk = mem_kind var.e_type in
	  let kind = op_kind var.e_type in
	  let inc =  int_value (value_of step) in
	  let (prep, upb) =
	    match hi.e_guts with 
		Const (_, _) -> (NOP, gen_expr hi)
	      | _ ->
                  (match !tmp with
                      Some d ->
                        ( SEQ [gen_expr hi; LOCAL d.d_offset; STORE memk],
                          SEQ [LOCAL d.d_offset; LOAD memk] ) 
                    | None -> failwith "for bound") in

	  SEQ [prep;
	    (* var := lo *)
	    gen_expr lo; gen_addr var; STORE memk;

	    (* lab1: if var > hi goto lab2 *)
	    LABEL lab1; gen_expr var; upb;
	    JUMPC (kind, (if inc > integer 0 then Gt else Lt), lab2);

	    (* body *)
	    gen_stmt exit_lab body;

	    (* var := var + inc; goto lab1 *)
            LINE (line_num s.s_loc); 
            gen_expr var; constant kind (IntVal inc);
	    BINOP (kind, Plus); gen_addr var; STORE memk;
	    JUMP lab1;
	    
	    LABEL lab2]


      | WithStmt (branches, else_part) ->
	  let labn = label () in

	  let gen_branch (e, tn, body) =
	    let lab1 = label () and lab2 = label () in
	    SEQ [gen_desc e; typejump (desc_type tn) lab2 lab1;
              LABEL lab2; gen_stmt exit_lab body; JUMP labn; 
              LABEL lab1] in

	  SEQ [SEQ (List.map gen_branch branches);
            gen_else exit_lab "E_WITH" s.s_loc else_part; 
            LABEL labn]

      | TypeCase (e, branches, elsept) ->
          let labn = label () in

          let gen_branch (tn, body) =
            let lab1 = label () and lab2 = label () in
            SEQ [DUP 0; typejump (desc_type tn) lab2 lab1;
              LABEL lab2; POP 1; gen_stmt exit_lab body; JUMP labn; 
              LABEL lab1] in

          SEQ [gen_desc e;
            SEQ (List.map gen_branch branches);
            POP 1; gen_else exit_lab "E_CASE" e.e_loc elsept; 
            LABEL labn]

      | Seq ss -> SEQ (List.map (gen_stmt exit_lab) ss)

      | Skip -> NOP

      | ErrStmt ->
	  failwith "igen ErrStmt" in
  SEQ [LINE (line_num s.s_loc); code]	

and gen_else exit_lab ecode loc =
  function
      Some s -> gen_stmt exit_lab s
    | None -> ERROR (ecode, line_num loc)

(* gen_copy -- generate code to copy aggregate value params *)
let gen_copy fps = 
  (* In the case where the parameter is a string constant shorter than 
     the formal, this code copies junk beyond the terminating 0X. In 
     extreme cases, it could cause a segfault. *)
  let copy fp =
    if fp.d_kind <> ParamDef || scalar fp.d_type then NOP else begin
      if is_flex fp.d_type then
	SEQ [LOCAL fp.d_offset; 
	  LOCAL (fp.d_offset + word_size); LOAD IntT;
	  SEQ (List.map (fun i ->
	      SEQ [LOCAL (fp.d_offset + i*word_size); 
		LOAD IntT; BINOP (IntT, Times)]) 
	    (Util.range 2 (flexity fp.d_type)));
	const (flex_base fp.d_type).t_rep.m_size;
	BINOP (IntT, Times); FLEXCOPY]
      else
	SEQ [LOCAL fp.d_offset; LOCAL fp.d_param; LOAD IntT;
	  const fp.d_type.t_rep.m_size; FIXCOPY]
    end in
  SEQ (List.map copy fps)


(* Code output *)

let transform code =
  (* Tail recursion in case code is very long *)
  let rec walk xs zs =
    match xs with
	[] -> List.rev zs
      | XMARK :: ys -> Stack.mark (); walk ys zs
      | XSTKMAP n :: ys ->
	  let m = Stack.make_map n in
	  if m = null_map then walk ys zs else
	    walk ys (STKMAP (make_map 0 (genlab ()) m) :: zs)
      | x :: ys -> Stack.simulate x; walk ys (x :: zs) in
  Stack.reset (); walk code []

let gen_procdef d loc fsize body ret =
  let p = get_proc d.d_type in
  let line = line_num loc in
  level := d.d_level+1;
  let code = SEQ [
    if d.d_level > 0 then SAVELINK else NOP;
    gen_copy p.p_fparams;
    gen_stmt nolab body;
    (match ret with
        Some e -> 
          SEQ [LINE (expr_line e); gen_expr e; RETURN (op_kind e.e_type)]
      | None ->
          if kind_of p.p_result = VoidT then
            RETURN VoidT
          else
            ERROR ("E_RETURN", line))] in

  let code2 = Peepopt.optimise (transform (Icode.canon code)) in
  let stk = Stack.max_depth () in
  let map = frame_map d in
  put "PROC $ $ $ $" [fSym d.d_lab; fNum !fsize; fNum stk; fSym map];
  if loc <> no_loc then Icode.put_line line;
  Icode.output line code2;
  put "END\n" []

(* gen_proc -- generate code for a procedure, ignore other declarations *)
let rec gen_proc = 
  function
      ProcDecl (_, x, _, Block (decls, body, ret, fsize), _) ->
        List.iter gen_proc decls;
	let d = get_def x in
        gen_procdef d x.x_loc fsize body ret
    | PrimDecl (x, _, name, _) ->
	let d = get_def x in
	put "PRIMDEF $ $ 0 $\n" 
	  [fSym d.d_lab; fStr name; fSym (frame_map d)];
    | _ -> ()

(* gen_descriptor -- generate a descriptor *)
let gen_descriptor t =
  put "! Descriptor for $" [fId t.t_name];
  put "DEFINE $" [fSym t.t_desc];
  begin 
    match t.t_guts with
	RecordType r ->
	  if r.r_abstract then
	    put_int 0
	  else begin
	    let alabel = sprintf "$.%anc" [fStr t.t_desc] in
	    put_sym (type_map t);
	    put_int r.r_depth;
	    put_sym alabel;
	    List.iter (function d -> put_sym d.d_lab) r.r_methods;
	    put "" [];
	    put "DEFINE $" [fSym alabel];
	    List.iter (function t' -> put_sym t'.t_desc) 
	      (List.rev (ancestors t))
	  end
      | _ -> 
	  put_sym (type_map t)
  end;
  put "" []

(* translate -- generate code for the whole program *)
let translate stamp 
    (Module (m, imports, body, glodefs, _)) =
  let lcount = if !Config.linecount then num_lines () else 0 in
  put "MODULE $ $ $" [fId m.x_name; fHex stamp; fNum lcount];
  List.iter 
    (function (x, x', st) -> 
      if x' <> intern "SYSTEM" then put "IMPORT $ $" [fId x'; fHex !st]) 
    imports;
  put "ENDHDR\n" [];

  begin match body with
      Block (globals, init, None, fsize) ->
	List.iter gen_proc globals;
	if init.s_guts <> Skip then begin
	  let d = get_def m in
	  gen_procdef d no_loc fsize init None
	end
    | _ ->
	failwith "translate"
  end;

  let vardefs = List.filter (function d -> d.d_kind = VarDef) !glodefs in
  if vardefs <> [] then begin
    put "! Global variables" [];
    List.iter (function d ->
        put "GLOVAR $ $" [fSym d.d_lab; fNum d.d_type.t_rep.m_size])
      vardefs;
    put "" [];

    let mapdefs = List.filter (function d -> d.d_map <> null_map) vardefs in
    if mapdefs <> [] then begin
      put "! Pointer map" [];
      put "DEFINE $.%gcmap" [fId !current];
      List.iter (function d -> put_varmap d.d_lab d.d_map) mapdefs;
      put_sym "GC_END";
      put "" []
    end
  end;

  put_strings ();
  List.iter gen_descriptor (desc_table ());
  put_maps ();

  put "! End of file" []
