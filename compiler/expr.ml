(*
 * expr.ml
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

open Tree
open Symtab
open Dict
open Gcmap
open Print
open Eval
open Error
open Mach

let sem_type t = sem_context "this expression has $" [fOType t]
let sem_type2 t1 t2 =
  sem_context "the left operand has $" [fOType t1];
  sem_context "the right operand has $" [fOType t2]

let expect_args f n loc =
  let arg = 
    match n with 0 -> fStr "no arguments" | 1 -> fStr "1 argument"
      | _ -> fMeta "$ arguments" [fNum n] in
  sem_error "$ expects $" [f; arg] loc

(* err_context -- environment where undeclared names are faked *)
let err_context = ref empty_env

(* fake_def -- add fake definition to environment *)
let fake_def x = 
  let d = { d_tag = x; d_module = !current; d_export = Private; 
    d_kind = VarDef; d_loc = no_loc; d_line = 0; d_type = errtype; 
    d_used = true; d_lab = nosym; d_level = 0; d_offset = 0; 
    d_param = 0; d_comment = None; d_env = empty_env; d_map = null_map } in
  define !err_context d; d

(* lookup_def -- find definition of a name, give error if none *)
let lookup_def x env =
  let d =
    if x.x_module = anon then begin
      try lookup x.x_name env with 
        Not_found -> 
	  sem_error "'$' has not been declared" [fId x.x_name] x.x_loc;
	  x.x_def <- Some (fake_def x.x_name); 
	  raise Not_found
    end
    else begin
      let m = 
	try lookup x.x_module env
	with Not_found -> 
	  sem_error "module '$' has not been imported" 
	    [fId x.x_module] x.x_loc;
	  raise Not_found in
      m.d_used <- true;
      match m.d_kind with
	  ModDef (y, env') ->
	    begin try 
	      let d = lookup x.x_name env' in
	      if d.d_export = Private then raise Not_found;
	      d
	    with Not_found -> 
	      sem_error "module '$' does not export '$'" 
		[fId y; fId x.x_name] x.x_loc;
	      raise Not_found
	    end
	| _ ->
	    sem_error "'$' is not a module" [fId x.x_module] x.x_loc;
	    raise Not_found
    end in
  d.d_used <- true; x.x_def <- Some d; d

(* lookup_typename -- look up a type name *)
let lookup_typename x env =
  let d = lookup_def x env in
  if d.d_kind <> TypeDef then begin
    if not (is_errtype d.d_type) then 
      sem_error "'$' is not a type" [fQualId x] x.x_loc;
    raise Not_found
  end;
  d

(* convert -- force conversion of numeric type *)
let convert e t =
  begin match e.e_guts with
      Const (v, _) ->
        if (kind_of t >= FloatT) then
          edit_expr e (Const (widen v, t))
        else begin
          let vn = int_value v in
          if same_types t shortint then begin
            if int_value v < minshort || int_value v > maxshort then
              sem_warn "the integer value $ does not fit in type SHORTINT"
                [fInteger vn] e.e_loc;
            edit_expr e (Const (IntVal (signext 16 vn), t))
          end
          else if same_types t inttype then begin
            if (int_value v < minint || int_value v > maxint) then
              sem_warn "the integer value $ does not fit in type INTEGER"
                [fInteger vn] e.e_loc;
            edit_expr e (Const (IntVal (signext 32 vn), t))
          end
          else
            edit_expr e (Const (IntVal vn, t))
        end
    | _ ->
        edit_expr e (Convert (copy_expr e))
  end;
  e.e_type <- t


(* shift -- check shift operator *)
let shift f e1 e2 e =
  begin match e1.e_guts, e2.e_guts with
      Const (x, _), Const (y, _) ->
	let n = int_of_integer (int_value y) in
	edit_expr e (Const (IntVal (f (int_value x) n), e1.e_type))
    | _, _ -> ()
  end;
  inttype

let joinable t1 t2 =
  if !Config.ob07flag then
    integral t1 && integral t2 || floating t1 && floating t2
  else
    numeric t1 && numeric t2

let coerceable t1 t2 =
  if !Config.ob07flag then
    integral t1 && integral t2 || floating t1 && floating t2
  else
    numeric t1 && numeric t2 && kind_of t1 <= kind_of t2

(* coerce -- convert expression to a given numeric type *)
let coerce e t =
  if kind_of e.e_type <> kind_of t then convert e t

(* is_var -- check that expression denotes a variable *)
let rec is_var e =
  match e.e_guts with
      Name x ->
	if undefined x then
	  true
	else begin
	  let d = get_def x in
	  match d.d_kind with 
	      VarDef ->
                (* In Oberon-07, imported variables are read-only,
                   even if they were exported read-write from another
                   (non-Oberon-07) module *)
		d.d_module = !current 
                  || d.d_export = Visible && not !Config.ob07flag
	    | ParamDef -> true
	    | CParamDef -> false
	    | VParamDef -> true
	    | _ -> false
	end
    | Deref p -> true
    | Sub (a, i) -> is_var a
    | Select (r, x) ->
	if undefined x then
	  true
	else begin
	  let d = get_def x in 
	  is_var r && (d.d_module = !current || d.d_export = Visible)
	end
    | Cast (e, tn) ->
	if undefined tn then
	  true
	else begin
	  let d = get_def tn in
	  is_var e && is_record d.d_type
	end
    | _ -> false

(* is_char_const -- test if expression is a character constant *)
let is_char_const e =
  match e.e_guts with
      Const (_, t) -> same_types t character
    | _ -> false

let promote_char e =
  match e.e_guts with
      Const (c, _) ->
        let s = String.make 1 (char_of_integer (int_value c)) in
	edit_expr e (String (save_string s, 1));
 	e.e_type <- new_type 0 (row 2 character)
    | _ -> failwith "promote_char"

(* find_method -- search for method *)
let rec find_method t x =
  match t.t_guts with
      RecordType r ->
	begin 
	  try find_def x r.r_methods with
	    Not_found ->
	      if r.r_depth > 0 then
	        find_method r.r_parent x
	      else
	        raise Not_found
	end
    | _ -> raise Not_found

(* find_field -- look up field or method *)
let rec find_field x t =
  match t.t_guts with
      RecordType r ->
	begin try find_def x r.r_fields
	  with Not_found -> find_def x r.r_methods end
    | _ -> raise Not_found

(* get_module_ref -- find if an expr is actually a module name *)
let get_module_ref env e =
  match e.e_guts with
      Name x ->
	if x.x_module <> anon then
	  raise Not_found
	else begin
	  let d = lookup x.x_name env in
	  match d.d_kind with
	      ModDef (y, m) -> x.x_name
	    | _ -> raise Not_found
	end
    | _ -> raise Not_found

(* check_qual -- convert Select to qualified name *)
let check_qual e env =
  match e.e_guts with
      Select (e1, x) ->
	begin try
	  let m = get_module_ref env e1 in
	  edit_expr e (Name (makeName (m, x.x_name, e.e_loc)))
	with Not_found -> ()
	end
    | _ -> ()
	  
let dynamic e =
  if is_errtype e.e_type then
    true
  else if is_record e.e_type then begin
    match e.e_guts with
	Name x ->
	  undefined x || (let d = get_def x in d.d_kind = VParamDef)
      | Deref _ ->
	  true
      | _ ->
	  false
  end
  else if is_pointer e.e_type then begin
    is_record (base_type e.e_type) || is_errtype (base_type e.e_type)
  end
  else
    false

(* check_typetest -- common checks for type test, cast, WITH statement *)
let check_typetest e t2 loc =
  if not (dynamic e) then begin
    sem_error "a record pointer or VAR parameter is needed here" [] loc;
    if not (is_record e.e_type) then sem_type e.e_type
  end
  else if not (subtype t2 e.e_type) then begin
    sem_error "a supertype of $ is needed here" [fOType t2] e.e_loc;
    sem_type e.e_type
  end

(* check_super -- find method for super call *)
let check_super e p loc =
  if p.p_kind <> Method then begin
    (* Not a super call after all! *)
    sem_error "a pointer is needed here" [] loc;
    sem_type e.e_type;
    errtype
  end
  else begin
    match e.e_guts with
	Select (e1, x) ->
	  let r = get_record e1.e_type in
	  begin try
	    let d = find_field x.x_name r.r_parent in
	    if d.d_kind = FieldDef then raise Not_found;
 	    let pp = get_proc d.d_type in
 	    if pp.p_kind = AbsMeth then
 	      sem_error "method '$' is declared abstract in $"
 		[fId x.x_name; fOType r.r_parent] loc;
	    x.x_def <- Some d; d.d_type
	  with Not_found ->
	    sem_error ("in this super call, the parent $ does not"
	      ^ " support method '$'") 
	      [fOType r.r_parent; fId x.x_name] loc;
	    errtype
	  end
      | _ ->
	  failwith "check_super"
  end

exception Non_proc

let check_deref e =
  if is_pointer (e.e_type) then begin
    edit_expr e (Deref (copy_expr e));
    e.e_type <- base_type (e.e_type)
  end;
  e.e_type

let const_value e =
  match e.e_guts with
      Const (v, _) -> v
    | _ -> raise Not_found

let op_of e =
  match e.e_guts with
      Binop (w, _, _) -> w
    | Monop (w, _) -> w
    | _ -> failwith "op_of"

let type_mismatch cxt args lt e =
  if approx_same lt e.e_type then       
    sem_error "types do not match exactly $" [fMeta cxt args] e.e_loc
  else
    sem_error "$ is needed $" [fOType lt; fMeta cxt args] e.e_loc;
  sem_type e.e_type

(* check_desig -- check and annotate a designator, return its type *)
let rec check_desig e env =
  let t = check_desig1 e env in
    e.e_type <- t; t

and check_desig1 e env =
  match e.e_guts with
      Name x -> 
        begin try
          let d = lookup_def x env in 
          begin match d.d_kind with
              ConstDef v ->
                edit_expr e (Const (v, d.d_type))
            | EnumDef n ->
                edit_expr e (Const (IntVal (integer n), d.d_type))
            | StringDef ->
                let n = bound d.d_type in
                edit_expr e (String (d.d_lab, n-1))
            | (VarDef | ParamDef | CParamDef | VParamDef | 
                ProcDef | PrimDef) -> ()
            | _ ->
                sem_error "'$' is not a variable" [fQualId x] x.x_loc;
                raise Not_found
          end;
          d.d_type
        with Not_found -> errtype
        end
    | Deref e1 ->
        let t1 = check_desig e1 env in
        begin match t1.t_guts with
            PointerType d -> d.d_type
          | ProcType p1 -> check_super e1 p1 e.e_loc
          | _ -> 
              if not (is_errtype t1) then begin
                sem_error "a pointer is needed here" [] e1.e_loc;
                sem_type t1
              end;
              errtype
        end
    | Sub (e1, e2) ->
        let t1r = check_desig e1 env in
        let t1 = check_deref e1 in
        let t2 = check_expr e2 env in
        let t0 = 
          match t1.t_guts with
              ArrayType (upb, u1) -> u1
            | FlexType u1 -> u1
            | _ -> 
                if not (is_errtype t1) then begin
                  sem_error "an array is needed here" [] e1.e_loc;
                  sem_type t1r
                end;
                errtype in
        if not (integral t2) then begin
          sem_error "a subscript must be an integer" [] e2.e_loc;
          sem_type t2
        end;
        if same_types t2 longint && not (is_errtype t2) then
          sem_error "sorry, LONGINT subscripts are not implemented" 
            [] e2.e_loc;
        t0
    | Select (e1, x) ->
        begin try 
          let m = get_module_ref env e1 in
          edit_expr e (Name (makeName (m, x.x_name, e.e_loc)));
          check_desig e env
        with Not_found ->
          let t1r = check_desig e1 env in
          let t1 = check_deref e1 in
          if not (is_record t1) then begin
            if not (is_errtype t1) then begin
              sem_error "a record or record pointer is needed here" 
                [] e1.e_loc;
              sem_type t1r
            end;
            errtype
          end
          else begin
            try
              let d = find_field x.x_name t1 in
              x.x_def <- Some d; d.d_type
            with Not_found ->
              sem_error
                "this record does not have a visible field called '$'" 
                [fId x.x_name] e1.e_loc;
              sem_type t1r;
              errtype
          end
        end
    | FuncCall _ ->
        let t = check_expr e env in
        begin match e.e_guts with
            (FuncCall _ | MethodCall _) -> 
              sem_error "a function call is not allowed here" 
                [] e.e_loc;
              errtype
          | Cast _ -> t
          | _ -> failwith "desig call"
        end
    | _ -> failwith "desig"

(* check_expr -- check and annotate an expression, return its type *)
and check_expr e env =
  let t = check_subexp e env in
  e.e_type <- t; t

and check_subexp e env =
  match e.e_guts with
      Name _ | Deref _ | Sub _ | Select _ ->
	check_desig e env
    | Const (v, t) -> t
    | Decimal d ->
	(* OCaml wrongly allows 2^31 as a valid integer *)
	let v = 
	  try integer_of_string d with Failure _ -> integer (-1) in
	let v' = 
	  if v >= integer 0 then v else begin
	    sem_error "constant does not fit in 32 bits" [] e.e_loc;
	    integer_of_string "0x7fffffff" 
	  end in
	edit_expr e (Const (IntVal v', numtype)); numtype
    | String (lab, n) -> new_type 0 (row (n+1) character)
    | Nil -> niltype
    | FuncCall (p, args) -> 
	let t = check_call p args e env true in
	if not (same_types t voidtype) then
	  t
	else begin
	  if not (is_errtype t) then
	    sem_error "this procedure may not be used as a function" 
	      [] e.e_loc;
	  errtype
	end
    | Monop (Uminus, { e_guts = Decimal d }) ->
	let v =
	  try integer_of_string ("-" ^ d) with Failure _ ->
	    sem_error "constant does not fit in 32 bits" [] e.e_loc;
	    integer_of_string "0x80000000" in
	edit_expr e (Const (IntVal v, numtype)); numtype
    | Monop (w, e1) -> 
	let t = check_monop env w e1 e in
	if not (is_errtype t) then begin 
	  match e1.e_guts with
	      Const (x1, _) -> 
		edit_expr e (Const (do_monop (op_of e) x1, t))
	    | _ -> ()
	end;
	t
    | Binop (w, e1, e2) -> 
	let t = check_binop env w e1 e2 e in
	if not (is_errtype t) then begin
	  match e1.e_guts, e2.e_guts with
	      Const (x1, _), Const (x2, _) -> 
	        let v = try 
		    (* The expression may have been edited, so use new op *)
		    do_binop (op_of e) x1 x2
		  with 
		      Division_by_zero ->
			sem_error "this expression divides a constant by zero" 
			  [] e.e_loc;
			intval 0
		    | Bound_error ->
			sem_error 
			  "this constant expression causes a bound error"
			  [] e.e_loc;
			intval 0 in
		edit_expr e (Const (v, t))
	    | _ -> ()
	end;
	t

    | TypeTest (e1, tn) ->
	begin try
	  let _ = check_expr e1 env in
	  let d = lookup_typename tn env in
	  check_typetest e1 d.d_type e1.e_loc
	with Not_found -> ()
	end;
	boolean
    | Set els ->
        let check_elem =
          function
              Single x ->
                check_assign x inttype env "in this set element" []
            | Range (x, y) ->
                check_assign x inttype env "in this set element" [];
                check_assign y inttype env "in this set element" []

        and set_const els = 
          let set_val =
            function
                Single x -> 
                  bit_range (int_value (const_value x)) 
                    (int_value (const_value x))
              | Range (x, y) -> 
                  bit_range (int_value (const_value x)) 
                    (int_value (const_value y)) in
          List.fold_left integer_bitor (integer 0) 
            (List.map set_val els) in

	  List.iter check_elem els; 
	  ( try let v = set_const els in 
	      edit_expr e (Const (IntVal v, settype))
	    with Not_found -> () );
	  settype
    | _ -> failwith "subexp"

(* check_monop -- check application of unary operator *)
and check_monop env w e1 e =
  let t1 = check_expr e1 env in
  match w with
      Uminus | Uplus ->
	if numeric t1 then
	  t1
	else if same_types t1 settype then begin
	  if w = Uminus then edit_expr e (Monop (BitNot, e1));
	  settype
	end
	else begin
	  if not (is_errtype t1) then begin
	    sem_error "the operand of $ must be numeric" [fOp w] e1.e_loc;
	    sem_type t1
	  end;
	  inttype
	end
    | Not ->
	if not (same_types t1 boolean) then begin
	  sem_error "the operand of $ must have type BOOLEAN" [fOp w] e1.e_loc;
	  sem_type t1
	end;
	boolean
    | _ -> failwith "bad monop"

(* check_binop -- check application of binary operator *)
and check_binop env w e1 e2 e =
  let t1 = check_expr e1 env and t2 = check_expr e2 env in
  match w with
      Plus | Minus | Times ->
	if joinable t1 t2 then begin
	  let t = join_type t1 t2 in
	  coerce e1 t; coerce e2 t; t
	end
	else if same_types t1 settype && same_types t2 settype then begin
	  let w' = match w with Plus -> BitOr | Minus -> BitSub 
	    | Times -> BitAnd | _ -> failwith "set op" in
	  edit_expr e (Binop (w', e1, e2));
	  settype
	end
	else begin
	  if not (is_errtype t1) && not (is_errtype t2) then begin
	    sem_error "the operands of $ must be $ or both sets" 
	      [fOp w; fStr (if !Config.ob07flag then
                "both integers or both real" else "both numeric")] e.e_loc;
	    sem_type2 t1 t2
	  end;
	  errtype
        end
    | Over ->
	if coerceable t1 longreal && coerceable t2 longreal then begin
	  let t = join_type realtype (join_type t1 t2) in
	  coerce e1 t; coerce e2 t; t
	end
	else if same_types t1 settype && same_types t2 settype then begin
	  edit_expr e (Binop (BitXor, e1, e2));
	  settype
	end
	else begin
	  if not (is_errtype t1) && not (is_errtype t2) then begin
	    sem_error "the operands of / must be both real or both sets" 
	      [] e.e_loc;
	    sem_type2 t1 t2
	  end;
	  errtype
	end
    | Div | Mod ->
	if integral t1 && integral t2 then begin
	  let t = join_type t1 t2 in
	  coerce e1 t; coerce e2 t; t
	end
	else begin
	  if not (is_errtype t1) && not (is_errtype t2) then begin
	    sem_error "the operands of $ must be integers" [fOp w] e.e_loc;
	    sem_type2 t1 t2
	  end;
	  errtype
	end
    | Eq | Neq | Lt | Gt | Leq | Geq ->
	if joinable t1 t2 then begin
	  let t = join_type t1 t2 in
	  coerce e1 t; coerce e2 t
	end
	else if is_string t1 && is_char_const e2 then
	  promote_char e2
	else if is_char_const e1 && is_string t2 then
	  promote_char e1
	else if (w = Eq || w = Neq) && is_address t1 && is_niltype t2 then
	  e2.e_type <- t1
	else if (w = Eq || w = Neq) && is_niltype t1 && is_address t2 then
	  e1.e_type <- t2
	else if not (is_discrete t1 && same_types t1 t2
	    || is_string t1 && is_string t2
	    || (w = Leq || w = Geq) && 
		  same_types t1 settype && same_types t2 settype
	    || (w = Eq || w = Neq) && scalar t1 && 
		  (subtype t1 t2 || subtype t2 t1)) then begin
	  if not (is_errtype t1) && not (is_errtype t2) then begin
	    sem_error "the operands of $ have incompatible types" 
	      [fOp w] e.e_loc;
	    sem_type2 t1 t2
	  end
	end;
	boolean
    | In ->
	if not (integral t1 && same_types t2 settype) then begin
	  if not (is_errtype t1) && not (is_errtype t2) then begin
	    sem_error "the operands of $ must be an integer and a set" 
	      [fOp w] e.e_loc;
	    sem_type2 t1 t2
	  end
	end;
	boolean
    | And | Or ->
        if not (same_types t1 boolean && same_types t2 boolean) then begin
	  sem_error "the operands of $ must have type BOOLEAN" [fOp w] e.e_loc;
	  sem_type2 t1 t2
	end;
	boolean
    | _ -> failwith "bad binop"

and check_call f args e env cast_ok =
  let t = check_desig f env in
  match t.t_guts with
      ProcType p ->
	begin match p.p_kind with
	    Procedure ->
	      check_args "procedure" env p.p_fparams args f.e_loc
	  | Method | AbsMeth ->
	      check_message env p.p_fparams f args e
	  | _ -> failwith "check_call"
	end;
	p.p_result
    | BuiltinType q ->
	check_builtin env q args e f.e_loc
    | _ ->
	(* Maybe it's a cast? *)
        begin try
	  (* raise Non_proc if it isn't a cast after all *)
	  if not cast_ok || List.length args <> 1 then raise Non_proc;
          let tn = List.hd args in
	  check_qual tn env;
	  match tn.e_guts with
	      Name x ->
		let d = lookup_def x env in
		if d.d_kind <> TypeDef then raise Non_proc;
		edit_expr e (Cast (f, x));
		check_typetest f d.d_type f.e_loc;
		if is_errtype f.e_type then errtype else d.d_type
	    | _ -> raise Non_proc
	with 
	    Non_proc ->
	      if is_errtype t then
		incr Error.err_count (* Note an error to suppress inicheck *)
	      else begin
		sem_error "a procedure is needed here" [] f.e_loc;
		sem_type t
	      end;
	      errtype
	  | Not_found ->
	      errtype
	end

and check_message env formals f args e =
  match f.e_guts with
      Select (r, m) ->
	check_methargs env formals r args f.e_loc;
	if dynamic r then
	  edit_expr e (MethodCall (r, m, args))
	else if not (undefined m) then begin
	  let d = get_def m in
	  let f1 = makeExpr (Name m, f.e_loc) in
	  f1.e_type <- d.d_type;
	  edit_expr e (FuncCall(f1, r::args))
        end
    | Deref e1 ->
	(* A super call *)
	begin match e1.e_guts with
	    Select (r, m) ->
	      check_methargs env formals r args f.e_loc;
	      if not (undefined m) then begin
		let rcvr = List.hd formals in
		let d = get_def m in
		let f1 = makeExpr (Name m, e1.e_loc) in
		f1.e_type <- d.d_type;
		let e' =
		  match rcvr.d_kind, r.e_guts with
		      (CParamDef | ParamDef), Deref e1 -> e1
		    | VParamDef, _ -> r
		    | _, _ -> failwith "super_arg" in
		edit_expr e (FuncCall (f1, e' :: args))
	      end
	  | _ -> failwith "message2"
	end
    | _ -> failwith "message"

and check_methargs env formals rcvr args loc =
  let frcvr = List.hd formals in
  begin
    match frcvr.d_kind with
	(ParamDef | CParamDef) ->
	  begin
	    match rcvr.e_guts with
		Deref _ -> ()
	      | _ -> 
		sem_error "a pointer is needed for this method call" 
		  [] rcvr.e_loc;
		sem_type rcvr.e_type;
	  end
      | VParamDef ->
	  if not (is_var rcvr) then
	    sem_error "this VAR receiver should be a variable" [] rcvr.e_loc
      | _ ->
	  failwith "check_methargs"
  end;
  check_args "method" env (List.tl formals) args loc

and check_args kind env formals args loc =
  if List.length formals <> List.length args then
    expect_args (fMeta "this $" [fStr kind]) (List.length formals) loc
  else
    List.iter (check_arg env) (List.combine formals args)

and check_arg env (formal, arg) =
  if is_flex formal.d_type then begin
    let t1 = check_expr arg env in
    if is_string formal.d_type && is_char_const arg then
      promote_char arg
    else if not (array_match t1 formal.d_type) 
	&& not (same_types (base_type formal.d_type) sysbyte) then begin
      sem_error "open array parameter '$' should have $" 
	[fId formal.d_tag; fOType formal.d_type] arg.e_loc;
      sem_type t1
    end
  end
  else begin
    match formal.d_kind with
	(ParamDef | CParamDef) ->
          check_assign1 arg formal.d_type env false
            "as argument '$' of this procedure call" [fId formal.d_tag]
      | VParamDef ->
	  let t1 = check_expr arg env in
 	  if not (same_types t1 formal.d_type || is_record formal.d_type 
 				&& subtype t1 formal.d_type) then begin
	    if approx_same t1 formal.d_type then
	      sem_error "VAR parameter '$' does not have the same type"
		[fId formal.d_tag] arg.e_loc
	    else
	      sem_error "VAR parameter '$' should have $" 
		[fId formal.d_tag; fOType formal.d_type] arg.e_loc;
	    sem_type t1
	  end
      | _ -> failwith "check_arg"
  end;

  if formal.d_kind = VParamDef && not (is_var arg) then
    sem_error "VAR parameter '$' should be a variable" 
      [fId formal.d_tag] arg.e_loc;

and check_builtin env p args e loc =
  (* p.b_nargs = -1 if the primitive has a variable number of arguments;
     otherwise it is the number of arguments.  p.b_argtypes = [] if the
     argument types can vary; otherwise it is a list of arg types. *)
  if p.b_nargs >= 0 && List.length args <> p.b_nargs then begin
    expect_args (fStr p.b_name) p.b_nargs loc;
    errtype
  end
  else begin
    if p.b_argtypes <> [] then begin
      let check (e, t) =
	check_assign e t env "as an argument of $" [fStr p.b_name] in
      List.iter check (List.combine args p.b_argtypes)
    end;

    let propagate f e1 t1 =
      begin match e1.e_guts with
	  Const (v, _) -> edit_expr e (Const (f v, t1))
        | _ -> () 
      end;
      t1 in

    let typeconv t1 e1 =
      convert e1 t1; edit_expr e e1.e_guts; t1 in

    let check_var test reqd e =
      let t = check_expr e env in
      if not (test t) then begin
        sem_error "the argument of $ must be $ variable"
	  [fStr p.b_name; fStr reqd] e.e_loc;
	sem_type t
      end
      else if not (is_var e) then
        sem_error "the argument of $ must be a variable" 
          [fStr p.b_name] e.e_loc;
      t in

    match p.b_id, args  with 

	ChrFun, [e1] ->
	  let chr v = 
	    IntVal (integer_bitand (int_value v) (integer 255)) in
	  propagate chr e1 character

      | OrdFun, [e1] ->
	  let t1 = check_expr e1 env in
	  if not (is_discrete t1) && not (same_types t1 settype)
              || same_types t1 longint then begin
	    let reqd = if !Config.extensions then 
	        "a discrete type" else "type CHAR" in
	    sem_error "the argument of ORD must have $"
	      [fStr reqd] e1.e_loc;
	    sem_type t1
	  end 
	  else if not (same_types t1 character) && not (same_types t1 settype)
	      && not !Config.extensions then begin
	    sem_extend "ORD expects an argument of type CHAR" [] e1.e_loc;
	    sem_type t1
	  end;

	  let id x = x in
	  propagate id e1 inttype

      | OddFun, [e1] ->
	  let odd v = IntVal (integer_mod (int_value v) (integer 2)) in
	  propagate odd e1 boolean

      | Entier, [e1] ->
	  let t1 = check_expr e1 env in
	  if not (floating t1) then begin
	    sem_error "the argument of $ must be have a real type" 
	      [fStr p.b_name] e1.e_loc;
	    sem_type t1
	  end;
	  inttype

      | Short, [e1] ->
	  let t1 = check_expr e1 env in
	  if same_types t1 inttype || same_types t1 numtype then 
	    typeconv shortint e1
	  else if same_types t1 longint then
	    typeconv inttype e1
	  else if same_types t1 longreal then
	    typeconv realtype e1
	  else begin
	    sem_error ("the argument of SHORT must have type"
	      ^ " INTEGER, LONGINT or LONGREAL") [] e1.e_loc;
	    sem_type t1;
	    errtype
	  end

      | Long, [e1] ->
	  let t1 = check_expr e1 env in
	  if subtype t1 shortint then
	    typeconv inttype e1
	  else if same_types t1 inttype || same_types t1 numtype then
	    typeconv longint e1
	  else if same_types t1 realtype then
	    typeconv longreal e1
	  else begin
	    sem_error ("the argument of LONG must have type"
	      ^ " SHORTINT, INTEGER or REAL") [] e1.e_loc;
	    sem_type t1;
	    errtype
	  end

      | FltFun, [e1] -> typeconv realtype e1

      | LslFun, [e1; e2] -> shift integer_lsl e1 e2 e
      | LsrFun, [e1; e2] -> shift integer_lsr e1 e2 e
      | AsrFun, [e1; e2] -> shift integer_asr e1 e2 e
      | RorFun, [e1; e2] -> shift integer_ror e1 e2 e
      | AshFun, [e1; e2] -> 
	  let f x n = 
	    if n >= 0 then integer_lsl x n 
	    else integer_asr x (-n) in
  	  shift f e1 e2 e

      | AbsFun, [e1] ->
	  let t1 = check_expr e1 env in
	  if not (numeric t1) then begin
	    sem_error "ABS needs a numeric argument" [] 
	      (List.nth args 0).e_loc;
	    sem_type t1;
	    inttype
	  end
	  else begin 
	    let abs = function
	        IntVal n -> 
		  IntVal (if n >= integer 0 then n else integer_neg n)
	      | FloVal x -> 
		  FloVal (if x >= 0.0 then x else -. x) in
	    propagate abs e1 t1
	  end

      | (MinFun | MaxFun), [e1] ->
	  let fail () =
	    let reqd = if !Config.extensions then 
				"basic or enumerated" else "basic" in
	      sem_error "the argument of $ must be a $ type"
		[fStr p.b_name; fStr reqd] e1.e_loc in
	  check_qual e1 env;
	  begin match e1.e_guts with
	      Name x ->
		begin try 
		  let d = lookup_typename x env in
		  let (y, z, t) =
		    match d.d_type.t_guts with
		        BasicType CharT ->
			  (IntVal minchar, IntVal maxchar, character)
		      | BasicType BoolT ->
			  (IntVal (integer 0), IntVal (integer 1), inttype)
		      | BasicType SetT ->
			  (IntVal (integer 0), 
			    IntVal (integer (set_size-1)), inttype)
		      | BasicType ShortT ->
			  (IntVal minshort, IntVal maxshort, shortint)
		      | BasicType IntT ->
			  (IntVal minint, IntVal maxint, inttype)
		      | BasicType LongT ->
			  (IntVal minlong, IntVal maxlong, longint)
		      | BasicType FloatT ->
			  (FloVal minreal, FloVal maxreal, realtype)
		      | BasicType DoubleT ->
			  (FloVal mindouble, FloVal maxdouble, longreal)
		      | EnumType n ->
			  (IntVal (integer 0), 
			    IntVal (integer (n-1)), d.d_type)
		      | _ -> 
			  fail ();
			  (IntVal (integer 0), IntVal (integer 0), inttype) in
		  edit_expr e (Const ((if p.b_id = MinFun then y else z), t));
		  t
		with Not_found -> errtype
		end
	    | _ -> 
		fail ();
		errtype
	  end

      | SizeFun, [e1] ->
	  check_qual e1 env;
	  begin match e1.e_guts with
	      Name x ->
		begin try
		  let d = lookup_typename x env in
		  edit_expr e
		    (Const (IntVal (integer d.d_type.t_rep.m_size), 
		      inttype))
		with Not_found -> ()
		end
	    | _ ->
		sem_error "the argument of SIZE must be a type name"
		  [] e1.e_loc
	  end;
	  numtype

      | NewProc, _ ->
	  if List.length args = 0 then
	    sem_error "NEW expects 1 or 2 arguments" [] loc
	  else begin
	    let e1 = List.nth args 0 in
            let t1 = check_var is_pointer "a pointer" e1 in
            if is_pointer t1 then begin
	      let tb = base_type t1 in
	      match tb.t_guts with
 		  RecordType r ->
 		    if List.length args <> 1 then
 		      sem_error "NEW expects 1 argument" [] loc;
 		    if r.r_abstract then begin
 		      sem_error
                        "cannot create instance of abstract record type"
                        [] e1.e_loc;
                      sem_type t1
                    end
 		| ArrayType _ ->
		    if List.length args <> 1 then
		      sem_error "NEW expects 1 argument" [] loc
		| FlexType _ ->
		    let dim = flexity tb and tbb = flex_base tb in
		    if List.length args <> dim+1 then begin
		      if not (is_errtype tbb) then
			sem_error "NEW expects $ arguments" [fNum (dim+1)] loc
		    end
		    else begin
		      for i = 0 to dim-1 do
			let e2 = List.nth args (i+1) in
			check_assign e2 inttype env "as bound of NEW" []
		      done
		    end;
		| _ -> 
		    (* We have already complained about a bad target type *)
		    ()
	    end
	  end;
	  voidtype

      | LenFun, _ ->
	  if List.length args < 1 || List.length args > 2 then begin
	    sem_error "LEN requires 1 or 2 arguments" [] e.e_loc;
	    inttype
	  end
	  else begin
	    let e1 = List.nth args 0 in
	    let t1 = check_expr e1 env in
	    let check n t =
	      let rec loop i t =
	        match t.t_guts with
		    ArrayType (k, t2) -> 
		      if i > 0 then
			loop (i-1) t2
		      else begin
			if (safe e1) then
  			  edit_expr e (Const (IntVal (integer k), inttype));
			inttype
		      end
		  | FlexType t2 ->
		      if i > 0 then
			loop (i-1) t2
		      else
			inttype
		  | _ -> 
		      if not (is_errtype t1) then begin
			if n = 0 then
			  sem_error "the argument of LEN must be an array"
			    [] e1.e_loc
			else
			  sem_error ("the argument of LEN must be an array" ^
			    " of at least $ dimensions") [fNum (n+1)] e1.e_loc;
			sem_type t1
		      end;
		      inttype in
              loop n t in
	    if List.length args = 1 then
	      check 0 t1
	    else begin
	      let v = check_tconst (List.nth args 1) inttype env
			"this argument of LEN" in
	      let n = int_of_integer (int_value v) in
	      if n >= 0 then
		check n t1
	      else begin
		sem_error 
		  "this argument of LEN should be a non-negative integer"
		  [] (List.nth args 1).e_loc;
		inttype
	      end
	    end
  	  end

      | (IncProc | DecProc), _ ->
	  if List.length args < 1 || List.length args > 2 then
	    sem_error "$ expects 1 or 2 arguments" [fStr p.b_name] e.e_loc
          else begin
            let e1 = List.nth args 0 in
            let t1 = check_var integral "an integer" e1 in
            if List.length args = 2 && integral t1 then begin
              let e2 = List.nth args 1 in
              check_assign e2 t1 env
                "as the second argument of $" [fStr p.b_name]
            end
          end;
	  voidtype

      | PackProc, [e1; e2] ->
          ignore (check_var floating "a real" e1);
          check_assign e2 inttype env "as argument of PACK" [];
          voidtype

      | UnpkProc, [e1; e2] ->
          ignore (check_var floating "a real" e1);
          ignore (check_var (same_types inttype) "an integer" e2);
          voidtype

      | (InclProc | ExclProc), [e1; e2] ->
	  if not (is_var e1) then
	    sem_error "the argument of $ must be a variable"
		[fStr p.b_name] e1.e_loc;
	  voidtype

      | Assert, _ ->
	  if List.length args < 1 || List.length args > 2 then
	    sem_error "ASSERT expects 1 or 2 arguments" [] e.e_loc
          else begin
            let e1 = List.nth args 0 in
  	    check_assign e1 boolean env "as argument of ASSERT" [];
	    if List.length args = 2 then begin
	      let e2 = List.nth args 1 in
	      check_assign e2 inttype env "as argument of ASSERT" []
	    end
          end;
	  voidtype

      | AdrFun, [e1] ->
	  let _ = check_expr e1 env in
	  if not (is_var e1) then
	    sem_error "the argument of SYSTEM.ADR must be a variable"
	      [] e1.e_loc;
	  inttype

      | ValFun, [e1; e2] ->
	  (* Don't edit the expression, since this can result in constants
	     that are labelled with the wrong type. *)
	  check_qual e1 env;
	  let t2 = check_expr e2 env in
	  begin match e1.e_guts with
	      Name x ->
		begin try
		  let d = lookup_typename x env in 
                  let t1 = d.d_type in
                  if not (scalar t1) then
                    sem_error
                      "scalar type expected in SYSTEM.VAL" [] e1.e_loc;
                  if not (scalar t2) then
                    sem_error
                      "scalar value expected in SYSTEM.VAL" [] e2.e_loc;
                  let s1 = max t1.t_rep.m_size param_rep.m_size in
                  let s2 = max t2.t_rep.m_size param_rep.m_size in
                  if s1 != s2 then
                    sem_error 
                      "argument size must match result type in SYSTEM.VAL" 
                      [] e2.e_loc;
                  t1
		with Not_found -> errtype
		end
	    | _ ->
		sem_error 
		  "the first argument of SYSTEM.VAL must be a type name"
		  [] e1.e_loc;
		errtype
	  end

      | BitFun, _ -> 
	  boolean

      | (GetProc|PutProc), [e1; e2] ->
	  let t1 = check_expr e1 env in
	  let t2 = check_expr e2 env in
	  if not (same_types t1 inttype) then
	    sem_error "the first argument of SYSTEM.$ must have type INTEGER"
	      [fStr p.b_name] e1.e_loc;
	  if not (scalar t2) then
	    sem_error "the second argument of SYSTEM.$ must be a scalar"
	      [fStr p.b_name] e2.e_loc;
	  begin match p.b_id with
	      GetProc ->
		if not (is_var e2) then
		  sem_error 
		    "the second argument of SYSTEM.GET must be a variable"
		    [] e2.e_loc
            | PutProc ->
		if same_types t2 numtype then
		  convert e2 inttype
            | _ -> failwith "get/put"
	  end;
	  voidtype

      | _, _ ->
	  failwith (sprintf "no typing rule for $ with $ args" 
	    [fStr p.b_name; fNum (List.length args)])
  end

and check_assign e lt env cxt args =
  check_assign1 e lt env true cxt args 
  
and check_assign1 e lt env glob cxt args =
  let rt = check_expr e env in
  if not !Config.ob07flag
      && numeric lt && numeric rt && kind_of lt >= kind_of rt then
    coerce e lt
  else if !Config.ob07flag
      && (integral lt && integral rt || floating lt && floating rt) then
    coerce e lt
  else if is_string lt && is_char_const e && bound lt >= 2 then
    promote_char e
  else if is_address lt && is_niltype rt then
    e.e_type <- lt
  else if is_proc lt then begin
    proc_value e glob; 
    if not (proc_match lt e.e_type) then
      type_mismatch cxt args lt e
  end
  else if not (subtype rt lt 
      || is_string lt && is_string_const e && bound lt >= bound rt
      || same_types lt ptrtype && is_address rt
      || !Config.ob07flag && is_array lt && is_flex rt
            && same_types (base_type lt) (base_type rt)) then
    type_mismatch cxt args lt e

and proc_value e glob =
  match e.e_guts with
      Name x ->
        if not (undefined x) then begin
          let d = get_def x in
          match d.d_kind with
              ProcDef ->
                if d.d_level > 0 then begin
                  if glob then
                    sem_error ("local procedure '$' may not be used"
                        ^ " as a procedure value") [fId x.x_name] x.x_loc
                  else if not !Config.extensions then
                    sem_extend ("local procedure '$' may not be used"
                        ^ " as an argument") [fId x.x_name] x.x_loc
                end
            | PrimDef ->
                sem_error ("built-in procedure '$' may not be used"
                  ^ " as a procedure value") [fId x.x_name] x.x_loc;
                e.e_type <- errtype
            | _ -> ()
          end
      | _ -> ()
            
(* check_const -- check a constant expression, returning type and value *)
and check_const e env cxt =
  let t = check_expr e env in
  match e.e_guts with
      Const (v, _) -> (t, v)
    | _ -> 
	sem_error "$ must be a constant" [fStr cxt] e.e_loc;
	(t, IntVal (integer 0))

(* check_tconst -- check for a constant of specified type *)
and check_tconst e t env cxt =
  let (t1, v) = check_const e env cxt in
  if same_types t1 t then
    v
  else if coerceable t1 t then begin
    coerce e t;
    const_value e
  end
  else begin
    sem_error "$ should have $" [fStr cxt; fOType t] e.e_loc;
    sem_type t1;
    IntVal (integer 0)
  end
