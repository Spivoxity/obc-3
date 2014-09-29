(*
 * check.ml
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

open Tree
open Symtab
open Dict
open Mach
open Expr
open Error
open Eval
open Print

(* Some global variables to save passing parameters everywhere *)
let level = ref 0		(* Proc nesting level *)
let return_type = ref voidtype	(* Return type of current proc *)
let allocate = ref (function d -> failwith "allocate function")
				(* Storage allocator *)
let proc_locals = ref []	(* Locals of current proc *)
let loop_level = ref 0		(* Nesting level of loop stmts *)
let is_module = ref false	(* Whether checking module body *)

let agenda = ref ([] : (def * typexpr) list)
				(* List of type decls awaiting checking *)

(* add_def -- add definition to environment, give error if already declared *)
let add_def env d =
  try define env d with 
    Exit -> 
      let d0 = lookup env d.d_tag in
      if not (is_errtype d0.d_type) then
	sem_error "'$' has already been declared" [fId d.d_tag] d.d_loc

let make_def x k t doc =
  { d_tag = x.x_name; d_module = !current; d_export = x.x_export; d_kind = k; 
    d_used = (x.x_export <> Private); d_loc = x.x_loc; 
    d_line = Error.line_num x.x_loc;
    d_type = t; d_lab = nosym; d_level = !level; d_offset = 0; 
    d_param = 0; d_comment = doc; d_env = empty_env; d_map = [] }

let typedef () =
  { d_tag = anon; d_module = !current; d_export = Private; d_kind = TypeDef;
    d_used = true; d_loc = no_loc; d_line = 0; d_type = errtype; 
    d_lab = nosym; d_level = 0; d_offset = 0; d_param = 0; d_comment = None;
    d_env = empty_env; d_map = [] }

(* get_space -- allocate local space *)
let get_space size t =
  align t.t_rep.m_align size;
  size := !size + t.t_rep.m_size;
  - !size

(* upward_alloc -- allocate objects upward in memory *)
let upward_alloc size d =
  align d.d_type.t_rep.m_align size;
  let addr = !size in
  size := !size + d.d_type.t_rep.m_size;
  d.d_offset <- addr

(* downward_alloc -- allocate objects downward in memory *)
let downward_alloc size d =
  d.d_offset <- get_space size d.d_type

(* param_size -- compute space occupied by formal *)
let param_size k t =
  let s = param_rep.m_size in
  if is_proc t then
    match k with
	(ParamDef | CParamDef) -> 2 * s
      | VParamDef -> s
      | _ -> failwith "param_size"
  else if scalar t then
    match k with 
	(ParamDef | CParamDef) ->  max s t.t_rep.m_size 
      | VParamDef -> s
      | _ -> failwith "param_size"
  else if is_array t then
    (flexity t + 1) * s
  else if is_record t then
    match k with
	(ParamDef | CParamDef) -> s 
      | VParamDef -> 2 * s
      | _ -> failwith "param_size"
  else
    failwith "param_size"

(* param_alloc -- allocate space for a parameter *)
let param_alloc head psize d = 
  let addr = head + !psize in
  psize := !psize + param_size d.d_kind d.d_type;
  d.d_offset <- addr

(* param_copy -- allocate space to copy aggregate value params *)
let param_copy fsize d =
  if d.d_kind = ParamDef && not (scalar d.d_type) 
      && not (is_flex d.d_type) then begin
    d.d_param <- d.d_offset;
    d.d_offset <- get_space fsize d.d_type
  end

(* global_alloc -- allocate space for global variable *)
let global_alloc fsize d = 
  d.d_lab <- sprintf "$.$" [fId !current; fId d.d_tag];
  d.d_map <- d.d_type.t_map

let is_typename tx =
  match tx.tx_guts with TypeName _ -> true | _ -> false

let check_target t loc = 
  match t.t_guts with
      ArrayType _ -> 
	if t.t_map <> [] then make_desc t
    | RecordType _ | FlexType _ -> ()
    | _ -> 
	if not (is_errtype t) then begin
	  sem_error 
	    "the base type of a pointer must be an array or record" 
	    [] loc;
	  sem_type t
	end

let find_receiver rcvr =
  let rt = rcvr.d_type in
  match rcvr.d_kind with
      (ParamDef | CParamDef) ->
	if is_pointer rt && is_record (base_type rt) then
	  base_type rt
	else begin
	  if not (is_errtype rt 
		  || is_pointer rt 
		      && is_errtype (base_type rt)) then begin
	    sem_error "a value receiver must be a record pointer" 
	      [] rcvr.d_loc;
	    sem_type rt
	  end;
	  raise Not_found
        end
    | VParamDef ->
	if is_record rt then
	  rt
        else begin
	  if not (is_errtype rt) then begin
	    sem_error "a VAR receiver must have a record type" [] rcvr.d_loc;
	    sem_type rt
	  end;
	  raise Not_found
	end
    | _ ->
	failwith "find_receiver"

let add_method kind binder d =
  let bindrec = get_record binder in
  if kind = AbsMeth && not bindrec.r_abstract then
    sem_error "only abstract records can have abstract methods" [] d.d_loc;
  begin try 
    let prev = find_field binder d.d_tag in
    match prev.d_kind with
	FieldDef -> 
	  sem_error "method '$' has the same name as a field" 
	    [fId d.d_tag] d.d_loc
      | ProcDef ->
	  sem_error "method '$' has already been defined" 
	    [fId d.d_tag] d.d_loc
      | _ -> failwith "prev method"
  with Not_found -> () 
  end;
  bindrec.r_methods <- bindrec.r_methods @ [d]


(* Method inheritance *)

(* Should check that fields do not hide methods of the parent *)

let methods t = 
  if is_record t then let r = get_record t in r.r_methods 
  else []

let make_method table d =
  while Growvect.size table <= d.d_offset do
    Growvect.append table dummy_def
  done;
  Growvect.set table d.d_offset d

let fMeth md =
  let p = get_proc md.d_type in
  let fp0 = List.hd p.p_fparams in
  let t = base_type fp0.d_type in
  fMeta "$.$" [fQual (t.t_module, t.t_name); fId md.d_tag]

let check_override md0 md =
  let clash attr =
    sem_error "method '$' overrides '$' with $"
      [fMeth md; fMeth md0; fStr attr] md.d_loc in
  match (md0.d_type.t_guts, md.d_type.t_guts) with
      (ProcType p0, ProcType p) ->
	let rcvr0 = List.hd p0.p_fparams and rcvr = List.hd p.p_fparams in
	if rcvr0.d_kind <> rcvr.d_kind then
	  clash "different receiver mode"
	else if not (match_args (List.tl p0.p_fparams) 
					(List.tl p.p_fparams)) then
	  clash "incompatible parameters"
	else if not (same_types p0.p_result p.p_result) then
	  clash "different result type"
	else if md0.d_export > md.d_export then
	  clash "more restrictive export status"
    | _ -> failwith "check_override"

let check_method ms0 table vsize md =
  begin try
    let md0 = find_def ms0 md.d_tag in
    check_override md0 md;
    md.d_offset <- md0.d_offset;
  with Not_found ->
    md.d_offset <- !vsize;
    incr vsize
  end;
  make_method table md

let check_methods t =
  if is_record t then begin
    let r = get_record t in
    if !Config.debug > 0 then 
      printf "! $ defines methods $\n" 
	[fId t.t_name; fList(fMeth) r.r_methods];
    let ms0 = methods r.r_parent in
    let vsize = ref (List.length ms0) in
    let table = Growvect.create 10 in
    List.iter (make_method table) ms0;
    List.iter (check_method ms0 table vsize) r.r_methods;
    r.r_methods <- Growvect.to_list table;
    if !Config.debug > 0 then 
      printf "! $ now has methods $\n" 
	[fId t.t_name; fList(fMeth) r.r_methods]
  end

(* check_return -- check that procedure contains a RETURN statement *)
let rec check_return s = 
  match s.s_guts with
      Return e -> true
    | IfStmt (arms, elsept) ->
	List.exists (fun (_, s) -> check_return s) arms || check_return elsept
    | CaseStmt (sw, arms, default) ->
	let check2 (labs, body) = check_return body in
	List.exists check2 arms 
	  || (match default with Some s1 -> check_return s1 | None -> false)
    | WhileStmt arms -> 
	List.exists (fun (_, s) -> check_return s) arms
    | RepeatStmt (body, test) -> check_return body
    | LoopStmt body -> check_return body
    | ForStmt (v, lo, hi, step, body, _) ->
	check_return body
    | WithStmt (arms, default) ->
	List.exists (fun (_, _, body) -> check_return body) arms 
	  || (match default with Some s1 -> check_return s1 | None -> false)
    | LocalStmt (decls, body) -> check_return body
    | Seq ss -> List.exists check_return ss
    | _ -> false

let check_used ds = 
  let check d = 
    if not d.d_used then
      match d.d_kind with
	  (* ParamDef | VParamDef | CParamDef ->
	    sem_warn "parameter $ is not used" [fId d.d_tag] d.d_loc *)
	| VarDef | ProcDef ->
	    let msg = 
	      if d.d_level = 0 then
	        "$ is declared but not used or exported"
	      else
	        "$ is declared but not used" in
	    sem_warn msg [fId d.d_tag] d.d_loc
	| ModDef _ ->
	    sem_warn "$ is imported but never used" [fId d.d_tag] d.d_loc
	| _ -> () in
  List.iter check ds

(* bad_paramtype -- test if a type expression is a sensible formal type *)
let rec bad_paramtype t =
  if t.t_name <> anon then
    false
  else begin
    match t.t_guts with
        PointerType d -> bad_paramtype (d.d_type)
      | ArrayType (upb, t1) -> 
	  (* We allow ARRAY n OF CHAR because it matches a string constant *)
	  not (same_types t1 character)
      | FlexType t1 -> bad_paramtype t1
      | RecordType _ -> true
      | ProcType _ -> false
      | _ -> false
  end

(* check_caselab -- check case label *)
let check_caselab env t lab =
  let check_lab e = check_tconst env t "this case label" e in 
  match lab with
      Single e -> 
	let v = check_lab e in
	(int_value v, int_value v, e.e_loc)
    | Range (e1, e2) ->
	let v1 = check_lab e1 and v2 = check_lab e2 in
	let loc = join_locs e1.e_loc e2.e_loc in
	if v1 > v2 then	
	  sem_error "this case label specifies an empty range" [] loc;
	(int_value v1, int_value v2, loc)

(* check_dupcases -- check for duplicate case labels *)
let check_dupcases vs t =
  let rec chk = 
    function
	[] | [_] -> ()
      | (x1, y1, loc1) :: ((x2, y2, loc2) :: ys as rest) -> 
	  if y1 >= x2 then
	    sem_error "this is a duplicate case label for value $" 
	      [if same_types t character then 
		  fMeta "'$'" [fChr (char_of_integer x2)]
		else fInteger x2] loc2;
	  chk rest in

  let ord (x1, y1, loc1) (x2, y2, loc2) = 
    if x1 <> x2 then compare x1 x2 else compare loc1 loc2 in
  chk (List.sort ord vs)

let check_forvar env e =
  let t = match e.e_guts with
      Name x ->
	begin try
	  let d = lookup_def env x in
	  begin match d.d_kind with
	      (VarDef | ParamDef | CParamDef | VParamDef) -> d.d_type
	    | _ ->
	      sem_error "the name after FOR must be a variable"
		[] e.e_loc;
	      errtype
	  end
	with Not_found -> errtype
	end
    | _ ->
	sem_error "the expression after FOR must be a simple variable"
	  [] e.e_loc;
	errtype in
  e.e_type <- t; t

let receiver_loc (Heading (ds, _)) =
  match List.hd ds with
      VarDecl (_, _, tx, _) -> tx.tx_loc
    | _ -> failwith "receiver_loc"

let rec check_stmt env s =
  match s.s_guts with
      Assign (lhs, rhs) ->
        let lt = check_desig env lhs in
	if not (is_var lhs) then
	  sem_error "the LHS of an assignment must be a variable" [] lhs.e_loc;
	check_assign "on the RHS of this assignment" [] env lt rhs rhs.e_loc
    | SimAssign pairs ->
	if not !Config.extensions then
	  sem_extend "simultaneous assignment is not allowed" [] s.s_loc;
	List.iter (fun (e1, e2) ->
	    let lt = check_desig env e1 in
	    if not (is_var e1) then
	      sem_error "the LHS of an assignment must be a variable" 
		[] e1.e_loc;
	    if not (scalar lt) then begin
	      sem_error 
		"simultaneous assignment is implemented only for scalar types"
		[] e1.e_loc;
	      sem_type lt
	    end;
	    check_assign "on the RHS of this assignment" [] env lt e2 e2.e_loc)
	  pairs
    | ProcCall e ->
	begin match e.e_guts with
	    FuncCall (p, args) ->
	      let t = check_call env p args e false in
	      e.e_type <- t;
	      if not (same_types t voidtype) && not !Config.extensions then
		sem_extend ("a call that returns a result "
		  ^ "cannot be used as a statement") [] s.s_loc
	  | _ -> failwith "proc call"
	end

    | Return res ->
	if !is_module then
	  sem_error "a RETURN statement is not allowed in a module body" 
	    [] s.s_loc
	else begin match res with
	    Some e ->
	      if same_types !return_type voidtype 
	          && not (is_errtype !return_type) then
	        sem_error "this RETURN statement should not specify a result" 
		  [] s.s_loc
	      else
		check_assign "in this RETURN statement" []
		  env !return_type e e.e_loc
          | None ->
	      if not (same_types !return_type voidtype) then
	        sem_error "this RETURN statement should specify a result" 
		  [] s.s_loc
	end

    | IfStmt (arms, elsept) ->
	List.iter (fun (cond, thenpt) ->
	    let ct = check_expr env cond in
	    if not (same_types ct boolean) then begin
	      sem_error "the test in an IF statement must have type BOOLEAN" 
		[] cond.e_loc; 
	      sem_type ct
	    end;   
	    check_stmt env thenpt)
	  arms;
	check_stmt env elsept;

    | CaseStmt (switch, arms, default) ->
	let st = check_expr env switch in
	if not (is_discrete st) then begin
	  let reqd = if !Config.extensions then "a discrete type" 
	    else "an integral or character type" in
	  sem_error "the expression after CASE must have $"
	      [fStr reqd] switch.e_loc;
	  sem_type st
	end
	else begin
	  if not (integral st || same_types st character) 
	      && not !Config.extensions then begin
	    sem_extend "CASE expects an integer or character expression" 
	      [] switch.e_loc;
	    sem_type st
	  end;
	  if same_types st longint then
	    sem_error "sorry, CASE for type LONGINT is not implemented" 
	      [] switch.e_loc;

	  let nerrs = !Error.err_count in
	  let check_labs (labs, body) = 
	    List.map (check_caselab env st) labs in
	  let vs = List.concat (List.map check_labs arms) in
	  (* Check for duplicate only if there were no errors while
	      checking the labels *)
	  if !Error.err_count = nerrs then check_dupcases vs st
	end;
	List.iter (function (labs, body) -> check_stmt env body) arms;
	check_else env default

    | WhileStmt arms ->
	List.iter (fun (cond, body) ->
	    let ct = check_expr env cond in
	    if not (same_types ct boolean) then begin
	      sem_error "the test in a WHILE statement must have type BOOLEAN" 
		[] cond.e_loc;
	      sem_type ct
	    end;
	    check_stmt env body)
	  arms

    | RepeatStmt (body, cond) ->
	check_stmt env body;
	let ct = check_expr env cond in
	if not (same_types ct boolean) then begin
	  sem_error "the test after UNTIL must have type BOOLEAN"
	    [] cond.e_loc;
	  sem_type ct
	end

    | LoopStmt body ->
	incr loop_level;
	check_stmt env body;
	decr loop_level
    | ExitStmt ->
	if !loop_level = 0 then
	  sem_error "this EXIT statement is not inside a LOOP statement" 
	    [] s.s_loc
    | ForStmt (var, lo, hi, step, body, tmp) ->
 	let vt = check_forvar env var in
	if not (is_discrete vt) then begin
	  sem_error "the variable after FOR must have a discrete type" 
	    [] var.e_loc;
	  sem_type vt
	end else begin
	  if not (integral vt) && not !Config.extensions then begin
	    sem_extend "the variable after FOR must have an integral types"
	      [] var.e_loc;
	    sem_type vt
	  end;

	  check_assign "as a starting value" [] env vt lo lo.e_loc;
	  check_assign "as an ending value" [] env vt hi hi.e_loc;
	  let (bt, _) = check_const env "a step value" step in
	  if not (integral bt) then begin
	    sem_error "the step value must be an integer" [] step.e_loc;
	    sem_type bt
	  end
	end;
	begin
	  match hi.e_guts with
	      Const _ -> ()
	    | _ -> 
		let x = makeDefId (anon, Private, no_loc) in
		let d = make_def x VarDef vt None in 
		!allocate d; tmp := d
	end;
 	check_stmt env body
    | WithStmt (branches, else_part) ->
	let check_branch (e1, tn, body) =
	  try
	    let dx = lookup_def env (get_name e1) in
	    let dt = lookup_typename env tn in
	    e1.e_type <- dx.d_type;
	    check_typetest e1 dt.d_type e1.e_loc;
	    let dx' = { dx with d_loc = e1.e_loc; d_type = dt.d_type } in
	    let env' = new_block env in
	    add_def env' dx';
	    check_stmt env' body
	  with Not_found -> () in
	List.iter check_branch branches;
	check_else env else_part
    | LocalStmt (decls, body) ->
	if not !Config.extensions then
	  sem_extend "declarations cannot be nested inside blocks" [] s.s_loc;
	let env' = new_block env in
	check_decls false !allocate env' decls;
	List.iter (check_body env') decls;
	check_stmt env' body;
	let locals = top_block env' in
	check_used locals;
	proc_locals := locals @ !proc_locals
    | Seq ss -> List.iter (check_stmt env) ss
    | Skip -> ()
    | ErrStmt ->
	failwith "check ErrStmt"

and check_else env =
  function
      Some ss -> check_stmt env ss
    | None -> ()

(* check_typexpr -- check a type expression, returning the otype *)
and check_typexpr lzy env name tx = 
  match tx.tx_guts with
      TypeName x ->
	begin try
	  let d = lookup_typename env x in d.d_type
	with 
	  Not_found -> errtype
	end
    | _ ->
	let t = check_typecons lzy env name tx in
	t.t_name <- name; 
	if is_record t then make_desc t;
	t
	
and check_typecons lzy env name tx =
  match tx.tx_guts with
      Enum xs ->
	if not !Config.extensions then
	  sem_extend "enumerated types are not allowed" [] tx.tx_loc;
	let t = new_type !level (EnumType (List.length xs), int_rep, []) in
	let j = ref 0 in
	let dcl x = add_def env 
	      (make_def x (ConstDef (IntVal (integer !j))) t None);
	  incr j in
	List.iter dcl xs;
	t
    | Pointer tx1 ->
	let d = typedef () in
	if lzy && (is_typename tx1 || !Config.extensions) then
	  agenda := (d, tx1) :: !agenda
	else begin
	  let t = check_typexpr lzy env anon tx1 in
	  check_target t tx1.tx_loc;
	  d.d_type <- t
	end;
	new_type !level (PointerType d, addr_rep, [GC_Offset 0])
    | Array (upb, tx1) ->
	let v1 = check_tconst env inttype "an array bound" upb
 	and t2 = check_typexpr lzy env anon tx1 in
 	if is_flex t2 then
 	  sem_error "arrays cannot have open arrays as elements" [] tx1.tx_loc;
 	if is_abstract t2 then
 	  sem_error "arrays cannot have abstract record types as elements"
 	    [] tx1.tx_loc;
	if int_value v1 < integer 0 then
	  sem_error "upper bound of array must be >= 0" [] upb.e_loc;
	new_type !level (row (int_of_integer (int_value v1)) t2)
    | Flex tx1 ->
	let t1 = check_typexpr lzy env anon tx1 in
 	if is_abstract t1 then
 	  sem_error "open arrays cannot have abstract record types as elements"
 	    [] tx1.tx_loc;
	new_type !level (flex t1)
    | Record (abs, parent, fields) ->
	let offset = ref 0 in
	let (pt, fields0) = 
	  match parent with 
	      Some x -> 
		let t1 = check_typexpr false env anon x in
		if is_record t1 then begin
		  let r = get_record t1 in
		  offset := t1.t_rep.m_size; (t1, r.r_fields)
		end
		else begin
		  if not (is_errtype t1) then begin
		    sem_error "a parent must be a record type" [] x.tx_loc;
		    sem_type t1
		  end;
		  (voidtype, [])
		end
	    | None -> (voidtype, []) in
	let env' = add_block env fields0 in
	check_decls lzy (upward_alloc offset) env' fields;
	align max_align offset;
	new_type !level (record abs pt !offset (top_block env'))
    | Proc heading ->
	let d = 
	  check_heading Procedure (makeDefId (anon, Private, no_loc)) 
	    heading None env (ref 0) in
	d.d_type
    | _ ->
	failwith "check_typecons"

(* check_decl -- check a declaration and add it to the environment *)
and check_decl lzy alloc env = 
  function
      ConstDecl (x, e, doc) ->
	let t = check_expr env e in
	let d =
	  match e.e_guts with
	      Const (v, _) -> make_def x (ConstDef v) t doc
	    | String (str, n) -> 
		let d = make_def x StringDef t doc in
		d.d_lab <- str; d
	    | _ -> 
		sem_error "a CONST declaration must contain a constant" 
		  [] e.e_loc;
		make_def x (ConstDef (IntVal (integer 0))) errtype doc in
	add_def env d
    | VarDecl (kind, xs, tx, doc) ->
        let t = check_typexpr lzy env anon tx in
	begin match kind with
	    ParamDef | CParamDef | VParamDef ->
	      if bad_paramtype t then
		sem_warn ("you should name this parameter type,"
		  ^ " or else no actual parameter will match") [] tx.tx_loc;
	  | _ ->
	      if is_flex t then begin
		sem_error "an open array type is not allowed here" 
		  [] tx.tx_loc;
		sem_type t
	      end
	end;
        let def x = 
	  begin match kind with
	      (ParamDef | CParamDef | VParamDef) ->
		if x.x_export <> Private then
		  sem_error "cannot export a parameter" [] x.x_loc
	    | _ -> ()
	  end;
	  let d = make_def x kind t doc in alloc d; add_def env d in
	List.iter def xs
    | TypeDecl decls ->
	List.iter (fun (x, te, doc) ->
	  let t = check_typexpr true env x.x_name te in
	  let d = make_def x TypeDef t doc in
	  x.x_def <- d; add_def env d) decls;
	force_agenda env
    | ProcDecl (kind, x, heading, body, doc) ->
	let fsize = 
	  match body with Block (_, _, fs) -> fs | NoBlock -> ref 0 in
	check_proc env kind x heading fsize doc
    | PrimDecl (x, heading, name, doc) ->
	if !level > 0 then
	  sem_error "primitives must be declared at the outermost level"
	    [] x.x_loc;
	check_proc env Procedure x heading (ref 0) doc;
	make_prim name
    | ForwardDecl (kind, x, heading, doc) -> ()
    | DummyDecl -> ()

and check_decls lzy alloc env ds = 
  List.iter (check_decl lzy alloc env) ds

and force_agenda env =
  List.iter (fun (d, tx) ->
    let t = check_typexpr false env anon tx in
    check_target t tx.tx_loc;
    d.d_type <- t) !agenda;
  agenda := []

and check_heading kind x (Heading (fparams, result)) doc env fsize =
  incr level;
  let psize = ref 0 in
  let alloc = param_alloc frame_head psize in
  let env' = new_block env in
  check_decls false alloc env' fparams;
  decr level;
  let loc = ref no_loc in
  let rt = 
    (match result with
	Some t -> 
	  begin try
	    loc := t.x_loc; 
	    let d = lookup_typename env t in
	    d.d_type
          with Not_found -> errtype
	  end
      | None -> voidtype) in
  if not (scalar rt) then begin
    sem_error "a procedure may not return an array or record type" [] !loc;
    sem_type rt;
  end;
  let p = { p_kind = kind; p_fparams = top_block env'; 
	    p_result = rt; p_pcount = !psize / param_rep.m_size } in
  let t = new_type !level (ProcType p, addr_rep, []) in
  let d = make_def x ProcDef t doc in
  d.d_env <- env'; d

(* check_proc -- check a procedure declaration *)
and check_proc env kind x heading fsize doc =
  let d = check_heading kind x heading doc env fsize in
  match kind with
      Procedure ->
	d.d_lab <- proc_name !current !level x.x_name;
	x.x_def <- d; add_def env d
    | Method | AbsMeth ->
	if !level > 0 then
	  sem_error "methods may only be declared at the outermost level" 
	      [] x.x_loc
	else begin
	  let p = get_proc d.d_type in
	  let rcvr = List.hd p.p_fparams in
	  try 
	    let binder = find_receiver rcvr in
 	    if binder.t_module <> !current then
 	      sem_error "$ does not belong to this module" 
 		[fOType binder] (receiver_loc heading);
	    d.d_lab <- sprintf "$.$" [fSym binder.t_desc; fId x.x_name];
	    x.x_def <- d; add_method kind binder d
	  with Not_found -> ()
	end
    | _ -> failwith "check_proc"

(* check_body -- check body of a procedure declaration *)
and check_body env =
  function
      ProcDecl (_, x, _, Block (locals, body, fsize), _) ->
	if not (undefined x) then begin
	  let d = get_def x in
	  let p = get_proc d.d_type in
	  let env' = d.d_env in
	  proc_locals := [];
	  let cxt = !err_context in
	  incr level; err_context := env';

	  let errs0 = !Error.err_count in
	  fsize := if !level > 1 then word_size else 0;
	  check_decls false (downward_alloc fsize) env' locals;
	  List.iter (check_body env') locals;
	  return_type := p.p_result;
	  allocate := downward_alloc fsize;
	  check_stmt env' body;
	  check_used (top_block env');
	  if !Error.err_count = errs0 then
	    (* Don't check for uninitialised variables if there were errors *)
	    Inicheck.check_init !level body;
	  if not (same_types p.p_result voidtype) 
	      && not (check_return body) then
	    sem_warn "this function contains no RETURN statement" 
	      [] x.x_loc;

	  decr level; err_context := cxt;
	  List.iter (param_copy fsize) p.p_fparams;
	  align max_align fsize;
	  let locdefs = !proc_locals @ top_block env' in
	  d.d_map <- local_map locdefs
	end

    | PrimDecl (x, _, _, _) ->
	if not (undefined x) then begin
	  let d = get_def x in
	  let p = get_proc d.d_type in
	  d.d_map <- local_map p.p_fparams
	end
    | _ -> ()

let check_import env (int, ext, stamp) =
  if ext = !current then
    sem_error "a module must not import itself (boy, are you inept!)" 
      [] int.x_loc
  else begin
    try
      let (env', st, doc) = 
	if ext = intern (if !Config.lcflag then "system" else "SYSTEM") then 
	  (sysenv (), 0, None)
	else 
	  Symfile.import
	    (Util.search_path (extern ext ^ ".k") !Config.libpath) in
      stamp := st;
      add_def env (make_def int (ModDef (ext, env')) voidtype None)
    with Not_found ->
      sem_error "the interface file for '$' cannot be found" 
	[fId ext] int.x_loc;
  end

let annotate (Module (m, imports, body, glodefs, doc)) =
  level := 0;
  let glo_env = new_block (init_env ()) in
  err_context := glo_env;
  List.iter (check_import glo_env) imports;
  if !Error.err_count = 0 then begin	
    (* No point going on if imports failed *)
    match body with
	Block (globals, body, fsize) ->
	  check_decls false (global_alloc fsize) glo_env globals;
	  List.iter check_methods (desc_table ());
	  is_module := false;
	  List.iter (check_body glo_env) globals;
	  return_type := voidtype;
	  proc_locals := [];
	  allocate := downward_alloc fsize;
	  is_module := true;
	  level := 1;
	  check_stmt glo_env body;
	  level := 0;
	  align max_align fsize;

	  let globals = top_block glo_env in
	  check_used globals;
	  glodefs := globals;

	  (* Hackily set the definition of the module to be the procedure
	     for the module body *)
	  let d = make_def m ProcDef bodytype doc in
	  d.d_lab <- sprintf "$.%main" [fId !current];
	  d.d_map <- local_map !proc_locals;
	  m.x_def <- d;

      | NoBlock -> failwith "annotate"
  end

