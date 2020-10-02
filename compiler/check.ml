(*
 * check.ml
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
open Symfile
open Dict
open Gcmap
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
let loop_level = ref 0		(* Nesting level of loop stmts *)
let is_module = ref false	(* Whether checking module body *)

let agenda = ref ([] : (def * typexpr) list)
				(* List of type decls awaiting checking *)

(* add_def -- add definition to environment, give error if already declared *)
let add_def d env =
  try define env d with 
    Exit -> 
      let d0 = lookup d.d_tag env in
      if not (is_errtype d0.d_type) then
	sem_error "'$' has already been declared" [fId d.d_tag] d.d_loc

let make_def x k t doc =
  (* Variables are exported read-only in Oberon-07 *)
  let exp = if !Config.ob07flag && k = VarDef
		&& x.x_export = Visible then ReadOnly else x.x_export in
  { d_tag = x.x_name; d_module = !current; d_export = exp; d_kind = k; 
    d_used = (x.x_export <> Private); d_loc = x.x_loc; 
    d_line = Error.line_num x.x_loc;
    d_type = t; d_lab = nosym; d_level = !level; d_offset = 0; 
    d_param = 0; d_comment = doc; d_env = empty_env; d_map = null_map }

let typedef () =
  { d_tag = anon; d_module = !current; d_export = Private; d_kind = TypeDef;
    d_used = true; d_loc = no_loc; d_line = 0; d_type = errtype; 
    d_lab = nosym; d_level = 0; d_offset = 0; d_param = 0; d_comment = None;
    d_env = empty_env; d_map = null_map }

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
  match (k, t.t_guts) with
      (* Arrays are address, plus n flex bounds *)
      (_, ArrayType _) -> s
    | (_, FlexType _) -> (flexity t + 1) * s

      (* Records are address, plus desciptor if a VAR param *)
    | ((ParamDef|CParamDef), RecordType _) -> s
    | (VParamDef, RecordType _) -> 2*s

      (* Procedure value params are code + static link *)
    | ((ParamDef|CParamDef), ProcType _) -> 2*s

      (* Scalar value params may take several words *)
    | ((ParamDef|CParamDef), _) when scalar t -> max s t.t_rep.m_size

      (* Scalar VAR params (including procedure variables) are an address *)
    | (VParamDef, _) when scalar t -> s

    | (_, _) -> failwith "param_size"

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

let not_07 feature loc = 
  if !Config.ob07flag && not !Config.extensions then 
    sem_extend "$ is not allowed in Oberon-07" [fStr feature] loc

let check_target t loc = 
  match t.t_guts with
      ArrayType _ -> 
        not_07 "pointer to array" loc;
        if t.t_map <> null_map then make_desc t
    | FlexType _ -> 
        not_07 "pointer to open array" loc
    | RecordType _ -> ()
    | _ -> 
	if not (is_errtype t) then begin
	  sem_error 
            "the base type of a pointer must be $"
            [fStr (if !Config.ob07flag then "a record"
                else "an array or record")] loc;
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
    let prev = find_field d.d_tag binder in
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
    Growvect.append table None
  done;
  Growvect.set table d.d_offset (Some d)

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

let check_method md ms0 table vsize =
  begin try
    let md0 = find_def md.d_tag ms0 in
    check_override md0 md;
    md.d_offset <- md0.d_offset;
  with Not_found ->
    md.d_offset <- !vsize;
    incr vsize
  end;
  make_method table md

let check_concrete d t loc =
  let p = get_proc d.d_type in
  if p.p_kind = AbsMeth then begin
    (* Check that p is not an abstract method erroneously added to a
       non-abstract record type. *)
    let fp = List.hd p.p_fparams in
    if not (same_types fp.d_type t) then
      sem_error
        "$ should implement abstract method '$' or be declared abstract itself"
        [fOType t; fId d.d_tag] loc
  end

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
    List.iter (fun md -> check_method md ms0 table vsize) r.r_methods;
    let h = function Some d -> d | None -> failwith "check_methods" in
    r.r_methods <- List.map h (Growvect.to_list table);
    if not r.r_abstract then
      List.iter (fun d -> check_concrete d t r.r_loc) r.r_methods;
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
	List.exists (fun (_, body) -> check_return body) arms 
	  || (match default with Some s1 -> check_return s1 | None -> false)
    | TypeCase (sw, arms, default) ->
	List.exists (fun (_, body) -> check_return body) arms 
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
  if !Error.err_count = 0 then List.iter check ds

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
let check_caselab lab t env =
  let check_lab e = check_tconst e t env "a case label" in 
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

let check_forvar e env =
  let t = match e.e_guts with
      Name x ->
	begin try
	  let d = lookup_def x env in
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

let element_loc =
  function
      Single e -> e.e_loc
    | Range (e1, e2) -> Error.join_locs e1.e_loc e2.e_loc

let make_typecase (switch, arms, default) env =
  let fix (labs, body) =
    if List.length labs > 1 then
      sem_error "Only one case label allowed in type CASE statement" 
        [] (element_loc (List.nth labs 1));
    match List.hd labs with
        Single e ->
          begin
            check_qual e env;
            match e.e_guts with
                Name x -> (x, body)
              | _ -> 
                  sem_error "type name expected in type CASE statement"
                    [] e.e_loc;
                  raise Not_found
          end
      | Range (e1, e2) ->
          sem_error "range pattern not allowed in type CASE statement" 
            [] (Error.join_locs e1.e_loc e2.e_loc);
          raise Not_found in
  match switch.e_guts with
      Name x -> 
        TypeCase (switch, List.map fix arms, default)
    | _ ->
        sem_error "name expected in type CASE statement" [] switch.e_loc;
        raise Not_found

let rec check_stmt s env =
  match s.s_guts with
      Assign (lhs, rhs) ->
        let lt = check_desig lhs env in
	if not (is_var lhs) then
	  sem_error "the LHS of an assignment must be a variable" [] lhs.e_loc;
	check_assign rhs lt env "on the RHS of this assignment" []

    | SimAssign pairs ->
	if not !Config.extensions then
	  sem_extend "simultaneous assignment is not allowed" [] s.s_loc;
	List.iter (fun (e1, e2) ->
	    let lt = check_desig e1 env in
	    if not (is_var e1) then
	      sem_error "the LHS of an assignment must be a variable" 
		[] e1.e_loc;
	    if not (scalar lt) then begin
	      sem_error 
		"simultaneous assignment is implemented only for scalar types"
		[] e1.e_loc;
	      sem_type lt
	    end;
	    check_assign e2 lt env "on the RHS of this assignment" [])
	  pairs

    | ProcCall e ->
	begin match e.e_guts with
	    FuncCall (p, args) ->
	      let t = check_call p args e env false in
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
		check_assign e !return_type env
                  "in this RETURN statement" []
          | None ->
	      if not (same_types !return_type voidtype) then
	        sem_error "this RETURN statement should specify a result" 
		  [] s.s_loc
	end

    | IfStmt (arms, elsept) ->
	List.iter (fun (cond, thenpt) ->
	    let ct = check_expr cond env in
	    if not (same_types ct boolean) then begin
	      sem_error "the test in an IF statement must have type BOOLEAN" 
		[] cond.e_loc; 
	      sem_type ct
	    end;   
	    check_stmt thenpt env)
	  arms;
	check_stmt elsept env;

    | CaseStmt (switch, arms, default) ->
	let st = check_expr switch env in
        if is_record st || is_pointer st && is_record (base_type st) then begin
          try
            let tc = make_typecase (switch, arms, default) env in
            s.s_guts <- tc; check_typecase s env
          with Not_found -> ()
        end
        else begin
          if not (is_discrete st) then begin
            let reqd = if !Config.extensions then "a discrete type" 
              else "an integral or character type" in
            sem_error "the expression after CASE must have $"
                [fStr reqd] switch.e_loc;
            sem_type st
          end else begin
            if not (integral st || same_types st character) 
                && not !Config.extensions then begin
              sem_extend "CASE expects an integer or character expression" 
                [] switch.e_loc;
              sem_type st
            end;
            if same_types st longint then
              sem_error "sorry, CASE for type LONGINT is not implemented" 
                [] switch.e_loc;

            (* Replace numtype by integer *)
            let st' = if integral st then inttype else st in
            let nerrs = !Error.err_count in
            let check_labs (labs, body) = 
              List.map (fun lab -> check_caselab lab st' env) labs in
            let vs = List.concat (List.map check_labs arms) in
            (* Check for duplicate only if there were no errors while
                checking the labels *)
            if !Error.err_count = nerrs then check_dupcases vs st'
          end;
          List.iter (function (labs, body) -> check_stmt body env) arms;
          check_else env default
        end

    | WhileStmt arms ->
	List.iter (fun (cond, body) ->
	    let ct = check_expr cond env in
	    if not (same_types ct boolean) then begin
	      sem_error "the test in a WHILE statement must have type BOOLEAN" 
		[] cond.e_loc;
	      sem_type ct
	    end;
	    check_stmt body env)
	  arms

    | RepeatStmt (body, cond) ->
	check_stmt body env;
	let ct = check_expr cond env in
	if not (same_types ct boolean) then begin
	  sem_error "the test after UNTIL must have type BOOLEAN"
	    [] cond.e_loc;
	  sem_type ct
	end

    | LoopStmt body ->
	incr loop_level;
	check_stmt body env;
	decr loop_level

    | ExitStmt ->
	if !loop_level = 0 then
	  sem_error "this EXIT statement is not inside a LOOP statement" 
	    [] s.s_loc

    | ForStmt (var, lo, hi, step, body, tmp) ->
 	let vt = check_forvar var env in
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

	  check_assign lo vt env "as a starting value" [];
	  check_assign hi vt env "as an ending value" [];
	  let (bt, inc) = check_const step env "a step value" in
	  if not (integral bt) then begin
	    sem_error "the step value must be an integer" [] step.e_loc;
	    sem_type bt
	  end
          else if int_value inc = integer 0 then begin
            sem_error "the step value must be non-zero" [] step.e_loc
          end
	end;
	begin
	  match hi.e_guts with
	      Const _ -> ()
	    | _ -> 
		let x = makeDefId (anon, Private, no_loc) in
		let d = make_def x VarDef vt None in 
		!allocate d; tmp := Some d
	end;
 	check_stmt body env

    | WithStmt (branches, else_part) ->
	let check_branch (e1, tn, body) =
	  try
	    let dx = lookup_def (get_name e1) env in
	    let dt = lookup_typename tn env in
	    e1.e_type <- dx.d_type;
	    check_typetest e1 dt.d_type e1.e_loc;
	    let dx' = { dx with d_loc = e1.e_loc; d_type = dt.d_type } in
	    let env' = new_block env in
	    add_def dx' env';
	    check_stmt body env'
	  with Not_found -> () in
	List.iter check_branch branches;
	check_else env else_part

    | Seq ss -> List.iter (fun s -> check_stmt s env) ss

    | Skip -> ()

    | _ -> failwith "check"

and check_typecase s env =
  match s.s_guts with
      TypeCase (switch, arms, default) ->
        let st = switch.e_type in
        if not (dynamic switch) then begin
          sem_error "a record pointer or VAR parameter is needed here" 
            [] switch.e_loc;
          if not (is_record st) then sem_type st
        end
        else begin
          let dx = get_def (get_name switch) in
          let check_arm (tn, body) =
            try
              let dt = lookup_typename tn env in
              if not (subtype dt.d_type st) then begin
                sem_error "a subtype of $ is needed here" [fOType st] tn.x_loc;
                sem_type dt.d_type
              end;
              let dx' = { dx with d_loc = tn.x_loc; d_type = dt.d_type } in
              let env' = new_block env in
              add_def dx' env'; check_stmt body env'
            with Not_found -> () in
          List.iter check_arm arms;
          check_else env default
        end
    | _ -> failwith "check_typecase"

and check_else env =
  function
      Some ss -> not_07 "ELSE part" ss.s_loc; check_stmt ss env
    | None -> ()

(* check_typexpr -- check a type expression, returning the otype *)
and check_typexpr tx name env lzy = 
  match tx.tx_guts with
      TypeName x ->
	begin try
	  let d = lookup_typename x env in d.d_type
	with 
	  Not_found -> errtype
	end
    | _ ->
	let t = check_typecons tx name env lzy in
	t.t_name <- name; 
	if is_record t then make_desc t;
	t
	
and check_typecons tx name env lzy =
  match tx.tx_guts with
      Enum xs ->
	if not !Config.extensions then
	  sem_extend "enumerated types are not allowed" [] tx.tx_loc;
	let t = new_type !level 
	  (EnumType (List.length xs), int_rep, null_map) in
	let j = ref 0 in
	let dcl x = 
	  add_def (make_def x (EnumDef !j) t None) env; incr j in
	List.iter dcl xs; t
    | Pointer tx1 ->
	let d = typedef () in
	if lzy && (is_typename tx1 || !Config.extensions) then
	  agenda := (d, tx1) :: !agenda
	else begin
	  let t = check_typexpr tx1 anon env lzy in
	  check_target t tx1.tx_loc;
	  d.d_type <- t
	end;
	new_type !level (pointer d)
    | Array (upb, tx1) ->
	let v1 = check_tconst upb inttype env "an array bound"
 	and t2 = check_typexpr tx1 anon env lzy in
 	if is_flex t2 then
 	  sem_error "arrays cannot have open arrays as elements" [] tx1.tx_loc;
 	if is_abstract t2 then
 	  sem_error "arrays cannot have abstract record types as elements"
 	    [] tx1.tx_loc;
	if int_value v1 < integer 0 then
	  sem_error "upper bound of array must be >= 0" [] upb.e_loc;
	new_type !level (row (int_of_integer (int_value v1)) t2)
    | Flex tx1 ->
	let t1 = check_typexpr tx1 anon env lzy in
 	if is_abstract t1 then
 	  sem_error "open arrays cannot have abstract record types as elements"
 	    [] tx1.tx_loc;
	new_type !level (flex t1)
    | Record (abs, parent, fields) ->
	let offset = ref 0 in
	let (pt, fields0) = 
	  match parent with 
	      Some x -> 
		let t1 = check_typexpr x anon env false in
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
	let env' = add_block fields0 env in
	check_decls fields env' (upward_alloc offset) lzy;
	new_type !level (record abs pt tx.tx_loc !offset (top_block env'))
    | Proc heading ->
	let d = 
	  check_heading Procedure (makeDefId (anon, Private, no_loc)) 
	    heading None env (ref 0) in
	d.d_type
    | _ ->
	failwith "check_typecons"

(* check_decl -- check a declaration and add it to the environment *)
and check_decl d env alloc lzy = 
  match d with
      ConstDecl (x, e, doc) ->
	let t = check_expr e env in
	let d =
	  match e.e_guts with
	      Const v -> make_def x (ConstDef v) t doc
	    | String (str, n) -> 
		let d = make_def x StringDef t doc in
		d.d_lab <- str; d
	    | _ -> 
		sem_error "a CONST declaration must contain a constant" 
		  [] e.e_loc;
		make_def x (ConstDef (IntVal (integer 0))) errtype doc in
	add_def d env
    | VarDecl (kind, xs, tx, doc) ->
        if kind = CParamDef && not !Config.extensions then
	  sem_extend "CONST parameters are not allowed" [] (List.hd xs).x_loc;
        let t = check_typexpr tx anon env lzy in
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
	      end;
	end;

        if kind <> VParamDef && is_abstract t then
          sem_error "cannot declare instance of abstract record type"
            [] tx.tx_loc;

        let def x = 
	  begin match kind with
	      (ParamDef | CParamDef | VParamDef) ->
		if x.x_export <> Private then
		  sem_error "cannot export a parameter" [] x.x_loc
            | VarDef ->
                if !level > 0 && x.x_export <> Private then
                  sem_error "cannot export a local variable" [] x.x_loc
            | FieldDef -> ()
	    | _ -> failwith "var kind"
	  end;
          let kind' =
            (* In Oberon-07, aggregate value parameters are implicitly CONST *)
            if !Config.ob07flag && kind = ParamDef 
              && not (scalar t) then CParamDef else kind in
	  let d = make_def x kind' t doc in alloc d; add_def d env in
	List.iter def xs
    | TypeDecl decls ->
	List.iter (fun (x, te, doc) ->
	  let t = check_typexpr te x.x_name env true in
	  let d = make_def x TypeDef t doc in
	  x.x_def <- Some d; add_def d env) decls;
	force_agenda env
    | ProcDecl (kind, x, heading, body, doc) ->
	let fsize = 
	  match body with Block (_, _, _, fs) -> fs | NoBlock -> ref 0 in
	check_proc kind x heading doc env fsize
    | PrimDecl (x, heading, name, doc) ->
	if !level > 0 then
	  sem_error "primitives must be declared at the outermost level"
	    [] x.x_loc;
	check_proc Procedure x heading doc env (ref 0)
    | ForwardDecl (kind, x, heading, doc) -> ()
    | DummyDecl -> ()

and check_decls ds env alloc lzy = 
  List.iter (fun d -> check_decl d env alloc lzy) ds

and force_agenda env =
  List.iter (fun (d, tx) ->
    let t = check_typexpr tx anon env false in
    check_target t tx.tx_loc;
    d.d_type <- t) (List.rev !agenda);
  agenda := []

and check_heading kind x (Heading (fparams, result)) doc env fsize =
  incr level;
  let psize = ref 0 in
  let alloc = param_alloc frame_head psize in
  let env' = new_block env in
  check_decls fparams env' alloc false;
  decr level;
  let loc = ref no_loc in
  let rt = 
    (match result with
	Some t -> 
	  begin try
	    loc := t.x_loc; 
	    let d = lookup_typename t env in
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
  let t = new_type !level (proctype p) in
  let d = make_def x ProcDef t doc in
  d.d_env <- env'; d

(* check_proc -- check a procedure declaration *)
and check_proc kind x heading doc env fsize =
  let d = check_heading kind x heading doc env fsize in
  match kind with
      Procedure ->
	d.d_lab <- proc_name !current !level x.x_name;
	x.x_def <- Some d; add_def d env
    | Method | AbsMeth ->
        not_07 "method aka type-bound procedure" x.x_loc;
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
	    x.x_def <- Some d; add_method kind binder d
	  with Not_found -> ()
	end
    | _ -> failwith "check_proc"

(* check_body -- check body of a procedure declaration *)
and check_body d env =
  match d with
      ProcDecl (_, x, _, Block (locals, body, ret, fsize), _) ->
	if not (undefined x) then begin
	  let d = get_def x in
	  let p = get_proc d.d_type in
	  let env' = d.d_env in
	  let cxt = !err_context in
	  incr level; err_context := env';

	  fsize := if !level > 1 then word_size else 0;
	  check_decls locals env' (downward_alloc fsize) false;
	  List.iter (fun d -> check_body d env') locals;
	  return_type := p.p_result;
	  allocate := downward_alloc fsize;
	  check_stmt body env';
          
          begin match ret with
              Some e ->
                if same_types !return_type voidtype 
                    && not (is_errtype !return_type) then
                  sem_error "this procedure should not have a RETURN clause" 
                    [] e.e_loc
                else
                  check_assign e !return_type env'
                    "in this RETURN clause" []
            | None ->
                if not (same_types p.p_result voidtype) 
                    && not (check_return body) then
                  sem_warn "this typed procedure returns no result" 
                    [] x.x_loc
          end;

	  check_used (top_block env');

	  if !Error.err_count = 0 then
	    (* Don't check for uninitialised variables if there were errors *)
	    Inicheck.check_init body !level;

	  decr level; err_context := cxt;
	  List.iter (param_copy fsize) p.p_fparams;
	  align max_align fsize;
	  d.d_map <- local_map (top_block env')
	end

    | _ -> ()

let check_import (int, ext, stamp) env =
  if ext = !current then
    sem_error "a module must not import itself (boy, are you inept!)" 
      [] int.x_loc
  else begin
    try
      let env' =
        if ext = intern_sys "SYSTEM" then begin
           stamp := 0; sysenv ()
        end else begin
        let symfile = Symfile.import ext in

           stamp := symfile.y_checksum;
           symfile.y_env
        end in
      add_def (make_def int (ModDef (ext, env')) voidtype None) env
    with Not_found ->
      sem_error "the interface file for '$' cannot be found" 
	[fId ext] int.x_loc;
  end

let annotate (Module (m, imports, body, glodefs, doc)) =
  level := 0;
  let glo_env = new_block (init_env ()) in
  err_context := glo_env;
  List.iter (fun imp -> check_import imp glo_env) imports;
  if !Error.err_count = 0 then begin	
    (* No point going on if imports failed *)
    match body with
	Block (globals, body, None, fsize) ->
	  check_decls globals glo_env (global_alloc fsize) false;
	  List.iter check_methods (desc_table ());
	  is_module := false;
	  List.iter (fun d -> check_body d glo_env) globals;
	  return_type := voidtype;
	  allocate := downward_alloc fsize;
	  is_module := true;
	  level := 1;
	  check_stmt body glo_env;
	  level := 0;
	  align max_align fsize;

          let globals = top_block glo_env in
	  check_used globals;
	  glodefs := globals;

	  (* Hackily set the definition of the module to be the procedure
	     for the module body *)
	  let d = make_def m ProcDef bodytype doc in
	  d.d_lab <- sprintf "$.%main" [fId !current];
	  m.x_def <- Some d

      | _ -> failwith "annotate"
  end

