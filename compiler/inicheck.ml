(*
 * inicheck.ml
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
open Tree
open Dict
open Error
open Print

module IdSet = Set.Make(struct
  type t = ident
  let compare = compare
end)  

let level = ref 0
let warned = ref IdSet.empty

type iniset = 
    FinSet of IdSet.t			(* a finite set *)
  | Universe				(* universal set *)

let iniempty = FinSet IdSet.empty

let iniadd x =
  function
      FinSet s -> FinSet (IdSet.add x s)
    | Universe -> Universe

let inijoin s1 s2 =
  match (s1, s2) with
      (_, Universe) -> Universe
    | (Universe, _) -> Universe
    | (FinSet t1, FinSet t2) -> FinSet (IdSet.union t1 t2)

let inimeet s1 s2 =
  match (s1, s2) with
      (_, Universe) -> s1
    | (Universe, _) -> s2
    | (FinSet t1, FinSet t2) -> FinSet (IdSet.inter t1 t2)

let inimem x =
  function
      FinSet s -> IdSet.mem x s
    | Universe -> true

let maybe_add x i0 =
  let d = get_def x in
  if d.d_kind = VarDef && d.d_level = !level then
    iniadd x.x_name i0
  else
    i0

let rec check_expr e i0 =
  match e.e_guts with
      Name x ->
	let d = get_def x in
	(* Complain only about local, scalar variables -- and only once *)
	if d.d_kind = VarDef && d.d_level = !level 
	    && scalar (d.d_type) && not (inimem x.x_name i0) 
	    && not (IdSet.mem x.x_name !warned) then begin
	  sem_warn "variable $ may not be initialised" [fId x.x_name] e.e_loc;
	  warned := IdSet.add x.x_name !warned
	end;
	i0
    | Deref e1 ->
	check_expr e1 i0 
    | Sub (e1, e2) ->
	check_all [e1; e2] i0
    | Select (e1, x) ->
	check_expr e1 i0 
    | (Const _ | String (_, _) | Nil | Decimal _) -> i0
    | FuncCall (f, args) ->
	begin match f.e_type.t_guts with
	    ProcType p ->
	      let i1 = check_expr f i0 in
	      let i2 = check_args p.p_fparams args i0 in
	      inijoin i1 i2
	  | BuiltinType b ->
	      begin match b.b_id with
		  NewProc ->
		    (* NEW initialises its first argument *)
		    let i1 = check_assign (List.hd args) i0 in
		    let i2 = check_all (List.tl args) i0 in
		    inijoin i1 i2
		| AdrFun -> i0
		| _ -> check_all args i0
	      end
	  | _ -> failwith "inicheck FuncCall"
	end
    | MethodCall (rcvr, m, args) ->
	let d = get_def m in
	let p = get_proc d.d_type in
	let i1 = check_expr rcvr i0 in
	let i2 = check_args (List.tl p.p_fparams) args i0 in
	inijoin i1 i2
    | Monop (w, e1) ->
	check_expr e1 i0
    | Binop ((And|Or), e1, e2) ->
	let i1 = check_expr e1 i0 in
	ignore (check_expr e2 i1); i1
    | Binop (w, e1, e2) ->
	check_all [e1; e2] i0
    | Set elts ->
	let get_exprs = 
	  function Single e1 -> [e1] | Range (e1, e2) -> [e1; e2] in
	check_all (List.concat (List.map get_exprs elts)) i0
    | Convert e1 ->
	check_expr e1 i0
    | Cast (e1, t) ->
	check_expr e1 i0
    | TypeTest (e1, t) ->
	check_expr e1 i0

and check_args formals actuals i0 =
  let check_arg d e =
    match d.d_kind with
        (ParamDef | CParamDef) -> check_expr e i0
      | VParamDef -> check_assign e i0 
      | _ -> failwith "inicheck_arg" in
  List.fold_left inijoin i0 (List.map2 check_arg formals actuals)

and check_assign e i0 =
  match e.e_guts with
      Name x ->
	let d = get_def x in
	if d.d_kind = VarDef && d.d_level = !level then
	  iniadd x.x_name i0
	else
	  i0
    | _ ->
	check_expr e i0

and check_all es i0 =
  List.fold_left inijoin i0 (List.map (fun e -> check_expr e i0) es)

let exitset = ref iniempty

let rec check_stmt s i0 =
  match s.s_guts with
      Assign (e1, e2) ->
	let i1 = check_assign e1 i0 in
	let i2 = check_expr e2 i0 in
	inijoin i1 i2
    | SimAssign pairs ->
	List.fold_left (fun i (e1, e2) ->
	    let i1 = check_assign e1 i0 in
	    let i2 = check_expr e2 i0 in
	    inijoin i (inijoin i1 i2))
	  i0 pairs
    | ProcCall e1 ->
	check_expr e1 i0
    | IfStmt (arms, elsept) ->
	(* Loop over the arms, keeping i = iniset after evaluation of
	   the chain of guards, j = iniset after execution of any body *)
	let (i1, j1) = 
	  List.fold_left (fun (i, j) (e, s) ->
	      let i' = check_expr e i in
	      let j' = check_stmt s i' in
	      (i', inimeet j j'))
	    (i0, Universe) arms in
	let j2 = check_stmt elsept i1 in
	inimeet j1 j2
    | WhileStmt ((test1, body1)::arms) ->
	let i1 = check_expr test1 i0 in
	ignore (check_stmt body1 i1);
	ignore (List.fold_left (fun i (e, s) ->
	    let i' = check_expr e i in
	    ignore (check_stmt s i'); i')
	  i1 arms);
	i1
    | WhileStmt [] -> failwith "inicheck WhileStmt"
    | RepeatStmt (body, test) ->
	let i1 = check_stmt body i0 in
	check_expr test i1
    | ForStmt (var, lo, hi, step, body, _) ->
	let i1 = check_all [lo; hi] i0 in
	let i2 = check_assign var i1 in
	ignore (check_stmt body i2); i2
    | CaseStmt (e1, arms, elsept) ->
	let i1 = check_expr e1 i0 in
	let i2 = 
	  List.fold_left 
	    (fun i (_, s1) -> let i' = check_stmt s1 i1 in inimeet i i')
	    Universe arms in
	let i3 = check_else elsept i1 in
	inimeet i2 i3
    | WithStmt (arms, elsept) ->
	let i1 = 
	  List.fold_left
	    (fun i (x, _, body) ->
	      let i' = check_expr x i0 in
	      inimeet i (check_stmt body i'))
	    Universe arms in
	let i2 = check_else elsept i0 in
	inimeet i1 i2
    | TypeCase (x, arms, elsept) ->
        let i1 = check_expr x i0 in
        let i2 =
          List.fold_left
            (fun i (_, body) ->
              inimeet i (check_stmt body i1))
            Universe arms in
        let i3 = check_else elsept i1 in
        inimeet i2 i3
    | Seq ss ->
	List.fold_left (fun i s -> check_stmt s i) i0 ss
    | Skip -> i0
    | Return _ -> Universe
    | LoopStmt body ->
	(* We gather all the init sets from EXIT statments in body *)
	let saveexit = !exitset in
	exitset := Universe;
	ignore (check_stmt body i0);
	let i1 = !exitset in
	exitset := saveexit; i1
    | ExitStmt -> 
	exitset := inimeet i0 !exitset;
	Universe
    | ErrStmt -> Universe

and check_else sz i0 =
  match sz with
      Some s1 -> check_stmt s1 i0 
    | None -> Universe

let check_init s lev = 
  level := lev; warned := IdSet.empty;
  ignore (check_stmt s iniempty)
