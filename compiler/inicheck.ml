(*
 * inicheck.ml
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

let rec check_expr i0 e =
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
	check_expr i0 e1
    | Sub (e1, e2) ->
	check_all i0 [e1; e2]
    | Select (e1, x) ->
	check_expr i0 e1
    | (Const (_, _) | String (_, _) | Nil | Decimal _) -> i0
    | FuncCall (f, args) ->
	begin match f.e_type.t_guts with
	    ProcType p ->
	      let i1 = check_expr i0 f in
	      let i2 = check_args i0 p.p_fparams args in
	      inijoin i1 i2
	  | BuiltinType b ->
	      begin match b.b_id with
		  NewProc ->
		    (* NEW initialises its first argument *)
		    let i1 = check_assign i0 (List.hd args) in
		    let i2 = check_all i0 (List.tl args) in
		    inijoin i1 i2
		| AdrFun -> i0
		| _ -> check_all i0 args
	      end
	  | _ -> failwith "inicheck FuncCall"
	end
    | MethodCall (rcvr, m, args) ->
	let d = get_def m in
	let p = get_proc d.d_type in
	let i1 = check_expr i0 rcvr in
	let i2 = check_args i0 (List.tl p.p_fparams) args in
	inijoin i1 i2
    | Monop (w, e1) ->
	check_expr i0 e1
    | Binop ((And|Or), e1, e2) ->
	let i1 = check_expr i0 e1 in
	ignore (check_expr i1 e2); i1
    | Binop (w, e1, e2) ->
	check_all i0 [e1; e2]
    | Set elts ->
	let get_exprs = 
	  function Single e1 -> [e1] | Range (e1, e2) -> [e1; e2] in
	check_all i0 (List.concat (List.map get_exprs elts))
    | Convert e1 ->
	check_expr i0 e1
    | Cast (e1, t) ->
	check_expr i0 e1
    | TypeTest (e1, t) ->
	check_expr i0 e1

and check_args i0 formals actuals =
  let check_arg d e =
    match d.d_kind with
        (ParamDef | CParamDef) -> check_expr i0 e
      | VParamDef -> check_assign i0 e 
      | _ -> failwith "inicheck_arg" in
  List.fold_left inijoin i0 (List.map2 check_arg formals actuals)

and check_assign i0 e =
  match e.e_guts with
      Name x ->
	let d = get_def x in
	if d.d_kind = VarDef && d.d_level = !level then
	  iniadd x.x_name i0
	else
	  i0
    | _ ->
	check_expr i0 e

and check_all i0 es =
  List.fold_left inijoin i0 (List.map (check_expr i0) es)

let exitset = ref iniempty

let rec check_stmt i0 s =
  match s.s_guts with
      Assign (e1, e2) ->
	let i1 = check_assign i0 e1 in
	let i2 = check_expr i0 e2 in
	inijoin i1 i2
    | ProcCall e1 ->
	check_expr i0 e1
    | IfStmt (arms, elsept) ->
	(* Loop over the arms, keeping i = iniset after evaluation of
	   the chain of guards, j = iniset after execution of any loop
	   body *)
	let (i1, j1) = 
	  List.fold_left (fun (i, j) (e, s) ->
	      let i' = check_expr i e in
	      let j' = check_stmt i' s in
	      (i', inimeet j j'))
	    (i0, Universe) arms in
	let j2 = check_stmt i1 elsept in
	inimeet j1 j2
    | WhileStmt ((test1, body1)::arms) ->
	let i1 = check_expr i0 test1 in
	ignore (check_stmt i1 body1);
	ignore (List.fold_left (fun i (e, s) ->
	    let i' = check_expr i e in
	    ignore (check_stmt i' s); i')
	  i1 arms);
	i1
    | WhileStmt [] -> failwith "inicheck WhileStmt"
    | RepeatStmt (body, test) ->
	let i1 = check_stmt i0 body in
	check_expr i1 test
    | ForStmt (var, lo, hi, step, body, _) ->
	let i1 = check_all i0 [lo; hi] in
	let i2 = check_assign i1 var in
	ignore (check_stmt i2 body); i2
    | CaseStmt (e1, arms, elsept) ->
	let i1 = check_expr i0 e1 in
	let i2 = 
	  List.fold_left 
	    (fun i (_, s1) -> let i' = check_stmt i1 s1 in inimeet i i')
	    Universe arms in
	let i3 = check_else i1 elsept in
	inimeet i2 i3
    | WithStmt (arms, elsept) ->
	let i1 = 
	  List.fold_left
	    (fun i (x, _, body) ->
	      let i' = check_expr i0 x in
	      inimeet i (check_stmt i' body))
	    Universe arms in
	let i2 = check_else i0 elsept in
	inimeet i1 i2
    | TypeCase (x, arms, elsept) ->
        let i1 = check_expr i0 x in
        let i2 =
          List.fold_left
            (fun i (_, body) ->
              inimeet i (check_stmt i body))
            Universe arms in
        let i3 = check_else i1 elsept in
        inimeet i2 i3
    | Seq ss ->
	List.fold_left check_stmt i0 ss
    | Skip -> i0
    | Return _ -> Universe
    | LoopStmt body ->
	(* We gather all the init sets from EXIT statments in body *)
	let saveexit = !exitset in
	exitset := Universe;
	ignore (check_stmt i0 body);
	let i1 = !exitset in
	exitset := saveexit; i1
    | ExitStmt -> 
	exitset := inimeet i0 !exitset;
	Universe
    | ErrStmt -> Universe

and check_else i0 =
  function
      Some s1 -> check_stmt i0 s1 
    | None -> Universe

let check_init lev s = 
  level := lev; warned := IdSet.empty;
  ignore (check_stmt iniempty s)
