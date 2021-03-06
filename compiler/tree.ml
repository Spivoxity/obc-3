(*
 * tree.ml
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
open Print
open Eval
open Error

(* name -- type for applied occurrences, with mutable annotations *)
type name = 
  { x_name: ident; 		(* name of the reference *)
    x_module: ident;		(* module, or current if none specified *)
    x_export: export;		(* export mark (if defining occurrence) *)
    x_loc: location; 		(* line number *)
    mutable x_def: def option } (* definition in scope *)

(* abstract syntax *)
type program = 
  Module of name * import list * block * def list ref * docstring

and import = name * ident * int ref

and block = Block of decl list * stmt * expr option * int ref | NoBlock

and decl = 
    ConstDecl of name * expr * docstring
  | VarDecl of def_kind * name list * typexpr * docstring
  | TypeDecl of (name * typexpr * docstring) list
  | ProcDecl of proc_kind * name * proc_heading * block * docstring
  | PrimDecl of name * proc_heading * string * docstring
  | ForwardDecl of proc_kind * name * proc_heading * docstring
  | DummyDecl

and proc_heading = Heading of decl list * name option

and stmt = 
  { mutable s_guts: stmt_guts;	(* The statement itself *)
    s_loc: location }		(* Starting line *)

and stmt_guts =
    Assign of expr * expr
  | SimAssign of (expr * expr) list
  | ProcCall of expr
  | Return of expr option
  | IfStmt of (expr * stmt) list * stmt
  | CaseStmt of expr * (element list * stmt) list * stmt option
  | WhileStmt of (expr * stmt) list
  | RepeatStmt of stmt * expr
  | LoopStmt of stmt
  | ExitStmt
  | ForStmt of expr * expr * expr * expr * stmt * def option ref
  | WithStmt of (expr * name * stmt) list * stmt option
  | TypeCase of expr * (name * stmt) list * stmt option
  | Seq of stmt list
  | Skip
  | ErrStmt

and element = Single of expr | Range of expr * expr

and expr = 
  { mutable e_guts: expr_guts; 
    mutable e_type: otype;
    e_loc: location }		(* Position of main operator *)

and expr_guts =
    Name of name
  | Deref of expr
  | Sub of expr * expr 
  | Select of expr * name
  | Const of value
  | String of symbol * int
  | Nil
  | FuncCall of expr * expr list
  | MethodCall of expr * name * expr list
  | Convert of expr
  | Monop of op * expr 
  | Binop of op * expr * expr
  | Set of element list
  | Cast of expr * name
  | TypeTest of expr * name
  | Decimal of string

and typexpr = 
  { tx_guts: type_guts;
    tx_loc: location }

and type_guts =
    TypeName of name 
  | Enum of name list
  | Pointer of typexpr
  | Array of expr * typexpr
  | Flex of typexpr
  | Record of bool * typexpr option * decl list
  | Proc of proc_heading

let fQualId x = fQual (x.x_module, x.x_name)

let undefined x = (x.x_def = None)

(* get_def -- get stored definition of a name *)
let get_def x =
  match x.x_def with
      Some d -> d
    | None -> failwith (sprintf "missing def of $" [fQualId x])

(* get_name -- extract name from occurrence *)
let get_name e =
  match e.e_guts with
      Name x -> x
    | _ -> failwith "get_name"

(* Expr -- construct an expression node with dummy annotations *)
let makeExpr (e, p) = { e_guts = e; e_type = errtype; e_loc = p }

let makeStmt (s, n) = { s_guts = s; s_loc = n }

let makeTypexpr (t, p) = { tx_guts = t; tx_loc = p }

(* copy_expr -- copy an expr record *)
let copy_expr e = 
  { e_guts = e.e_guts; e_type = e.e_type; e_loc = e.e_loc }

(* edit_expr -- overwrite root of expr *)
let edit_expr e e1 = e.e_guts <- e1

(* is_string_const -- test if expression is a string constant *)
let is_string_const e =
  match e.e_guts with
      String (x, n) -> true
    | _ -> false

let rec subscripts e =
  match e.e_guts with
      Sub (e1, e2) -> subscripts e1 @ [e2]
    | _ -> []

let rec sub_base e =
  match e.e_guts with
      Sub (e1, e2) -> sub_base e1
    | _ -> e

(* safe -- test if an expression has no side effects or runtime errors *)
let rec safe e =
  match e.e_guts with
      Deref _ | Sub (_, _) -> false
    | Select (e1, x) -> safe e1
    | FuncCall (_, _) | MethodCall (_, _, _) -> false
    | Monop (w, e1) -> safe e1
    | Binop ((Div|Mod|Over), e1, e2) -> false
    | Binop (w, e1, e2) -> safe e1 && safe e2
    | Set els -> 
	let safe_el = 
	  function 
	      Single e1 -> safe e 
	    | Range (e1, e2) -> safe e1 && safe e2 in
	List.for_all safe_el els
    | Cast (_, _) -> false
    | TypeTest (_, _) -> false
    | _ -> true

(* makeName -- contruct a name node with dummy annotations *)
let makeName (m, x, n) = 
  { x_name = x; x_module = m; x_export = Private; 
    x_loc = n; x_def = None }

let makeDefId (x, s, n) =
  { x_name = x; x_module = !current; x_export = s;
    x_loc = n; x_def = None }


(* Grinder *)

(* (DEBUG-- *)
let fTail(fmt) xs = 
  fExt (fun prf -> List.iter (function x -> prf " $" [fmt x]) xs)

let fPSeq(fmt) xs = 
  fExt (fun prf ->
    match xs with
	[] -> prf "()" []
      | y::ys -> prf "($$)" [fmt y; fTail(fmt) ys])

let ppOpt f =
  function
      Some x -> fMeta "(SOME $)" [f x]
    | None -> fStr "(NONE)"

let ppName x = fId x.x_name

let ppImport (x, y, _) = fMeta "($ $)" [ppName x; fId y]

let ppExport =
  function
      Private -> fStr ""
    | ReadOnly -> fStr "-"
    | Visible -> fStr "*"

let ppDefId x = fMeta "$$" [fId x.x_name; ppExport x.x_export]

let ppKind k = 
  let kind = match k with 
	ConstDef _ -> "CONST" | EnumDef _ -> "ENUM" | StringDef -> "STRING"
      | TypeDef -> "TYPE" | VarDef -> "VAR" | ParamDef -> "PARAM"
      | VParamDef -> "VPARAM" | FieldDef -> "FIELD" | ProcDef -> "PROC"
      | PrimDef -> "PRIM" | ModDef (x, env) -> "MODULE" 
      | CParamDef -> "CPARAM" in
  fStr kind

let ppPKind k =
  let kind = match k with Procedure -> "PROC" | Method -> "METHOD" 
      | AbsMeth -> "ABSMETH" | Body -> "BODY" in
  fStr kind

let rec ppExpr e = 
  fExt (fun prf ->
    match e.e_guts with
	Name x -> prf "$" [fQualId x]
      | Deref e1 -> prf "(DEREF $)" [ppExpr e1]
      | Const v -> prf "$" [fVal v]
      | Sub (e1, e2) -> prf "(SUB $ $)" [ppExpr e1; ppExpr e2]
      | Select (e1, x) -> prf "(SELECT $ $)" [ppExpr e1; ppName x]
      | String (s, n) -> prf "(STRING $ $)" [fSym s; fNum n]
      | Decimal s -> prf "(DECIMAL $)" [fStr s]
      | Nil -> prf "(NIL)" []
      | FuncCall (f, args) ->
	  prf "(CALL $$)" [ppExpr f; fTail(ppExpr) args]
      | MethodCall (r, x, args) ->
	  prf "(METHCALL $ $$)" 
	    [ppExpr r; ppName x; fTail(ppExpr) args]
      | Convert e1 ->
	  prf "(CONVERT $)" [ppExpr e1]
      | Monop (w, e1) ->
	  prf "(MONOP_\"$\" $)" [fOp w; ppExpr e1]
      | Binop (w, e1, e2) ->
	  prf "(BINOP_\"$\" $ $)" [fOp w; ppExpr e1; ppExpr e2]
      | Set els ->
	  prf "(SET" [];
	  List.iter (function e -> prf " $" [ppElement e]) els;
	  prf ")" []
      | Cast (e1, x) -> prf "(CAST $ $)" [ppExpr e1; fQualId x]
      | TypeTest (e1, x) -> prf "(TYPETEST $ $)" [ppExpr e1; fQualId x])

and ppElement e = 
  fExt (fun prf ->
    match e with
	Single e -> prf "$" [ppExpr e]
      | Range (e1, e2) -> prf "(RANGE $ $)" [ppExpr e1; ppExpr e2])

let rec ppStmt s =
  fExt (fun prf ->
    match s.s_guts with
        Assign (e1, e2) -> 
	  prf "(ASSIGN $ $)" [ppExpr e1; ppExpr e2]
      | SimAssign pairs ->
	  let ppPair (e1, e2) = fMeta "($ $)" [ppExpr e1; ppExpr e2] in
	  prf "(SIMASSIGN$)" [fTail(ppPair) pairs]
      | ProcCall e -> prf "$" [ppExpr e]
      | Return eo ->
	  (match eo with
	      Some e -> prf "(RETURN $)" [ppExpr e]
	    | None -> prf "(RETURN)" [])
      | IfStmt (arms, elsept) ->
	  prf "(IF$ $)" 
	    [fList(fun (e, s) -> fMeta " $ $" [ppExpr e; ppStmt s]) arms;
	      ppStmt elsept]
      | CaseStmt (sw, arms, elsept) ->
	  prf "(CASE $" [ppExpr sw];
	  List.iter (function (labs, stmts) ->
	    prf " (LABELS$) $" [fTail(ppElement) labs; ppStmt stmts]) arms;
	  (match elsept with Some s -> prf " ELSE $" [ppStmt s] | None -> ());
	  prf ")" []
      | WhileStmt arms ->
	  prf "(WHILE$)" 
	    [fList(fun (e, s) -> fMeta " $ $" [ppExpr e; ppStmt s]) arms]
      | RepeatStmt (body, test) ->
	  prf "(REPEAT $ $)" [ppStmt body; ppExpr test]
      | LoopStmt body ->
	  prf "(LOOP $)" [ppStmt body]
      | ExitStmt -> 
	  prf "(EXIT)" []
      | ForStmt (v, lo, hi, step, body, _) ->
	  prf "(FOR $ $ $ $ $)" 
	    [ppExpr v; ppExpr lo; ppExpr hi; ppExpr step; ppStmt body]
      | WithStmt (arms, elsept) ->
	  prf "(WITH" [];
	  List.iter (function (x, t, body) ->
	    prf " $_$ $" [ppExpr x; fQualId t; ppStmt body]) arms;
	  (match elsept with Some s -> prf " ELSE $" [ppStmt s] | None -> ());
	  prf ")" []
      | TypeCase (switch, arms, elsept) ->
          prf "(TYPECASE $" [ppExpr switch];
          List.iter (function (t, body) ->
            prf " $ $" [fQualId t; ppStmt body]) arms;
	  (match elsept with Some s -> prf " ELSE $" [ppStmt s] | None -> ());
	  prf ")" []
      | Seq stmts ->
	  prf "(SEQ$)" [fTail(ppStmt) stmts]
      | Skip ->
	  prf "(SKIP)" []
      | ErrStmt ->
	  prf "(ERRSTMT)" [])

and ppTypexpr te = 
  fExt (fun prf ->
    match te.tx_guts with
	TypeName tn -> prf "$" [fQualId tn]
      | Enum xs -> prf "(ENUM$)" [fTail(ppDefId) xs]
      | Pointer te1 -> prf "(POINTER $)" [ppTypexpr te1]
      | Array (e, te1) -> prf "(ARRAY $ $)" [ppExpr e; ppTypexpr te1]
      | Flex te1 -> prf "(FLEX $)" [ppTypexpr te1]
      | Record (abs, p, fields) -> 
	  let parent = 
	    match p with Some x -> ppTypexpr x | None -> fStr "NONE" in
	  prf "(RECORD $$$)" 
	    [fStr (if abs then "ABS " else ""); parent; fTail(ppDecl) fields]
      | Proc hdg -> prf "(PROC $)" [ppHeading hdg])

and ppDecl d = 
  fExt (fun prf ->
    match d with
        ConstDecl (x, e, _) -> 
	  prf "(CONST $ $)" [ppDefId x; ppExpr e]
      | VarDecl (k, xs, te, _) -> 
	  prf "($ $ $)" 
	    [ppKind k; fPSeq(ppDefId) xs; ppTypexpr te]
      | TypeDecl decls ->
	  let pp (x, te, _) = fMeta "($ $)" [ppDefId x; ppTypexpr te] in
	  prf "(TYPE $)" [fPSeq(pp) decls]
      | ProcDecl (k, x, hdg, body, _) ->
	  prf "($ $ $ $)" 
	    [ppPKind k; ppDefId x; ppHeading hdg; ppBlock body]
      | PrimDecl (x, hdg, name, _) ->
	  prf "(PRIM $ $ = \"$\")"
            [ppDefId x; ppHeading hdg; fStr name]
      | ForwardDecl (k, x, hdg, _) ->
	  prf "($ $ $)" [ppPKind k; ppDefId x; ppHeading hdg]
      | DummyDecl -> prf "(DUMMY)" [])

and ppHeading (Heading (fps, rt)) = 
  fExt (fun prf ->
    let result = match rt with Some t -> fQualId t | None -> fStr "NONE" in
    prf "$ $" [fPSeq(ppDecl) fps; result])

and ppBlock =
  function
      Block (decls, stmts, ret, fsize) ->
        fMeta "(BLOCK (DECLS$) $$)" 
          [fTail(ppDecl) decls; ppStmt stmts;
            (match ret with Some e -> fMeta " (RETURN $)" [ppExpr e]
              | None -> fStr "")]
    | NoBlock -> fStr "(NOBLOCK)"

let ppTree (Module (m, imports, body, _, _)) = 
  let f prf = prf "(MODULE $ (IMPORTS$) $)" 
    [ppName m; fTail(ppImport) imports; ppBlock body] in
  fExt f

let print_tree fp t = fgrindf fp "$" [ppTree t]

let print_expr fp e = fgrindf fp "$" [ppExpr e]
(* --DEBUG) *)
