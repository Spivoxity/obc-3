/*
 * parser.mly
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
 */

%{
open Symtab
open Dict
open Tree
open Eval
open Error
open Print
%}

%token <Symtab.ident>	IDENT
%token <Symtab.op>	MULOP ADDOP RELOP
%token <Eval.integer>	NUMBER
%token <float>		FLOCON DBLCON
%token <char>		CHAR
%token <string> 	STRING DECIMAL

/* punctuation */
%token			SEMI DOT COLON LPAR RPAR COMMA SUB BUS LBRACE RBRACE
%token			STAR UPARROW EQUAL MINUS PLUS ASSIGN VBAR DOTDOT 
%token			BADTOK INFINITY

/* keywords */
%token			ARRAY BEGIN CONST DO ELSE ELSIF END IF IMPORT IS OF 
%token			FOR MODULE PROCEDURE RECORD REPEAT RETURN THEN TO TYPE 
%token			UNTIL VAR WHILE NOT POINTER NIL WITH
%token			CASE LOOP EXIT BY 
%token			ABSTRACT

/* operator priorities -- most needed only because of error productions */
%right			error

%right			COMMA ADDOP RELOP EQUAL MINUS PLUS IS MULOP
			STAR IDENT LPAR DOT SUB UPARROW RPAR BUS IF SEMI

%right			INFINITY

%start<Tree.program>	program

%{
let parse_error msg = 
  syn_error "$ at token '$'" [fStr msg; fToken] (here ())

let parse_error2 msg loc1 =
  syn_error2 "$ at token '$'" [fStr msg; fToken] loc1 (here ()) 

let missing_semi loc =
  syn_error "missing ';'" [] loc

(*
let _ = Random.self_init ()

let shown = ref false

let unmatched loc = 
  let msg =
    if not !shown && Random.float 1.0 < 0.2 then
      (shown := true;
        "\"(an unmatched left parenthesis creates an unresolved"
	^ " tension that will stay with you all day\"")
    else
      "unmatched left parenthesis" in
  parse_error2 msg loc;
  raise Parse_error
*)

let lloc () = (symbol_start (), symbol_end ())
let rloc n = (rhs_start n, rhs_end n)

let end_name = ref anon

let check_end bx ex loc =
  let ok = match ex with Some s -> s = bx || bx = anon | None -> false in
  if not ok then
    syn_error "expected identifier '$' after END" [fId bx] loc

let check_modname x =
  if x <> !current then
    syn_error "module name does not match file name" [] (here ())

(* make_call -- add empty params if needed in procedure call *)
let make_call e =
  match e.e_guts with
      FuncCall _ -> e
    | _ -> makeExpr (FuncCall (e, []), e.e_loc)

let exp e = makeExpr (e, lloc ())
let typexp tx = makeTypexpr (tx, lloc ())

let has_proc ds = 
  List.exists (function ProcDecl _ -> true | _ -> false) ds
%}
    
%%

program :	
    doc MODULE modname semi imports block DOT	
	{ Module ($modname, $imports, $block, ref [], $doc) } ;

modname :
    name			{ check_modname $name.x_name; 
				  end_name := $name.x_name; $name } ;

imports :
    /* empty */			{ [] }
  | IMPORT implist SEMI		{ $implist } ;

implist :
    import			{ [$import] }
  | import COMMA implist	{ $import :: $implist } ;

import :
    name			{ ($name, $name.x_name, ref 0) }
  | name ASSIGN IDENT		{ ($name, $IDENT, ref 0) } ;

block :	
    blockid decls END ident
      { check_end $blockid $ident (rloc 4); 
        Block ($decls, makeStmt (Skip, no_loc), ref 0) }
  | blockid decls BEGIN stmts END ident
      { check_end $blockid $ident (rloc 6);
        Block ($decls, $stmts, ref 0) } ;

blockid :
    /* EMPTY */			{ !end_name } ;

decls :
    /* empty */ %prec error	{ [] }
  | decls decl			{ $decls @ $decl }
  | decls proc			{ $decls @ [$proc] } 
  | decls error SEMI 		{ $decls } ;

decl :	
    CONST cdecls		{ $cdecls }
  | VAR vdecls			{ $vdecls }
  | TYPE tdecls			{ [TypeDecl $tdecls] } ;

cdecls :
    /* empty */ %prec INFINITY  { [] }
  | cdecl cdecls		{ $cdecl :: $cdecls } ;

cdecl :
    doc defid EQUAL expr semi	{ ConstDecl ($defid, $expr, $doc) } ;

tdecls :
    /* empty */ %prec INFINITY  { [] }
  | tdecl tdecls		{ $tdecl :: $tdecls } ;

tdecl :	
    doc defid EQUAL texpr semi { ($defid, $texpr, $doc) } ;

vdecls :
    /* empty */ %prec INFINITY  { [] }
  | vdecl vdecls		{ $vdecl :: $vdecls } ;

vdecl :
    doc defids COLON texpr semi { VarDecl (VarDef, $defids, $texpr, $doc) } ;

texpr :	
    tname			{ $tname }
  | LPAR defids RPAR		{ typexp (Enum $defids) }
  | POINTER TO texpr		{ typexp (Pointer $texpr) }
  | ARRAY exprs OF texpr	{ let array n t = typexp (Array (n, t)) in
				  List.fold_right array $exprs $texpr }
  | ARRAY OF texpr		{ typexp (Flex $texpr) }
  | RECORD parent fields END 	{ typexp (Record (false, $parent, $fields)) }
  | ABSTRACT RECORD parent fields END 
    	     	    	   	{ typexp (Record (true, $parent, $fields)) }
  | PROCEDURE params		{ typexp (Proc $params) } ;

tname :
    qualid			{ typexp (TypeName $qualid) } ;

parent :
    /* empty */			{ None }
  | LPAR tname RPAR		{ Some $tname } ;

fields :
    fieldlist			{ $fieldlist }
  | fieldlist SEMI fields	{ $fieldlist @ $fields } ;

fieldlist :	
    /* empty */			{ [] }
  | doc defids COLON texpr	{ [VarDecl (FieldDef, $defids, 
							$texpr, $doc)] } ;

proc :
    doc PROCEDURE procid params semi block semi	
      { ProcDecl (Procedure, $procid, $params, $block, $doc) }
  | doc PROCEDURE rcvr procid params semi block semi
      { let (Heading (ps, r)) = $params in
        ProcDecl (Method, $procid, Heading ($rcvr::ps, r), $block, $doc) }
  | doc ABSTRACT PROCEDURE rcvr procid params semi
      { let (Heading (ps, r)) = $params in
        ProcDecl (AbsMeth, $procid, Heading ($rcvr::ps, r), NoBlock, $doc) }
  | doc PROCEDURE procid params IS STRING semi
      { PrimDecl ($procid, $params, $STRING, $doc) } 
  | doc PROCEDURE error { end_name := anon } block semi
      { DummyDecl } 
  | doc PROCEDURE UPARROW procid params semi
      { ForwardDecl (Procedure, $procid, $params, $doc) }
  | doc PROCEDURE UPARROW rcvr procid params semi
      { let (Heading (ps, r)) = $params in
        ForwardDecl (Method, $procid, Heading ($rcvr::ps, r), $doc) } ;

procid :
    defid			{ end_name := $defid.x_name; $defid } ;

rcvr :
    LPAR defid COLON tname RPAR 
      { VarDecl (ParamDef, [$defid], $tname, None) }
  | LPAR CONST defid COLON tname RPAR 
      { VarDecl (CParamDef, [$defid], $tname, None) } 
  | LPAR VAR defid COLON tname RPAR 
      { VarDecl (VParamDef, [$defid], $tname, None) } ;

params :
    /* empty */			{ Heading ([], None) }
  | LPAR RPAR result		{ Heading ([], $result) }
  | LPAR formals RPAR result	{ Heading ($formals, $result) } ;

formals :	
    formal			{ [$formal] }
  | formal semi formals		{ $formal :: $formals } ;

formal :	
    defids COLON texpr		{ VarDecl (ParamDef, $defids, $texpr, None) }
  | CONST defids COLON texpr	{ VarDecl (CParamDef, $defids, $texpr, None) }
  | VAR defids COLON texpr	{ VarDecl (VParamDef, $defids, $texpr, None) } ;

result :
    /* empty */			{ None }
  | COLON qualid		{ Some $qualid } ;

ident :
    /* empty */			{ None }
  | IDENT			{ Some $IDENT } ;

/* This rather complicated syntax for 'stmts' is designed to cope
gracefully with missing and duplicated semicolons.  The nonterminal
'stmts0' generates sequences that (if non-empty) end with a
semicolon, and 'stmts1' generates non-empty sequences that do not end
with a semicolon.  Missing semicolons are inserted before any
statement that begins with a keyword. 

The salient fact is that the parser ends up with two states, one
(linked to stmts0) where it has seen a semicolon and is ready to see
the next statement, and another (linked to stmts1) where it needs to
see or insert a semicolon before continuing the sequence. */

stmts :
    stmts0			{ makeStmt (Seq (List.rev $stmts0), lloc ()) }
  | stmts1			{ makeStmt (Seq (List.rev $stmts1), lloc ()) } ;

stmts0 :
    /* empty */			{ [] }
  | stmts0 SEMI			{ $stmts0 }
  | stmts1 SEMI			{ $stmts1 } 
  | stmts1 error SEMI		{ $stmts1 } ;

stmts1 :
    stmts0 stmt0		{ makeStmt ($stmt0, rloc 2) :: $stmts0 } 
  | stmts0 stmt1		{ makeStmt ($stmt1, rloc 2) :: $stmts0 } 
  | stmts1 missing stmt1	{ makeStmt ($stmt1, rloc 3) :: $stmts1 } ;

missing :
    /* empty */ %prec error	{ missing_semi (lloc ()) } ;

stmt0 :
    desig ASSIGN expr		{ Assign ($desig, $expr) }
  | desig COMMA desigs ASSIGN expr COMMA exprs
      { if List.length $desigs <> List.length $exprs then
	  syn_error "Wrong number of expressions on RHS" [] (here ());
	SimAssign (List.combine ($desig::$desigs) ($expr::$exprs)) }
  | desig			{ ProcCall (make_call $desig) } ;

stmt1 :
    RETURN %prec error		{ Return None }
  | RETURN expr 		{ Return (Some $expr) }
  | ifs END 			{ IfStmt ($ifs, makeStmt (Skip, no_loc)) }
  | ifs ELSE stmts END 		{ IfStmt ($ifs, $stmts) }
  | whiles END			{ WhileStmt $whiles }
  | CASE expr OF cases else END { CaseStmt ($expr, $cases, $else) }
  | CASE error cases else END   { ErrStmt }
  | REPEAT stmts UNTIL expr	{ RepeatStmt ($stmts, $expr) }
  | LOOP stmts END		{ LoopStmt $stmts }
  | EXIT			{ ExitStmt }
  | FOR desig ASSIGN expr@e1 TO expr@e2 step DO stmts END
      { ForStmt ($desig, $e1, $e2, $step, $stmts, ref dummy_def) } 
  | WITH branches else END	{ WithStmt ($branches, $else) }
  | VAR vdecls BEGIN stmts END	{ LocalStmt ($vdecls, $stmts) }
  | error			{ ErrStmt } ;

/* We make 'ifs' left recursive for better error recovery. */

ifs :
    IF expr THEN stmts		{ [($expr,$stmts)] }
  | IF error stmts		{ [] }
  | ifs ELSIF expr THEN stmts   { $ifs @ [($expr, $stmts)] }
  | ifs ELSIF error stmts	{ $ifs } ;

whiles :
    WHILE expr DO stmts		{ [($expr, $stmts)] }
  | WHILE error stmts		{ [] }
  | whiles ELSIF expr DO stmts  { $whiles @ [($expr, $stmts)] }
  | whiles ELSIF error stmts	{ $whiles } ;

cases :
    case			{ $case }
  | case VBAR cases		{ $case @ $cases } ;

case :
    /* EMPTY */			{ [] }
  | elements COLON stmts	{ [($elements, $stmts)] };

elements :
    element			{ [$element] }
  | element COMMA elements	{ $element :: $elements } ;

element :
    expr			{ Single $expr }
  | expr@e1 DOTDOT expr@e2	{ Range ($e1, $e2) } ;

branches :
    branch			{ [$branch] }
  | branch VBAR branches	{ $branch :: $branches } ;

branch :
    name COLON qualid DO stmts	
      { (makeExpr (Name $name, rloc 1), $qualid, $stmts) } ;

else :
    /* empty */			{ None }
  | ELSE stmts			{ Some $stmts } ;

step :
    /* empty */			{ exp (Const (IntVal (integer 1), numtype)) }
  | BY expr			{ $expr } ;

expr :
    simple %prec error		{ $simple }
  | simple@e1 RELOP simple@e2	{ exp (Binop ($RELOP, $e1, $e2)) }
  | simple@e1 EQUAL simple@e2	{ exp (Binop (Eq, $e1, $e2)) } 
  | simple IS qualid		{ exp (TypeTest ($simple, $qualid)) } 
  | IF expr@e1 THEN expr@e2 ELSE expr@e3  { exp (IfExpr ($e1, $e2, $e3)) } ;

simple :
    term %prec error		{ $term }
  | PLUS term			{ exp (Monop (Uplus, $term)) }
  | MINUS term			{ exp (Monop (Uminus, $term)) }
  | simple PLUS term		{ exp (Binop (Plus, $simple, $term)) }
  | simple MINUS term		{ exp (Binop (Minus, $simple, $term)) }
  | simple ADDOP term		{ exp (Binop ($ADDOP, $simple, $term)) } ;

term :
    factor			{ $factor }
  | term MULOP factor		{ exp (Binop ($MULOP, $term, $factor)) }
  | term STAR factor		{ exp (Binop (Times, $term, $factor)) } ;

factor :
    NUMBER			{ exp (Const (IntVal $NUMBER, numtype)) }
  | DECIMAL			{ exp (Decimal $DECIMAL) }
  | FLOCON			{ exp (Const (FloVal $FLOCON, realtype)) }
  | DBLCON			{ exp (Const (FloVal $DBLCON, longreal)) }
  | CHAR			{ exp (Const (IntVal (integer
					    (int_of_char $CHAR)), character)) }
  | STRING			{ exp (String (save_string $STRING, 
					    String.length $STRING)) }
  | NIL				{ exp Nil }
  | desig %prec error		{ $desig }
  | LBRACE RBRACE		{ exp (Set []) }
  | LBRACE elements RBRACE	{ exp (Set $elements) }
  | NOT factor			{ exp (Monop (Not, $factor)) }
  | LPAR expr RPAR		{ $expr } 
  | LPAR expr %prec error	{ parse_error2 
				    "unmatched left parenthesis" (rloc 1);
				  raise Parse_error } ;

desig :
    name			{ exp (Name $name) }
  | desig UPARROW		{ exp (Deref $desig) }
  | desig SUB exprs BUS		{ let sub a i = exp (Sub (a, i)) in
				  List.fold_left sub $desig $exprs }
  | desig SUB exprs %prec error	{ parse_error2 
				    "unmatched subscript bracket" (rloc 2);
    	      	    	  	  raise Parse_error }
  | desig DOT name		{ exp (Select ($desig, $name)) }
  | desig actuals		{ exp (FuncCall ($desig, $actuals)) } ;

actuals :	
    LPAR RPAR			{ [] }
  | LPAR exprs RPAR		{ $exprs }
  | LPAR exprs %prec error	{ parse_error2 
				    "unmatched left parenthesis" (rloc 1);
				  raise Parse_error } ;

desigs :
    desig			{ [$desig] }
  | desig COMMA desigs		{ $desig :: $desigs } ;

exprs :	
    expr %prec error		{ [$expr] }
  | expr COMMA exprs		{ $expr :: $exprs } ;

qualid :	
    IDENT %prec DOT		{ makeName (anon, $IDENT, lloc ()) }
  | IDENT@x1 DOT IDENT@x2	{ makeName ($x1, $x2, lloc ()) };

name :	
    IDENT			{ makeName (anon, $IDENT, lloc ()) } ;

defids :
    defid %prec error		{ [$defid] }
  | defid COMMA defids		{ $defid :: $defids } ;

defid :
    IDENT			{ makeDefId ($IDENT, Private, rloc 1) }
  | IDENT MINUS			{ makeDefId ($IDENT, ReadOnly, rloc 1) }
  | IDENT STAR			{ makeDefId ($IDENT, Visible, rloc 1) } ;

semi :
    SEMI			{ () }
  | COMMA			{ parse_error "expected ';'" }
  | /* empty */ %prec error	{ missing_semi (lloc ()) } ;

/* Adding the alternative INFINITY makes the parser scan
   before reducing (doc --> eps), so that it picks up any docstring
   that precedes the following token. */

doc :
    /* empty */			{ let s = !Lexer.docstring in
				  Lexer.docstring := None; s } 
  | INFINITY			{ None } ;
