/*
 * parser.mly
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
%token			ABSTRACT RETURN07 RECORD07 TRUE FALSE

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
  if yyerrstate () = 0 then
    syn_error "missing ';'" [] loc

let lloc () = (symbol_start (), symbol_end ())
let rloc n = (rhs_start n, rhs_end n)
let rend n = (rhs_end n, rhs_end n)

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

let expr e = makeExpr (e, lloc ())

let const (v, t) =
  let e = expr (Const v) in e.e_type <- t; e

let typexpr tx = makeTypexpr (tx, lloc ())

let has_proc ds = 
  List.exists (function ProcDecl _ -> true | _ -> false) ds

let sequ ss loc = makeStmt (Seq (List.rev ss), loc)
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
    blockid decls body END ident
      { check_end $blockid $ident (rloc 5);
        Block ($decls, $body, None, ref 0) } ;

pblock :
    blockid decls body ret END ident
      { check_end $blockid $ident (rloc 6);
        Block ($decls, $body, $ret, ref 0) } ;

blockid :
    /* EMPTY */			{ !end_name } ;

body :
    /* empty */                 { makeStmt (Skip, no_loc) }
  | BEGIN stmts		        { $stmts } ;  

/* The syntax of RETURN clauses is different in Oberon-07, so in 07
   mode we make the lexer return a different token RETURN07; the abstract
   syntax allows both RETURN statements and 07-style clauses, but only
   one is accessible from the concrete syntax in each mode. */

ret :
    /* empty */		        { None }
  | RETURN07 expr		{ Some $expr } ;

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
    /* empty */ %prec INFINITY	{ [] }
  | cdecl cdecls		{ $cdecl :: $cdecls } ;

cdecl :
    doc defid EQUAL expr semi	{ ConstDecl ($defid, $expr, $doc) } ;

tdecls :
    /* empty */ %prec INFINITY	{ [] }
  | tdecl tdecls		{ $tdecl :: $tdecls } ;

tdecl :	
    doc defid EQUAL texpr semi { ($defid, $texpr, $doc) } ;

vdecls :
    /* empty */ %prec INFINITY	{ [] }
  | vdecl vdecls		{ $vdecl :: $vdecls } ;

vdecl :
    doc defids COLON texpr semi { VarDecl (VarDef, $defids, $texpr, $doc) } ;

texpr :	
    tname			{ $tname }
  | LPAR defids RPAR		{ typexpr (Enum $defids) }
  | POINTER TO texpr		{ typexpr (Pointer $texpr) }
  | ARRAY OF texpr		{ typexpr (Flex $texpr) }
  | ARRAY exprs OF texpr
      { let array n t = typexpr (Array (n, t)) in
        List.fold_right array $exprs $texpr }
  | abstract RECORD parent fields END
      { typexpr (Record ($abstract, $parent, $fields)) }
  | abstract RECORD07 parent fields07 END
      { typexpr (Record ($abstract, $parent, $fields07)) }
  | PROCEDURE params		{ typexpr (Proc $params) } ;

tname :
    qualid			{ typexpr (TypeName $qualid) } ;

abstract :
    /* empty */			{ false }
  | ABSTRACT			{ true } ;

parent :
    /* empty */			{ None }
  | LPAR tname RPAR		{ Some $tname } ;

/* Wirth says

    fields = fdeclopt { ; fdeclopt }
    fdeclopt = [ idents : type ]

in Oberon, but changes to

    fields = [ fdecl { ; fdecl } ]
    fdecl = idents : type

in Oberon-07, so forbidding stray semicolons. The lexer returns either
RECORD or RECORD07 so as to select the correct productions here.
There's a special production to catch a common error in Oberon-07. */

fields :
    /* empty */                 { [] }
  | fdecl			{ [$fdecl] }
  | fields SEMI 		{ $fields }
  | fields SEMI fdecl		{ $fields @ [$fdecl] } ;
    
fields07 :
    /* empty */			{ [] }
  | fdecls			{ $fdecls } ;

fdecls :
    fdecl			{ [$fdecl] }
  | fdecls SEMI fdecl		{ $fdecls @ [$fdecl] }
  | fdecls SEMI
      { syn_error "Oberon-07 forbids a semicolon here" [] (rloc 2);
        $fdecls } ;

fdecl :	
    doc defids COLON texpr
      { VarDecl (FieldDef, $defids, $texpr, $doc) } ;

proc :
    doc PROCEDURE procid params semi pblock semi	
      { ProcDecl (Procedure, $procid, $params, $pblock, $doc) }
  | doc PROCEDURE rcvr procid params semi pblock semi
      { let (Heading (ps, r)) = $params in
        ProcDecl (Method, $procid, Heading ($rcvr::ps, r), $pblock, $doc) }
  | doc ABSTRACT PROCEDURE rcvr procid params semi
      { let (Heading (ps, r)) = $params in
        ProcDecl (AbsMeth, $procid, Heading ($rcvr::ps, r), NoBlock, $doc) }
  | doc PROCEDURE procid params IS STRING semi
      { PrimDecl ($procid, $params, $STRING, $doc) }
  | doc PROCEDURE error { end_name := anon } pblock semi
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
    stmts0      		{ sequ $stmts0 (rloc 1) }
  | stmts1			{ sequ $stmts1 (rloc 1) } ;

stmts0 :
    /* empty */			{ [] }
  | stmts0 SEMI			{ $stmts0 }
  | stmts1 SEMI			{ $stmts1 } 
  | stmts1 error SEMI		{ $stmts1 } ;

stmts1 :
    stmts0 stmt0		{ makeStmt ($stmt0, rloc 2) :: $stmts0 } 
  | stmts0 stmt1		{ makeStmt ($stmt1, rloc 2) :: $stmts0 } 
  | stmts1 stmt1	        { missing_semi (rend 1);
                                  makeStmt ($stmt1, rloc 3) :: $stmts1 } ;

stmt0 :
    desig ASSIGN expr		{ Assign ($desig, $expr) }
  | desig COMMA desigs ASSIGN expr COMMA exprs
      { if List.length $desigs = List.length $exprs then
	  SimAssign (List.combine ($desig::$desigs) ($expr::$exprs))
        else begin
          syn_error "Wrong number of expressions on RHS" [] (lloc ());
          Skip
        end }
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
      { ForStmt ($desig, $e1, $e2, $step, $stmts, ref None) } 
  | WITH branches else END	{ WithStmt ($branches, $else) }
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
    /* empty */			{ const (IntVal (integer 1), numtype) }
  | BY expr			{ $expr } ;

expr :
    simple %prec error		{ $simple }
  | simple@e1 RELOP simple@e2	{ expr (Binop ($RELOP, $e1, $e2)) }
  | simple@e1 EQUAL simple@e2	{ expr (Binop (Eq, $e1, $e2)) } 
  | simple IS qualid		{ expr (TypeTest ($simple, $qualid)) } ;

simple :
    term %prec error		{ $term }
  | PLUS term			{ expr (Monop (Uplus, $term)) }
  | MINUS term			{ expr (Monop (Uminus, $term)) }
  | simple PLUS term		{ expr (Binop (Plus, $simple, $term)) }
  | simple MINUS term		{ expr (Binop (Minus, $simple, $term)) }
  | simple ADDOP term		{ expr (Binop ($ADDOP, $simple, $term)) } ;

term :
    factor			{ $factor }
  | term MULOP factor		{ expr (Binop ($MULOP, $term, $factor)) }
  | term STAR factor		{ expr (Binop (Times, $term, $factor)) } ;

factor :
    NUMBER			{ const (IntVal $NUMBER, numtype) }
  | DECIMAL			{ expr (Decimal $DECIMAL) }
  | FLOCON			{ const (FloVal $FLOCON, realtype) }
  | DBLCON			{ const (FloVal $DBLCON, longreal) }
  | CHAR			{ const (IntVal (integer
					    (int_of_char $CHAR)), character) }
  | STRING			{ expr (String (save_string $STRING, 
					    String.length $STRING)) }
  | NIL				{ expr Nil }
  | TRUE                        { const (IntVal (integer 1), boolean) }
  | FALSE			{ const (IntVal (integer 0), boolean) }
  | desig %prec error		{ $desig }
  | LBRACE RBRACE		{ expr (Set []) }
  | LBRACE elements RBRACE	{ expr (Set $elements) }
  | NOT factor			{ expr (Monop (Not, $factor)) }
  | LPAR expr RPAR		{ $expr } 
  | LPAR expr %prec error	{ parse_error2 
				    "unmatched parenthesis" (rloc 1);
				  raise Parse_error } ;

desig :
    name			{ expr (Name $name) }
  | desig UPARROW		{ expr (Deref $desig) }
  | desig SUB exprs BUS		{ let sub a i = expr (Sub (a, i)) in
				  List.fold_left sub $desig $exprs }
  | desig SUB exprs %prec error	{ parse_error2 
				    "unmatched subscript bracket" (rloc 2);
    	      	    	  	  raise Parse_error }
  | desig DOT name		{ expr (Select ($desig, $name)) }
    /* We can't always tell the difference between function calls
       and casts without knowing which identifiers are type names.
       So everything is treated as a function call by the parser,
       and the semantic analyser must sort things out.  Sometimes
       it says "function call not allowed here". */
  | desig actuals		{ expr (FuncCall ($desig, $actuals)) } ;

actuals :	
    LPAR RPAR			{ [] }
  | LPAR exprs RPAR		{ $exprs }
  | LPAR exprs %prec error	{ parse_error2 
				    "unmatched parenthesis" (rloc 1);
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
    IDENT export		{ makeDefId ($IDENT, $export, rloc 1) } ;

export :
    /* empty */                 { Private }
  | STAR			{ Visible }
  | MINUS			
      { if !Config.ob07flag then
          syn_error "'-' is not allowed as an export mark in Oberon-07"
            [] (rloc 1);
        ReadOnly } ;

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
