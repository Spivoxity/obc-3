/*
 * yacc.mly
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

%token<Grammar.symbol>  SYMBOL
%token<string>          TAG NUMBER
%token<Lexing.position * string>  SEMACT
%token                  QUOTE 
%token                  TOKEN LEFT RIGHT NONASSOC START TYPE PREC
%token                  PPERCENT COLON SEMI VBAR DOT AT
%token                  BADTOK EOF UNREACHABLE

%start<unit>            grammar

%{
open Print
open Grammar

let prec = ref 0
let index = ref 0
let rhsmap = ref []

let dcl_type tag pos sym =
  if sym.x_type = "" then
    sym.x_type <- tag
  else if sym.x_type <> tag then
    Error.syntax pos 
      "'$' is declared with multiple types" [fSym sym]

let maybe_dcl_type tag pos sym =
  match tag with
      Some t -> dcl_type t pos sym
    | None -> ()

let dcl_token assoc tag pos sym =
  begin match assoc with
      Token ->
        sym.x_genuine <- true;
        if sym.x_kind = Nonterm then sym.x_kind <- Token
    | Left | Right | Nonassoc ->
        if sym.x_kind = Nonterm || sym.x_kind = Token then begin
          sym.x_kind <- assoc; sym.x_prec <- !prec
        end else
          Error.syntax pos
            "'$' is declared with multiple precedences" [fSym sym]
    | _ ->
        failwith "dcl_token"
  end;
  maybe_dcl_type tag pos sym

let dcl_start tag pos sym =
  if List.exists (fun x -> same_syms x sym) !start_syms then
    Error.syntax pos 
      "$ is declared as a start symbol more than once" [fSym sym];
  start_syms := !start_syms @ [sym];
  maybe_dcl_type tag pos sym

let pr_sym = ref None

let gencount = ref 0

let gensym () = 
  incr gencount; lookup (sprintf "yyg$" [fNum !gencount])

let fLeft n fmt =
  let f prf =
    let s0 = sprintf "$" [fmt] in
    let n0 = String.length s0 in
    prf "$" [fStr s0];
    for i = n0+1 to n  do prf "$" [fChr ' '] done in
  fExt f

let dollar_num = Str.regexp "\\$\\([0-9]+\\)"

let subst rhs pos s =
  let i = int_of_string (Str.matched_group 1 s) in
  if i > List.length rhs then
    Error.syntax pos
      "$$ is beyond the end of the rule" [fChr '$'; fNum i]
  else begin
    let x = List.nth rhs (i-1) in
    if not (has_value x) then 
      Error.syntax pos
	"$$ refers to $, which has no value" [fChr '$'; fNum i; fSym x]
  end;
  sprintf "_$" [fNum i]

let dollar_name = 
  Str.regexp "\\$\\([A-Za-z_][A-Za-z0-9_]*\\(\\.[0-9]+\\)?\\)"

let subst2 rhs pos s =
  let t = Str.matched_group 1 s in
  match List.filter (fun (x, i) -> x = t) !rhsmap with
      [] -> Error.syntax pos "$$ is not defined" [fChr '$'; fStr t]; ""
    | [(_, i)] -> 
	let x = List.nth rhs (i-1) in
	if not (has_value x) then
	  Error.syntax pos "$$ has no value" [fChr '$'; fStr t];
	sprintf "_$" [fLeft (String.length t) (fNum i)]
    | _ -> Error.syntax pos "$$ is ambiguous" [fChr '$'; fStr t]; ""

let make_action rhs (pos, text) =
  let text' = Str.global_substitute dollar_num (subst rhs pos) text in
  let text'' = Str.global_substitute dollar_name (subst2 rhs pos) text' in
  (pos, text'')
%}

%%

grammar :
    heading PPERCENT body       { () } ;

heading :
    /* EMPTY */                 { () }
  | heading assoc tag symbols   
      { List.iter (dcl_token $assoc $tag (rhs_start_pos 2)) $symbols }
  | heading START tag symbols   
      { List.iter (dcl_start $tag (rhs_start_pos 2)) $symbols }
  | heading TYPE TAG symbols    
      { List.iter (dcl_type $TAG (rhs_start_pos 2)) $symbols }
  | heading QUOTE               { () } ;

assoc :
    TOKEN                       { Token }
  | LEFT                        { incr prec; Left }
  | RIGHT                       { incr prec; Right }
  | NONASSOC                    { incr prec; Nonassoc } ;

tag :
    /* EMPTY */                 { None }
  | TAG                         { Some $TAG } ;

symbols :
    /* EMPTY */                 { [] }
  | SYMBOL symbols              { $SYMBOL::$symbols } ; 

body :
    /* EMPTY */                 { () }
  | body para                   { () } ;

para :
    SYMBOL COLON prods SEMI     
      { if not (is_nonterm $SYMBOL) then
	  Error.syntax (rhs_start_pos 1) "left-hand side is a token" [];
	if !Grammar.start_syms = [] then
	  Grammar.start_syms := [$SYMBOL];
	List.iter (fun (line, rhs, prec, semact) ->
	  ignore (create_rule $SYMBOL rhs prec semact line)) $prods } ;

prods :
    prod                        { [$1] }
  | prod VBAR prods             { $1::$3 } ;

prod :
    init rhs SEMACT             
      { let a = make_action $rhs $SEMACT in
	(rhs_start_pos 2, $rhs, !pr_sym, a) } ;

init :
    /* EMPTY */                 { pr_sym := None } ;

rhs :
    /* EMPTY */                 
      { index := 0; rhsmap := []; [] }
  | rhs symbol	                
      { let (x, y) = $symbol in
	if is_token x then pr_sym := Some x;
        if not (is_token x) || not (same_syms x error_sym) then
          x.x_genuine <- true;
	incr index; rhsmap := !rhsmap @ [(y, !index)];
        $rhs @ [x] }
  | rhs PREC SYMBOL                 
      { let x = $SYMBOL in
        if not (is_token x) then 
          Error.syntax (rhs_start_pos 2)
            "%prec must be followed by a token" [];
        if x.x_kind = Token then 
          Error.syntax (rhs_start_pos 2)
            "%prec must be followed by a token with defined precedence" [];
        pr_sym := Some x;
        $rhs }
  | rhs SEMACT                  
      { let g = gensym () in
	let a = make_action $rhs $SEMACT in
        let r = create_rule g [] None a (fst a) in
	incr index;
	r.r_context <- $rhs; 
	$rhs @ [g] } ;

symbol :
      SYMBOL@s			{ ($s, $s.x_name) }
    | SYMBOL@s DOT NUMBER@n	{ ($s, $s.x_name^"."^$n) } 
    | SYMBOL@s AT SYMBOL@x	{ ($s, $x.x_name) } ;
