(*
 * lexer.mll
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

{
open Symtab
open Parser
open Error
open Lexing
open Eval

let lex_error fmt args = syn_error fmt args (here ())

let idtable = Hashtbl.create 1009

let init () = 
  let add (s, v) = 
    Hashtbl.add idtable 
      (if !Config.lcflag then Util.strlower s else s) v in
  List.iter add
    [ "ARRAY", ARRAY; "BEGIN", BEGIN; "BY", BY; "CASE", CASE; "CONST", CONST;
      "DIV", MULOP Div; "DO", DO; "ELSE", ELSE; "ELSIF", ELSIF; 
      "END", END; "FOR", FOR; "IF", IF ; "IMPORT", IMPORT; 
      "IS", IS; "MOD", MULOP Mod; "MODULE", MODULE; "NIL", NIL; 
      "OF", OF; "OR", ADDOP Or; "POINTER", POINTER; "PROCEDURE", PROCEDURE; 
      "RECORD", RECORD; "REPEAT", REPEAT; "THEN", THEN; 
      "TO", TO; "TYPE", TYPE; "UNTIL", UNTIL; "VAR", VAR; 
      "WHILE", WHILE; "IN", RELOP In ];

  (* Ordinary Oberon-2 *)
  if not !Config.ob07flag then
    List.iter add [ "RETURN", RETURN; "WITH", WITH; "RECORD", RECORD ];

  (* Oberon-07 *)
  if !Config.ob07flag then
    List.iter add
      [ "RETURN", RETURN07; "RECORD", RECORD07; "TRUE", TRUE; "FALSE", FALSE ];

  (* Extensions to Oberon-07 *)
  if not !Config.ob07flag || !Config.extensions then
    List.iter add [ "LOOP", LOOP; "EXIT", EXIT ];

  (* Extensions to all dialects *)
  if !Config.extensions then
    List.iter add [ "ABSTRACT", ABSTRACT ]

let lookup s =
  try Hashtbl.find idtable s with
    Not_found -> 
      let t = IDENT (intern s) in
      Hashtbl.add idtable s t; 
      t

(* hexval -- convert a hex number *)
let hexval s =
  let hex = "0123456789ABCDEF" in
  let v = ref (integer 0) in
  for i = 0 to String.length s - 1 do
    let d = String.index hex s.[i] in 
    v := integer_add (integer_mul !v (integer 16)) (integer d); 
  done;
  !v

let char_val n =
  if n < integer 256 then 
    char_of_integer n 
  else begin
    lex_error "character constant is too large" []; 
    '\000'
  end

let location lexbuf =
 (lexeme_start lexbuf, lexeme_end lexbuf)

let cstart = ref (0, 0)
let in_docstring = ref false

let docstring = ref None

let inc_line lexbuf = 
  Error.note_line (lexeme_end lexbuf)

let set_line lexbuf = 
  let words = Util.split_string (lexeme lexbuf) in
  if List.length words >= 3 && String.length (List.nth words 2) >= 2 then
    Error.set_line (int_of_string (List.nth words 1)) 
      (String.sub (List.nth words 2) 1 (String.length (List.nth words 2) - 2));
  Error.note_line (lexeme_end lexbuf)

let unget n lexbuf =
  lexbuf.lex_curr_pos <- lexbuf.lex_curr_pos - n;
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with 
    pos_cnum = lexbuf.lex_abs_pos + lexbuf.lex_curr_pos }
}

let letter = ['A'-'Z''a'-'z']
let digit = ['0'-'9']
let hexdigit = ['0'-'9''A'-'F']
let q = '\''
let qq = '"'
let notq = [^'\'''\n']
let notqq = [^'"''\n']

rule token = parse
    letter (letter | digit)* as s	{ lookup s }
  | digit+ as s				{ DECIMAL s }
  | digit+ '.' digit* ('E'['+''-']?digit+)? as s
					{ FLOCON (float_of_string s) }
  | (digit+ '.' digit* as s1) 'D' (['+''-']?digit+ as s2)
				        { DBLCON (float_of_string (s1^"E"^s2)) }
  | (digit+ as s) ".."			{ unget 2 lexbuf; DECIMAL s }
  | q (notq as c) q 
      | qq (notqq as c) qq  		{ CHAR c }
  | q (notq* as s) q 
      | qq (notqq* as s) qq  		{ STRING s }
  | q notq* '\n' | qq notqq* '\n'
      { unget 1 lexbuf; 
	lex_error "unterminated string constant" [];
	raise Yyparse.Parse_error }
  | (digit hexdigit* as s)'X' 		{ CHAR (char_val (hexval s)) }
  | (digit hexdigit* as s)'H'		{ NUMBER (hexval s) }
  | ";"					{ SEMI }
  | "."					{ DOT }
  | ":"					{ COLON }
  | "("					{ LPAR }
  | ")"					{ RPAR }
  | ","					{ COMMA }
  | "["					{ SUB }
  | "]"					{ BUS }
  | "{"					{ LBRACE }
  | "}"					{ RBRACE }
  | "="					{ EQUAL }
  | "/"					{ MULOP Over }
  | "+"					{ PLUS }
  | "-"					{ MINUS }
  | "*"					{ STAR }
  | "^"					{ UPARROW }
  | "~"					{ NOT }
  | "&"					{ MULOP And }
  | "|"					{ VBAR }
  | ".."				{ DOTDOT }
  | "<"					{ RELOP Lt }
  | ">"					{ RELOP Gt }
  | "#"					{ RELOP Neq }
  | "<>" | "!=" | "/="
      { lex_error "please use # for inequality" []; RELOP Neq }
  | "<="				{ RELOP Leq }
  | ">="				{ RELOP Geq }
  | ":="				{ ASSIGN }
  | [' ''\t']+				{ token lexbuf }
  | "(*"		
      { in_docstring := false; cstart := location lexbuf;
	comment 1 lexbuf; token lexbuf }
  | "(**"		
      { in_docstring := true; cstart := location lexbuf;
	comment 1 lexbuf; token lexbuf }
  | "(**)"				{ token lexbuf }
  | '\r'				{ token lexbuf }
  | '\n'				{ inc_line lexbuf; token lexbuf }
  | "#line"[^'\n']*'\n'			{ set_line lexbuf; token lexbuf }
  | _					{ BADTOK }
  | eof			
      { lex_error "unexpected end of file" []; exit 1 }

and comment n = parse
    "(*"				
      { comment (n+1) lexbuf }
  | "*)"				
      { if n > 1 then 
	  comment (n-1) lexbuf
	else if !in_docstring then
	  docstring := Some (join_locs !cstart (location lexbuf)) }
  | "\n"		
      { inc_line lexbuf; comment n lexbuf }
  | _			
      { comment n lexbuf }
  | eof			
      { Error.set_loc !cstart;
	lex_error "unterminated comment" []; exit 1 }
