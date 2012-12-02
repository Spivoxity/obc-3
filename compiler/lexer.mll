(*
 * lexer.mll
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

{
open Symtab
open Parser
open Error
open Lexing
open Eval

let idtable = Hashtbl.create 1009

let init () = 
  let add (s, v) = 
    Hashtbl.add idtable 
      (if !Config.lcflag then String.lowercase s else s) v in
  List.iter add
    [ "ARRAY", ARRAY; "BEGIN", BEGIN; "BY", BY; "CASE", CASE; "CONST", CONST;
      "DIV", MULOP Div; "DO", DO; "ELSE", ELSE; "ELSIF", ELSIF; 
      "END", END; "EXIT", EXIT; "FOR", FOR; "IF", IF ; "IMPORT", IMPORT; 
      "IS", IS; "LOOP", LOOP; "MOD", MULOP Mod; "MODULE", MODULE; "NIL", NIL; 
      "OF", OF; "OR", ADDOP Or; "POINTER", POINTER; "PROCEDURE", PROCEDURE; 
      "RECORD", RECORD; "REPEAT", REPEAT; "RETURN", RETURN; "THEN", THEN; 
      "TO", TO; "TYPE", TYPE; "UNTIL", UNTIL; "VAR", VAR; 
      "WHILE", WHILE; "WITH", WITH; "IN", RELOP In ];
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
  let i = ref 0 and v = ref (integer 0) in
  begin try 
      while true do 
	let d = String.index hex s.[!i] in 
	v := integer_add (integer_mul !v (integer 16)) (integer d); 
	incr i 
      done 
  with Not_found -> ()
  end;
  !v

let location lexbuf =
 (lexeme_start lexbuf, lexeme_end lexbuf)

let clevel = ref 0
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

let lex_error fmt args = syn_error fmt args (here ())
}

rule token = parse
    ['A'-'Z''a'-'z']['A'-'Z''a'-'z''0'-'9']*
			{ lookup (lexeme lexbuf) }
  | ['0'-'9']+		{ DECIMAL (lexeme lexbuf) }
  | ['0'-'9']+'.'['0'-'9']*('E'['+''-']?['0'-'9']+)?
			{ FLOCON (float_of_string (lexeme lexbuf)) }
  | ['0'-'9']+'.'['0'-'9']*'D'['+''-']?['0'-'9']+
			{ let s = lexeme lexbuf in
			  s.[String.index s 'D'] <- 'E';
			  DBLCON (float_of_string s) }
  | ['0'-'9']+".."	{ unget 2 lexbuf; 
			  DECIMAL (lexeme lexbuf) }
  | "'"[^'\'''\n']"'"|'"'[^'"''\n']'"'
			{ CHAR (lexeme_char lexbuf 1) }
  | "'"[^'\'''\n']*"'"|'"'[^'"''\n']*'"' 
			{ let s = lexeme lexbuf in
			  STRING (String.sub s 1 (String.length s - 2)) }
  | "'"[^'\'''\n']*'\n'|'"'[^'"''\n']*'\n'
		        { unget 1 lexbuf;
			  lex_error "unterminated string constant" [];
			  raise Yyparse.Parse_error }
  | ['0'-'9']['0'-'9''A'-'F']*'X' 
			{ let n = hexval (lexeme lexbuf) in
			  if n < integer 256 then
			    CHAR (char_of_integer n)
			  else begin
			    lex_error "character constant is too large" [];
			    CHAR '\000'
			  end }
  | ['0'-'9']['0'-'9''A'-'F']*'H' 
			{ NUMBER (hexval (lexeme lexbuf)) }
  | ";"			{ SEMI }
  | "."			{ DOT }
  | ":"			{ COLON }
  | "("			{ LPAR }
  | ")"			{ RPAR }
  | ","			{ COMMA }
  | "["			{ SUB }
  | "]"			{ BUS }
  | "{"			{ LBRACE }
  | "}"			{ RBRACE }
  | "="			{ EQUAL }
  | "/"			{ MULOP Over }
  | "+"			{ PLUS }
  | "-"			{ MINUS }
  | "*"			{ STAR }
  | "^"			{ UPARROW }
  | "~"			{ NOT }
  | "&"			{ MULOP And }
  | "|"			{ VBAR }
  | ".."		{ DOTDOT }
  | "<"			{ RELOP Lt }
  | ">"			{ RELOP Gt }
  | "#"			{ RELOP Neq }
  | ("<>"|"!=")		{ lex_error "please use # for inequality" []; 
			  RELOP Neq }
  | "/="		{ lex_error ("I see that you are a Haskell programmer"
					^ " at heart; try # instead") [];
			  RELOP Neq }
  | "<="		{ RELOP Leq }
  | ">="		{ RELOP Geq }
  | ":="		{ ASSIGN }
  | [' ''\t']+		{ token lexbuf }
  | "(*"		{ in_docstring := false; cstart := location lexbuf;
			  incr clevel; comment lexbuf; token lexbuf }
  | "(**"		{ in_docstring := true; cstart := location lexbuf;
			  incr clevel; comment lexbuf; token lexbuf }
  | '\r'		{ token lexbuf }
  | '\n'		{ inc_line lexbuf; token lexbuf }
  | "#line"[^'\n']*'\n'	{ set_line lexbuf; token lexbuf }
  | _			{ BADTOK }
  | eof			{ lex_error "unexpected end of file" []; exit 1 }

and comment = parse
    "(*"		{ incr clevel; comment lexbuf }
  | "*)"		{ decr clevel; 
			  if !clevel > 0 then 
			    comment lexbuf
			  else if !in_docstring then
			    docstring := 
			      Some (join_locs !cstart (location lexbuf)) }
  | "\n"		{ inc_line lexbuf; comment lexbuf }
  | _			{ comment lexbuf }
  | eof			{ Error.set_loc !cstart;
			  lex_error "unterminated comment" []; exit 1 }
