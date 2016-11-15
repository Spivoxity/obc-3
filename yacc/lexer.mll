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
open Grammar
open Yacc
open Lexing
open Print

let init lexbuf =
  lexbuf.lex_curr_p <-
    { pos_fname = !Error.in_file; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 }

let newline lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- 
    { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }

let add_line buf lexbuf =
  let pos = lexeme_start_p lexbuf in
  Buffer.add_string buf
    (sprintf "\n# $ \"$\"\n" [fNum pos.pos_lnum; fStr !Error.in_file])

let level = ref 0
let section = ref 1

let aloc = ref dummy_pos
let atext = Buffer.create 512
}

rule token = parse
    ['A'-'Z''a'-'z']['A'-'Z''a'-'z''0'-'9''_']* as s
				{ SYMBOL (lookup s) }
  | ['0'-'9']+ as s		{ NUMBER s }
  | "%token"			{ TOKEN }
  | "%left"			{ LEFT }
  | "%right"			{ RIGHT }
  | "%nonassoc"			{ NONASSOC }
  | "%start"			{ START }
  | "%type"			{ TYPE }
  | "%prec"			{ PREC }
  | "%%"			{ incr section;
				  if !section = 2 then 
				    PPERCENT 
				  else begin
				    add_line Output.sec3_code lexbuf;
				    section3 lexbuf
				  end }
  | "."				{ DOT }				  
  | ":"				{ COLON }
  | "|"				{ VBAR }
  | ";"				{ SEMI }
  | "@"				{ AT }
  | "<"([^'>''\n']+ as s)">"	{ TAG s }
  | "%{"			{ add_line Output.user_code lexbuf;
  				  quote lexbuf }
  | "{"				{ Buffer.clear atext; level := 1;
				  aloc := lexeme_end_p lexbuf;
				  action lexbuf }
  | [' ''\t']			{ token lexbuf }
  | "\n"			{ newline lexbuf; token lexbuf }
  | "/*"			{ comment lexbuf }
  | _				{ BADTOK }
  | eof				{ EOF }

and comment = parse
    "*/"			{ token lexbuf }
  | "\n"			{ newline lexbuf; comment lexbuf }
  | _				{ comment lexbuf }
  | eof				{ Error.fatal "end of file in comment" [] }

and quote = parse
    "%}"			{ QUOTE }
  | "\n"			{ newline lexbuf; 
				  Buffer.add_char Output.user_code '\n';
				  quote lexbuf }
  | _				{ Buffer.add_char Output.user_code
				    (lexeme_char lexbuf 0); 
				  quote lexbuf }
  | eof				{ Error.fatal "unmatched '%{'" [] }

and section3 = parse
    "\n"			{ newline lexbuf;
				  Buffer.add_char Output.sec3_code '\n';
				  section3 lexbuf }
  | _				{ Buffer.add_char Output.sec3_code 
				    (lexeme_char lexbuf 0);
				  section3 lexbuf }
  | eof				{ EOF }

and action = parse
    "{"				{ incr level; Buffer.add_char atext '{'; 
				  action lexbuf }
  | "}"				{ decr level;
				  if !level = 0 then
				    let s = Buffer.contents atext in
				    SEMACT (!aloc, s)
				  else begin
				    Buffer.add_char atext '}'; action lexbuf 
				  end }
  | "\n"			{ newline lexbuf; 
				  Buffer.add_char atext '\n'; 
				  action lexbuf }
  | _				{ Buffer.add_char atext (lexeme_char lexbuf 0);
				  action lexbuf }
  | eof				{ Error.fatal "unmatched '{'" [] }

