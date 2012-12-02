(*
 * yyparse.mli
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

type parse_tables =
  { yysemact : (unit -> Obj.t) array;	(* semantic actions, indexed by rule *)
    yyconst : int array;		(* ids for constant tokens *)
    yyblock : int array;		(* ids for tokens with arguments *)
    yylhs : int array;			(* lhs of each rule *)
    yyrlen : int array;			(* length of rhs for each rule *)
    yyaction : int array;		(* base of action row for each state *)
    yydefact : int array;		(* default action for each state *)
    yygoto : int array;			(* base of goto row for each lhs *)
    yydefgoto : int array;		(* default goto for each state *)
    yytabsize : int;			(* size of parser table *)
    yytable : int array;		(* packed action and goto rows *)
    yycheck : int array;		(* check table *)
    yyerror : string -> unit;		(* error handler *)
    yyname : string array;		(* name of each token *)
    yyrule : string array }		(* text of each rule *)

(* yyerr -- parse table entry for explicit error *)
val yyerr : int

exception Parse_error

exception YYexit of Obj.t

val yyparse : 
  parse_tables -> int -> (Lexing.lexbuf -> 'a) -> Lexing.lexbuf -> 'b

val yypeek : int -> 'a

val symbol_start : unit -> int
val symbol_end : unit -> int
val rhs_start : int -> int
val rhs_end : int -> int

val symbol_start_pos : unit -> Lexing.position
val symbol_end_pos : unit -> Lexing.position
val rhs_start_pos : int -> Lexing.position
val rhs_end_pos : int -> Lexing.position

val parse_error : string -> unit

val yyerrok : unit -> unit

val clear_parser : unit -> unit

val yydebug: bool ref
