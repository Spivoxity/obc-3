(*
 * yyparse.ml
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

open Lexing
open Printf

type parse_tables =
  { yysemact : (unit -> Obj.t) array;
    yyconst : int array;
    yyblock : int array;
    yylhs : int array;
    yyrlen : int array;
    yyaction : int array;
    yydefact : int array;
    yygoto : int array;
    yydefgoto : int array;
    yytabsize : int;
    yytable : int array;
    yycheck : int array;
    yyerror : string -> unit;
    yyname : string array;
    yyrule : string array }

exception Parse_error
exception YYexit of Obj.t

let yyerr = min_int
let dummy = Obj.repr ()
let errtok = -1

let yydebug = ref false
let stacksize = ref 100

let s_stack = ref (Array.make !stacksize 0)
  (* Stack of parser states *)	
and v_stack = ref (Array.make !stacksize (Obj.repr 0))
  (* Stack of semantic values *)	
and l_stack = ref (Array.make !stacksize dummy_pos)
  (* Stack of starting positions *)	
and r_stack = ref (Array.make !stacksize dummy_pos)
  (* Stack of ending positions *)	

let sp = ref (-1)			(* Stack pointer *)
and rhs_len = ref 0			(* Length of rhs during action *)
and errstate = ref 0			(* Count tokens to shift after error *)

(* double -- double the size of a stack *)
let double v x0 =
  let n = Array.length !v in
  let new_v = Array.make (2*n) x0 in
  Array.blit !v 0 new_v 0 n; v := new_v

(* yyparse -- parser engine *)
let yyparse tables start lexfun lexbuf =
  let init_sp = !sp			(* Make engine reentrant *)
  and init_rhs_len = !rhs_len
  and init_errstate = !errstate in

  let token = ref start			(* Current token *)
  and tokstr = ref ""			(* Text of token for debugging *)
  and lval = ref dummy			(* Semantic value of token *)
  and lpos = ref dummy_pos		(* Start of token *)
  and rpos = ref dummy_pos in		(* End of token *)

  (* debug -- print debugging line *)
  let debug fmt =
    printf "> state %d" !s_stack.(!sp);
      if !token >= 0 then
	printf ", token %s '%s'" 
	  tables.yyname.(!token) (String.escaped !tokstr);
      printf ": "; printf fmt in

  (* scan -- fetch and decode next token *)
  let scan () =
    if !yydebug then debug "scan\n";
    let t = Obj.repr (lexfun lexbuf) in
    if Obj.is_block t then begin
      token := tables.yyblock.(Obj.tag t);
      lval := Obj.field t 0
    end else begin
      token := tables.yyconst.(Obj.obj t);
      lval := dummy
    end;
    tokstr := Lexing.lexeme lexbuf;
    lpos := Lexing.lexeme_start_p lexbuf;
    rpos := Lexing.lexeme_end_p lexbuf in

  (* push -- push a state with value and positions *)
  let push s v l r =
    incr sp;
    if !sp = !stacksize then begin
      double s_stack 0; double v_stack (Obj.repr 0);
      double l_stack dummy_pos; double r_stack dummy_pos;
      stacksize := 2 * !stacksize
    end;
    !s_stack.(!sp) <- s; !v_stack.(!sp) <- v;
    !l_stack.(!sp) <- l; !r_stack.(!sp) <- r in

  (* unpack -- retrieve entry from packed table *)
  let unpack tbl def x y =
    let base = tbl.(x) in
    if base <> 0 && base+y >= 0 && base+y < tables.yytabsize 
	&& tables.yycheck.(base+y) = y then
      tables.yytable.(base+y)
    else
      def.(x) in

  (* recover -- error recovery *)
  let rec recover () =
    if !errstate = 3 then begin
      (* No shift after recovery: discard a token *)
      if !token = 0 then 
	raise Parse_error;		(* can't discard EOF *) 
      if !yydebug then debug "discard\n";
      token := -1;
    end
    else begin
      (* A fresh error: pop until errtok can be shifted *)
      if !sp = init_sp then raise Parse_error;
      let st = !s_stack.(!sp) in
      let newst = 
	unpack tables.yyaction tables.yydefact st errtok in
      if newst > 0 then begin
        let pos = !r_stack.(!sp) in
	if !yydebug then debug "recover %d\n" newst;
	push newst dummy pos pos;
	errstate := 3
      end
      else begin
	if !yydebug then debug "pop\n";
	decr sp; recover ()
      end
    end in

  try
    errstate := 0;
    push 0 dummy dummy_pos dummy_pos;

    while true do
      let st0 = !s_stack.(!sp) in

      (* Determine the next action *)

      (* By default, the parser generator presently generates parse
         tables with the byacc semantics -- where a state cannot have
         both an action vector and a default reduction -- but there's
         also a switch that uses the more liberal bison semantics,
         where both can exist.  This code supports both, with an
         engine that refrains from scanning if there is no action
         vector. *)
      let action =
	if tables.yyaction.(st0) = 0 then
	  (* Just a default reduction *)
	  tables.yydefact.(st0)
	else begin
	  (* Found an action table, so need lookahead token *)
	  if !token < 0 then scan ();
	  unpack tables.yyaction tables.yydefact st0 !token
	end in

      (* Perform the action *)
      if action > 0 then begin
        (* Shift and go to state action *)
	if !yydebug then debug "shift %d\n" action;
	push action !lval !lpos !rpos;
	token := -1;
	if !errstate > 0 then decr errstate
      end
      else if action = 0 || action = yyerr then begin
	(* Signal an error and try to recover *)
	if !yydebug then debug "error\n";
        if !errstate = 0 then tables.yyerror "syntax error";
        recover ()
      end
      else begin
        (* Reduce by rule -action *)
	let rule = -action in
	if !yydebug then 
	  debug "reduce %d (%s)\n" rule tables.yyrule.(rule);

	try 
	  (* For internal actions, yyrlen contains - (length of context) *)

	  (* Perform semantic action *)
	  rhs_len := abs tables.yyrlen.(rule);
	  let sval = tables.yysemact.(rule) () in

	  (* Pop the rhs *)
	  let rlen = max tables.yyrlen.(rule) 0 in
	  sp := !sp - rlen; 
	  let st1 = !s_stack.(!sp) in

	  (* Perform the state transition *)
	  let lhs = tables.yylhs.(rule) in
	  let st2 = unpack tables.yygoto tables.yydefgoto lhs st1 in
	  if !yydebug then debug "goto %d\n" st2;

	  (* Push the new state *)
	  let r = !r_stack.(!sp+rlen) in
	  let l = if !rhs_len = 0 then r else !l_stack.(!sp+1) in
	  push st2 sval l r
	with
	  Parse_error -> recover ()
      end
    done;
    Obj.magic ()
  with ex ->
    sp := init_sp;
    errstate := init_errstate;
    rhs_len := init_rhs_len;
    match ex with
	YYexit v -> 
	  (* Semantic action signalled to accept *)
	  Obj.obj v
      | _ ->
	  raise ex

let yypeek n = Obj.obj !v_stack.(!sp - !rhs_len + n)

let symbol_start_pos () =
  if !rhs_len > 0 then 
    !l_stack.(!sp - !rhs_len + 1) 
  else 
    !r_stack.(!sp - !rhs_len)

let symbol_end_pos () = !r_stack.(!sp)

let rhs_start_pos n = !l_stack.(!sp - !rhs_len + n)
let rhs_end_pos n = !r_stack.(!sp - !rhs_len + n)

let symbol_start () =
  let p = symbol_start_pos () in p.pos_cnum
let symbol_end () =
  let p = symbol_end_pos () in p.pos_cnum
let rhs_start n =
  let p = rhs_start_pos n in p.pos_cnum
let rhs_end n =
  let p = rhs_end_pos n in p.pos_cnum

let parse_error s = ()

let yyerrok () = errstate := 0

let yyerrstate () = !errstate

let clear_parser () =
  Array.fill !v_stack 0 !stacksize dummy
