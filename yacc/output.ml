(*
 * output.ml
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

open Print
open Grammar
open Lr0
open Lexing

(* Text output with counted lines *)

let out_name = ref ""
let out_file = ref stdout
let out_line = ref 0

let open_out_file name =
  out_name := name;
  out_line := 1;
  try
    out_file := open_out name;
  with Sys_error _ ->
    Error.fatal "can't write $" [fStr name]

let close_out_file () =
  close_out !out_file

let xputc ch =
  output_char !out_file ch;
  if ch = '\n' then incr out_line

let xputbuf buf =
  for i = 0 to Buffer.length buf - 1 do
    xputc (Buffer.nth buf i)
  done

let xprintf fmt args = Print.do_print xputc fmt args

let sync_line () =
  xprintf "# $ \"$\"\n" [fNum (!out_line+1); fStr !out_name]


(* Formatting utilities *)

(* fill_print -- filled output lines *)
let fill_print fmt a =
  let limit = 60 in
  let pos = ref 0 in
  for i = 0 to Array.length a - 1 do
    if !pos >= limit then begin xprintf "\n" []; pos := 0 end;
    let s = sprintf "$" [fmt a.(i)] in
    xprintf "$" [fStr s]; pos := !pos + String.length s
  done;
  xprintf "\n" []

let chunk = 10

(* A table with entries given by a function *)
let put_table name do_things valfun =
  xprintf "let $ = [|" [fStr name];
  let k = ref 0 in
  do_things (fun i ->
    if !k mod chunk = 0 then xprintf "\n" [];
    let v = valfun i in 
    if v = Compile.yyerr then
      xprintf " yyerr;" []
    else
      xprintf "$;" [fFix (v, 6)];
    incr k);
  xprintf "\n|]\n\n" []

let put_vect name v =
  put_table name 
    (fun put -> for i = 0 to Growvect.size v - 1 do put i done) 
    (Growvect.get v)

let put_nt_table name f =
  let tab = Growvect.make !num_nts 0 in
  do_nonterms 
    (fun x -> if x.x_value >= 0 then Growvect.set tab x.x_value (f x));
  put_vect name tab

let put_rule_table name f =
  put_table name do_rules f

let put_state_table name f = 
  put_table name do_states f


(* Parts of the output file *)

(* Definition of the 'token' type *)
let put_token_type () =
  xprintf "type token =\n" [];
  do_syms (fun x ->
    if is_token x && x.x_genuine then begin
      if x.x_type = "" then
	xprintf "  | $\n" [fSym x]
      else
	xprintf "  | $ of ($)\n" [fSym x; fStr x.x_type]
    end);
  xprintf "\n" []

(* Translation tables from OCaml constructors to token numbers *)
let put_transl name test =
  xprintf "let $ = [|\n" [fStr name];
  do_syms (fun x ->
    if is_token x && x.x_genuine && test x then
      xprintf "  $; (* $ *)\n" [fNum x.x_value; fSym x]);
  xprintf "|]\n\n" []


(* Action code *)

let fTag x =
  if x.x_type <> "" then 
    fStr x.x_type 
  else 
    fMeta "'t_$" [fSym x]

(* put_semact -- output one semantic action *)
let put_semact r =
  if r.r_id = 0 then
    (* The dummy rule 0 -- never reduced *)
    xprintf "(fun () -> failwith \"parser\");\n" []
  else if same_syms r.r_lhs entry_sym then begin
    (* The dummy rules for each entry point *)
    xprintf "(* Entry $ *)\n" [fSym (List.nth r.r_rhs 1)];
    xprintf "(fun () -> raise (YYexit (yypeek 2)));\n" []
  end
  else begin
    (* An ordinary rule: we use r_context in place of r_rhs here,
       in case the rule was generated from an embedded action *)
    let n = List.length r.r_context in
    let (pos, code) = r.r_semact in
    xprintf "(fun () ->\n" [];
    for i = 1 to n do
      let x = List.nth r.r_context (i-1) in
      if has_value x then
	xprintf "  let _$ = (yypeek $ : $) in\n" [fNum i; fNum i; fTag x]
    done;
    xprintf "  Obj.repr (\n" [];
    xprintf "# $ \"$\"\n" [fNum pos.pos_lnum; fStr pos.pos_fname];
    for i = 0 to pos.pos_cnum - pos.pos_bol - 2 do xputc ' ' done;
    xprintf "($)\n" [fStr code];
    sync_line ();
    xprintf "\t\t: $));\n" [fTag r.r_lhs]
  end


(* Putting it all together *)

let user_code = Buffer.create 1024
let sec3_code = Buffer.create 1024

(* The .ml file *)
let put_tables actvec defred gotovec defgoto = 
  (* Translation tables *)
  put_transl "yyconst" (fun x -> x.x_type = "");
  put_transl "yyblock" (fun x -> x.x_type <> "");

  (* Number of the LHS nonterminal in each rule *)
  put_rule_table "yylhs" (fun r -> r.r_lhs.x_value);

  (* Length of the RHS of each rule *)
  put_rule_table "yyrlen" 
    (fun r -> if r.r_len = 0 then - List.length r.r_context else r.r_len);

  (* Pointers to row of actions for each state (by lookahead token) *)
  put_state_table "yyaction" (fun p -> Table.start (Vector.get actvec p));
      
  (* Default reduction in each state *)
  put_state_table "yydefact" (fun p -> - Vector.get defred p);

  (* Pointers to row of gotos for each nonterminal (by state) *)
  put_nt_table "yygoto" (fun x -> Table.start (Vector.get gotovec x));

  (* Default goto state for each nonterminal *)
  put_nt_table "yydefgoto" (Vector.get defgoto);
      
  (* The big table of rows and its check table *)
  let size = Growvect.size Table.table in
  xprintf "let yytabsize = $\n\n" [fNum size];
  put_vect "yytable" Table.table;
  put_vect "yycheck" Table.check;

  (* Names of the tokens *)
  let tok_name = Array.make !num_toks "" in
  do_syms (fun x -> 
      if is_token x && x.x_value >= 0 then 
	tok_name.(x.x_value) <- x.x_name);
  xprintf "let yyname = [|\n" [];
  fill_print (fun s -> fMeta "\"$\"; " [fStr s]) tok_name;
  xprintf "|]\n\n" [];

  (* Text of each production *)
  xprintf "let yyrule = [|\n" [];
  do_rules (fun r ->
    xprintf "\"$ -->"	[fSym r.r_lhs]; 
    if List.length r.r_rhs = 0 then 
      xprintf " []" []
    else
      List.iter (fun x -> xprintf " $" [fSym x]) r.r_rhs;
    xprintf "\";\n" []);
  xprintf "|]\n\n" [];

  (* Semantic actions *)
  xprintf "let yysemact = [|\n" [];
  do_rules put_semact;
  xprintf "|]\n\n" [];

  (* Parse table structure *)
  List.iter (fun s -> xprintf "$\n" [fStr s])
    [ "let yytables =";
      "  { yysemact = yysemact;";
      "    yyconst = yyconst;";
      "    yyblock = yyblock;";
      "    yylhs = yylhs;";
      "    yyrlen = yyrlen;";
      "    yyaction = yyaction;";
      "	   yydefact = yydefact;";
      "    yygoto = yygoto;";
      "    yydefgoto = yydefgoto;";
      "    yytabsize = yytabsize;";
      "    yytable = yytable;";
      "    yycheck = yycheck;";
      "    yyerror = parse_error;";
      "    yyname = yyname;";
      "    yyrule = yyrule }";
      "" ]

(* Main functions for each entry point *)
let put_funcs () =
  let i = ref 0 in
  List.iter (fun x ->
      incr i;
      xprintf 
	"let $ (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =\n"
	[fSym x];
      xprintf 
	"  (yyparse yytables $ lexfun lexbuf : $)\n\n" 
	[fNum !i; fStr x.x_type])
    !start_syms

(* Declarations for the .mli file *)
let put_public () =
  List.iter (fun x ->
      xprintf "val $ :\n" [fSym x];
      xprintf "  (Lexing.lexbuf -> token) -> Lexing.lexbuf -> $\n\n" 
	[fStr x.x_type])
    !start_syms

let make_def_file name =
  open_out_file name;
  xprintf "(* Definitions generated by Mike's special yacc *)\n\n" [];
  put_token_type ();
  put_public ();
  close_out_file ()

let make_imp_file name actvec defred gotovec defgoto =
  open_out_file name;
  xprintf "(* Parser generated by Mike's special yacc *)\n\n" [];
  xprintf "open Yyparse;;\n\n" [];
  put_token_type ();
  xputbuf user_code; xprintf "\n" [];
  sync_line ();
  put_tables actvec defred gotovec defgoto;
  put_funcs ();
  xputbuf sec3_code; xprintf "\n" [];
  close_out_file ();

