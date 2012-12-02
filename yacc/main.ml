(*
 * main.ml
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

open Arg
open Print
open Grammar
open Lr0

let verbosity = ref 0
let stem = ref ""
let tflag = ref false
let sflag = ref false
  
let spec = 
  Arg.align
    [ "-v", Arg.Unit (fun () -> incr verbosity), " Produce report file";
      "-b", Arg.String (fun s -> stem := s), 
			"stem Specify stem for names of output files";
      "-a", Arg.Set Compile.aggressive, 
			" Aggressively follow default actions";
      "-s", Arg.Set sflag, " Use SLR method";
      "-t", Arg.Set tflag, " Show timings";
      "-d", Arg.Set Yyparse.yydebug, " Debug parser" ]

(* poly -- regexp to match polymorphic types *)
let poly = Str.regexp "\\(\\([A-Za-z_][A-Za-z0-9_']*\\)?[^A-Za-z_]\\)*'";;

(* is_polymorphic -- test if a type tag is polymorphic *)
let is_polymorphic t =
  Str.string_match poly t 0

let do_slr_lookahead () =
  do_states (fun q ->
      ItemSet.iter (fun z ->
	  let r = z.z_rule and k = z.z_index in
	  if k = r.r_len then
	    (* z is an item 'A --> omega .' that will yield a reduction *)
	    z.z_lookahead <- Grammar.follow r.r_lhs)
        q.p_items)

let main () =
  let fns = ref [] in
  Arg.parse spec (function s -> fns := !fns @ [s]) "Usage:";
  if List.length !fns <> 1 then begin
    Arg.usage spec "Usage:"; exit 2
  end;

  let t0 = Sys.time () in

  let in_file = List.hd !fns in
  Error.in_file := in_file;
  if !stem = "" then
    stem := Filename.basename (if Filename.check_suffix in_file ".mly" then
		  	Filename.chop_suffix in_file ".mly" else in_file);

  let chan = open_in in_file in
  let lexbuf = Lexing.from_channel chan in
  Lexer.init lexbuf;
  begin
    try Yacc.grammar Lexer.token lexbuf with
      Parsing.Parse_error -> 
	Error.syntax (Lexing.lexeme_start_p lexbuf) "syntax error" [];
	exit 1 
  end;

  (* Make rules '*entry* --> *i*, s' for each start symbol s *)
  let i = ref 0 in
  List.iter (fun s ->
      if s.x_kind <> Nonterm then 
	Error.error "start symbol '$' is a token" [fSym s]
      else if s.x_type = "" then
	Error.error "start symbol '$' has no specified type" [fSym s]
      else if is_polymorphic s.x_type then
	Error.error "start symbol '$' has a polymorphic type" [fSym s];
      incr i;
      let tok = Grammar.fix_token (sprintf "*$*" [fNum !i]) !i in
      ignore (make_rule entry_sym [tok; s] None 
	  (Lexing.dummy_pos, "") Lexing.dummy_pos))
    !Grammar.start_syms;

  (* Give up if errors were found in the grammar *)
  if !Error.status > 0 then exit !Error.status;
  
  if !verbosity > 0 then begin
    Report.start (!stem ^ ".output");
    Report.show_rules ()
  end;

  let t1 = Sys.time () in

  (* Assign a value to each symbol, and check for useless nonterminals *)
  Grammar.assign_values ();

  (* Find nullable nonterminals, etc. *)
  Grammar.preprocess ();

  (* Build the LR(0) automaton *)
  Lr0.compute_states ();

  let t2 = Sys.time () in

  (* Perform LALR(1) lookahead computation *)
  if !sflag then
    (do_slr_lookahead (); if !verbosity > 1 then Report.show_firsts ())
  else
    Lalr.do_lookahead ();

  let t3 = Sys.time () in

  (* Compute actions and gotos *)
  let actvec = Lr0.state_vector Table.empty_row in
  let defred = Lr0.state_vector 0 in
  let allgotos = ref [] in

  do_states (fun p ->
    let actions = Lr0.actions_for p in
    let report x a1 a2 =
      if !verbosity > 0 then Report.report_conflict p x a1 a2 in
    let actions' = 
      Conflict.resolve_conflicts report actions in

    (* Compute action vector and default reduction *)
    let (actlist, defr) = Compile.compile actions' in
    Vector.put actvec p (Table.make_row actlist); 
    Vector.put defred p defr;

    (* Accumulate all gotos into a list by nonterminal *)
    let gotos = Lr0.gotos_for p in
    allgotos := 
      List.map (fun (x, q) -> (x, (p.p_id, q.p_id))) gotos @ !allgotos;

    if !verbosity > 0 then 
      Report.show_state p actions' defr gotos (!verbosity > 1)
  );

  Conflict.report_conflicts ();

  let t4 = Sys.time () in

  if !verbosity > 0 then Report.close ();

  (* Check all rules are used at least once *)
  let useless = ref 0 in
  Grammar.do_rules 
    (fun r -> if r.r_id > 0 && not r.r_used then incr useless);
  if !useless > 0 then
    Error.warning "$ $ never reduced" 
      [fNum !useless; fStr (if !useless > 1 then "rules are" else "rule is")];

  (* Collect the gotos for each nonterminal *)
  let gotovec = Grammar.symbol_vector Table.empty_row in
  let defgoto = Grammar.symbol_vector 0 in
  List.iter (fun (x, gotos) -> 
      let d = Util.commonest compare (List.map snd gotos) in
      let entries = List.filter (fun (_, q) -> q <> d) gotos in
      Vector.put gotovec x (Table.make_row entries);
      Vector.put defgoto x d)
    (Util.group_sort compare_syms !allgotos);

  (* Pack rows into a compact table *)
  Table.pack_rows ();

  let t5 = Sys.time () in

  Output.make_def_file (!stem ^ ".mli");
  Output.make_imp_file (!stem ^ ".ml") actvec defred gotovec defgoto;

  let t6 = Sys.time () in

  if !tflag then begin
    printf "Parsing        $\n" [fFlo (t1 -. t0)];
    printf "LR(0) states   $\n" [fFlo (t2 -. t1)];
    printf "Look-aheads    $\n" [fFlo (t3 -. t2)];
    printf "Actions        $\n" [fFlo (t4 -. t3)];
    printf "Packing table  $\n" [fFlo (t5 -. t4)];
    printf "Output         $\n" [fFlo (t6 -. t5)]
  end;
  ()

let yacc = main ()
