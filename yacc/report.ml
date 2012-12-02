(*
 * report.ml
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

open Grammar
open Lr0
open Print

(* Diagnostic output *)

let log_file = ref stdout

let lprintf fmt args =
  fprintf !log_file fmt args

let start name =
  begin try 
    log_file := open_out name 
  with Sys_error _ ->
    Error.fatal "can't write $" [fStr name]
  end;

  lprintf "File '$'\n\n" [fStr name]

let show_rules () =
  lprintf "Rules:\n" [];
  do_rules (fun r -> lprintf "$:  $\n" [fFixNum (r.r_id, 4); fRule r]);
  lprintf "\n" [];
  do_nonterms (fun x ->			(* Undefined nonterminals *)
    if x.x_rules = [] then
      lprintf "Nonterminal '$' has no rules\n" [fSym x]);
  lprintf "\n" []

let show_firsts () =
  do_nonterms (fun x -> 
    lprintf "First($) = $\n" [fSym x; fSymSet (first x)]);
  lprintf "\n" [];
  do_nonterms (fun x -> 
    lprintf "Follow($) = $\n" [fSym x; fSymSet (follow x)]);
  lprintf "\n" []

let report_conflict p x a1 a2 =
  match (a1, a2) with
      (Shift q, Reduce r) ->
	  lprintf 
	    "State $: shift/reduce conflict (shift $, reduce $) on $\n"
	    [fNum p.p_id; fNum q.p_id; fNum r.r_id; fSym x]
    | (Reduce r1, Reduce r2) ->
	  lprintf
	    "State $: reduce/reduce conflict (reduce $, reduce $) on $\n"
	    [fNum p.p_id; fNum r1.r_id; fNum r2.r_id; fSym x]
    | (_, _) -> failwith "report_conflict"

let show_state p actions defred gotos show_la =
  lprintf "\nState $\n\n" [fNum p.p_id];
  (* Print items: just do core items (starting rule or dot not at the start),
     null items (rhs is empty) and rules for the root *)
  ItemSet.iter 
    (fun z ->
      let r = z.z_rule and k = z.z_index in
      if r.r_len = 0 || k > 0 || same_syms r.r_lhs root_sym then begin
	lprintf "\t$" [fItem z];
	if show_la && k = r.r_len then
	  lprintf "  $" [fSymSet z.z_lookahead];
	lprintf "\n" []
      end)
    p.p_items;
  lprintf "\n" [];

  (* Print parser actions *)
  let is_default = 
    function Reduce r -> r.r_id = defred | _ -> false in
  List.iter
    (fun (x, a) -> 
      if not (is_default a) then lprintf "\t$  $\n" [fSym x; fAction a]) 
    actions;
  if defred <> 0 then 
    lprintf "\t.  reduce $\n" [fNum defred]; 
  lprintf "\n" [];

  (* Print gotos *)
  if gotos <> [] then begin
    List.iter
      (fun (x, q) -> lprintf "\t$  goto $\n" [fSym x; fNum q.p_id])
      gotos;
    lprintf "\n" []
  end

let close () =
  let useless = ref 0 in		(* Useless rules *)
  do_rules (fun r ->
    if r.r_id > 0 && not r.r_used then begin
      if !useless = 0 then 
	lprintf "\nThe following rules are never reduced:\n" [];
      incr useless;
      lprintf "\t$  ($)\n" [fRule r; fNum r.r_id]
    end);

  close_out !log_file
