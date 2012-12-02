(*
 * conflict.ml
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

(* Conflict resolution *)

let sr_conflicts = ref 0
let rr_conflicts = ref 0

(* Resolve conflict between shifting y and going to state p
   and reducing by rule r *)
let resolve_sr report y p r =
  try
    match r.r_prec with
	Some x ->
	  if x.x_kind = Token || y.x_kind = Token then 
	    raise Not_found;
	  if x.x_prec < y.x_prec then Shift p
	  else if x.x_prec > y.x_prec then Reduce r
	  else if x.x_kind = Right then Shift p
	  else if x.x_kind = Left then Reduce r
	  else Error
      | None ->
	  raise Not_found
    with Not_found ->
      (* The conflict is not resolved by precedence: report it and
	 shift anyway *)
      incr sr_conflicts;
      report y (Shift p) (Reduce r);
      Shift p

let rec resolve report (x, acts0) =
  let rec res =
    function
	Shift q :: Reduce r :: acts ->
	  res (resolve_sr report x q r :: acts)
      | Reduce r :: Shift q :: acts ->
	  res (resolve_sr report x q r :: acts)
      | Reduce r1 :: Reduce r2 :: acts ->
	  let (r1', r2') =
	    if r1.r_id <= r2.r_id then (r1, r2) else (r2, r1) in
	  (* Reduce/reduce conflict: prefer r1' over r2' *)
	  incr rr_conflicts;
	  report x (Reduce r1') (Reduce r2');
	  res (Reduce r1' :: acts)
      | Error :: acts -> (x, [Error])
      | [a] -> (x, [a])
      | _ -> failwith "resolve" in
  res acts0

(* Resolve all conflicts in an action list *)
let resolve_conflicts report actions =
  Util.ungroup 
    (List.map (resolve report) 
      (Util.group_sort compare_syms actions))

(* Report any conflicts that were not resolved by precedence *)
let report_conflicts () =
  let report kind n =
    if n > 0 then
      Error.warning "$ $ $\n" [fNum n; fStr kind; 
			fStr (if n > 1 then "conflicts" else "conflict")] in
  report "shift/reduce" !sr_conflicts;
  report "reduce/reduce" !rr_conflicts
