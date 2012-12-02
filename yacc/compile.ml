(*
 * compile.ml
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

(* Compile parse tables *)

let aggressive = ref false

let error_shift =
  function
      (x, Shift p) -> same_syms x error_sym
    | _ -> false

let one_reduction =
  function
      (x, Reduce r) :: acts ->
	let ok = 
	  List.for_all 
	    (function (y, Reduce r') -> r.r_id = r'.r_id | _ -> false) 
	    acts in
	if ok then r else raise Not_found
    | _ -> 
	raise Not_found

let get_reductions acts =
  Util.flat_map 
    (fun (x, a) -> match a with Reduce r -> [r] | _ -> [])
    acts

let yyerr = min_int

let compile actions =
  (* Determine default reduction *)
  let defred =
    begin try
      let r =
	if not !aggressive then
	  (* Byacc-style -- use default reduction only if all actions
	     are the same reduction *)
	  one_reduction actions
	else begin
	  (* Bison-style -- always set default reduction if possible,
	     unless there's a shift on 'error' *)
	  if List.exists error_shift actions then raise Not_found;
	  Util.commonest compare_rules (get_reductions actions)
	end in
      r.r_used <- true; r.r_id
    with Not_found -> 
      0
    end in

  (* Prepare an action row *)
  let actrow = 
    Util.flat_map (function
	 (x, Shift q) -> [(x.x_value, q.p_id)]
       | (x, Reduce r) ->
	   r.r_used <- true;
	   if r.r_id <> defred then [(x.x_value, -r.r_id)] else []
       | (x, Error) ->
	   (* Preserve explicit errors (caused by %nonassoc) from
	      default reduction *)
	   if defred <> 0 then [(x.x_value, yyerr)] else []) 
      actions in

  (actrow, defred)
