(*
 * lalr.ml
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

(* Use array index notation for Vectors *)
module Array = Vector

(* LALR lookahead *)

(* We follow the method of DeRemer and Penello, 'Efficient Computation
   of LALR(1) Look-Ahead Sets', ToPLAS 4, 4, pp. 615-49, 1982 *)

(* Included here is a brief summary of the sets and relations that are 
   computed in the algorithm.

   The LALR lookahead for an item "A --> omega ." in state q is

 	LA(q, A --> omega) = 
	    { t in T | S ==>* alpha A t z  and  alpha omega accesses q }

   This is computed from a number of other sets and relations Reads, DR,
   Includes, Lookback which we now define ... 

   In what follows, a transition t is identified by the pair (p, A) where
   p is the starting state (t.t_source) and A is the symbol consumed
   (t.t_sym). *)

(* If t = (p, A), then reads t = [ u | u = (q, C) & (p, A) Reads (q, C) ]
   where (p, A) Reads (q, C) <==>
	    p -A-> q -C-> r  and C ==>* eps  for some r	*)
let reads t =
  let a = t.t_sym and q = t.t_target in
  if is_token a then [] else
    List.filter (fun u -> nullable u.t_sym) q.p_trans

let tokens p =
  List.filter is_token (List.map (fun u -> u.t_sym) p.p_trans)

(* compute_dr returns the vector { (p, A) |--> DR(p, A) }
   where DR(p, A) = { x in T | p -A-> q -x-> r  for some q, r }	*)
let compute_dr () = 
  let follow = trans_vector SymSet.empty in
  do_transitions (fun t ->
    if is_nonterm t.t_sym then
      follow.(t) <- symset_of_list (tokens t.t_target));
  follow

(* compute_includes returns the vector 
   { (p, A) |--> [ (q, B) | (p, A) Includes (q, B) ] }
   where (p, A) includes (q, B) <==>
	    B -> beta A gamma  and  gamma ==>* eps  and  q --beta--> p *)
let compute_includes () =
  let includes = trans_vector [] in
  let rec loop t q xs =
    match xs with
	[] -> ()
      | y::ys ->
	  let u = get_trans q y in
	  if is_nonterm y && List.for_all nullable ys then
	    includes.(u) <- t :: includes.(u);
	  loop t u.t_target ys in
  do_transitions (fun t ->
    List.iter (fun r -> loop t t.t_source r.r_rhs) t.t_sym.x_rules);
  includes

(* trace p xs = q where p --xs--> q  *)
let rec trace p xs =
  List.fold_left (fun q x -> let t = get_trans q x in t.t_target) p xs

(* compute_lookback returns the vector 
   { q |--> [ (r, (p, A)) | (q, r) Lookback (p, A) ] }
   where (q, A --> omega) Lookback (p, A) <==>
	    p -A-> r is a transition and  p --omega--> q *)
let compute_lookback () =
  let lkb = state_vector [] in
  let add t r = 
    let q = trace t.t_source r.r_rhs in 
    lkb.(q) <- (r, t) :: lkb.(q) in
  do_transitions (fun t -> List.iter (add t) t.t_sym.x_rules);
  lkb

(* lookback lkb (q, r) = [ t | (q, r) Lookback (p, A) & t = (p, A) ]
     where lkb is the table computed by compute_lookback. *)
let lookback lkb (q, r) =
  List.map snd (List.filter (fun (r', _) -> r.r_id = r'.r_id) lkb.(q))

(* For each item z = (q, A --> omega .), we set z.z_lookahead to

	LA(q, A --> omega) =
	    U { Follow(p, A) | (q, A --> omega) lookback (p, A) }

   where Follow is a function computed below. *)

let propagate follow lookb =
  do_states (fun q ->
      ItemSet.iter (fun z ->
	  let r = z.z_rule and k = z.z_index in
	  if k = r.r_len then
	    (* z is an item 'A --> omega .' that will yield a reduction *)
	    z.z_lookahead <-
	      collect (List.map follow (lookb (q, r))))
        q.p_items)

(* The endgame: begin with the DR sets, then compute Read as the
   least solution of

	Read(p, A) =
	    DR(p, A) U union { Read(q, B) | (p, A) Reads (q, B) }

   In turn, Follow(p, A) is the least solution of

	Follow(p, A) =
	    Read(p, A) U union { Follow(q, B) | (p, A) Includes (q, B) }

   And the Follow sets are what we need to compute the lookaheads.

   The fixpoint equations for Follow and Read are both solved by finding
   Strongly Connected Components in a graph where the nodes are transitions
   and the arcs are given by Includes and Reads respectively. *)

let do_lookahead verb =
  let follow = compute_dr () in
  if verb > 2 then Report.show_follow "DR" follow;
  fixpoint do_transitions follow reads;
  if verb > 2 then Report.show_follow "Read" follow;
  let includes = compute_includes () in
  fixpoint do_transitions follow (Vector.get includes);
  if verb > 2 then Report.show_follow "Follow" follow;
  let lkb = compute_lookback () in
  if verb > 2 then Report.show_lkb lkb;
  propagate (Vector.get follow) (lookback lkb)
