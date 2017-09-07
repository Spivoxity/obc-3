(*
 * lr0.ml
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

open Grammar
open Print

(* We represent LR0 items by pairs (r, k), where r is a rule 
  and k is an index in its RHS.  We can label them with lookahead too *)
type item = 
  { z_rule: rule;
    z_index: int;
    mutable z_lookahead: SymSet.t }

let make_item r k =
  { z_rule = r; z_index = k; z_lookahead = SymSet.empty }

let fItem z = 
  fExt (fun prf ->
    let r = z.z_rule and k = z.z_index in
    prf "$ -->" [fSym r.r_lhs];
    List.iter (fun x -> prf " $" [fSym x]) (Util.take k r.r_rhs);
    prf " ." [];
    List.iter (fun x -> prf " $" [fSym x]) (Util.drop k r.r_rhs);
    prf "  ($)" [fNum r.r_id])


(* Sets of items *)

module ItemSet = Set.Make(struct
  type t = item

  let compare z1 z2 =
    (* Compare only the rule numbers and indices *)
    Util.lexico (z2.z_index - z1.z_index) (compare_rules z1.z_rule z2.z_rule) 
end)

let itemset_of_list xs =
  List.fold_left (fun s x -> ItemSet.add x s) ItemSet.empty xs

let init_itemset x =
  itemset_of_list (List.map (fun r -> make_item r 0) x.x_rules)

(* next_syms -- set of next symbols for a set of LR0 items *)
let next_syms s =
  symset_of_list (Util.flat_map (fun z ->
      let r = z.z_rule and k = z.z_index in
      if k = r.r_len then [] else [List.nth r.r_rhs k])
    (ItemSet.elements s))

(*
Clos(s) = s u f(s) u f^2(s) u ...

t1 = s 			d1 = f(s)
t2 = s u d1 = s u f(t1)	d2 = f(d1 \ t1)
t3 = t2 u d2 = s u f(t1) u f(d1 \ t1) = s u f(t1 u t1) = s u f(t2)
			d3 = f(d2 \ t2)
t4 = t3 u d3 = s u f(t2) u f(d2 \ t2) = s u f(t2 u d2) = s u f(t3)

etc.
*)

(* closure -- closure of a set of LR0 items *)
let closure s0 =
  let step s = 
    SymSet.fold 
      (fun x s' -> ItemSet.union s' (init_itemset x)) 
      (next_syms s) ItemSet.empty in
  let rec loop t d =
    let d' = ItemSet.diff d t in
    if ItemSet.is_empty d' then t else
      loop (ItemSet.union t d') (step d') in
  loop s0 (step s0)

(* goto -- goto function *)
let goto s x =
  itemset_of_list (Util.flat_map (fun z ->
      let r = z.z_rule and k = z.z_index in
      if k = r.r_len then []
      else begin
	let y = List.nth r.r_rhs k in
	if not (same_syms x y) then []
	else [make_item r (k+1)]
      end) 
    (ItemSet.elements s))
      

(* States and transitions *)

type state =
  { p_id: int;
    p_items: ItemSet.t;
    mutable p_trans: transition list }

and transition =
  { t_id: int;
    t_source: state;
    t_sym: symbol;
    t_target: state }

and action =
    Shift of state
  | Reduce of rule
  | Error

let fTrans t = 
  fMeta "($, $)" [fNum t.t_source.p_id; fSym t.t_sym]

let fAction =
  function
      Shift p -> fMeta "shift $" [fNum p.p_id]
    | Reduce r -> fMeta "reduce $" [fNum r.r_id]
    | Error -> fStr "error"

let get_trans p x =
  try
    List.find (fun t -> same_syms t.t_sym x) p.p_trans
  with Not_found -> 
    failwith "get_trans"

let state_dict = Growvect.create 500
let nstates = ref 0
let ntrans = ref 0

module StateHash = Hashtbl.Make(struct
  type t = ItemSet.t

  let equal s1 s2 = ItemSet.equal s1 s2

  let hash s = 
    ItemSet.fold (fun z h -> 5 * z.z_rule.r_id + z.z_index + h) s 0
end)

let compute_states () =
  (* A temporary hash table maps sets of items to the corresponding state *)
  let state_table = StateHash.create 500 in

  (* A queue of states waiting for their transitions to be added *)
  let queue = Queue.create () in

  let find_state a =
    try StateHash.find state_table a with
      Not_found ->
	let i = !nstates in
	let p = { p_id = i; p_items = closure a; p_trans = [] } in
	incr nstates;
	StateHash.add state_table a p;
	Growvect.append state_dict p;
        Queue.add p queue; p in

  let make_trans p x q =
    let i = !ntrans in
    incr ntrans;
    { t_id = i; t_source = p; t_sym = x; t_target = q } in

  ignore (find_state (init_itemset root_sym));
  while not (Queue.is_empty queue) do 
    let p = Queue.take queue in
    let a = p.p_items in
    p.p_trans <-
      List.map (fun x ->
	  let q = 
	    if same_syms x eof_sym then p 
	    else find_state (goto a x) in
	  make_trans p x q)
	(SymSet.elements (next_syms a))
  done

let do_states f =
  for i = 0 to !nstates-1 do f (Growvect.get state_dict i) done

let do_transitions f =
  do_states (fun p -> List.iter f p.p_trans)

let state_vector y = Vector.create !nstates (fun p -> p.p_id) y
let trans_vector y = Vector.create !ntrans (fun t -> t.t_id) y


(* Parsing actions *)

let actions_for p =
  let shifts =
    (* [ (x, Shift q) | p --x--> q ] *)
    List.map (fun t -> (t.t_sym, Shift t.t_target))
      (List.filter (fun t -> is_token t.t_sym 
	&& not (same_syms t.t_sym eof_sym)) p.p_trans) in
  let reductions =
    (* [ (x, Reduce r) | (A -> omega .) in p and x in LA(p, A --> omega) ] *)
    Util.flat_map (fun z ->
	let r = z.z_rule and k = z.z_index in
	if k < r.r_len then [] else
	  List.map (fun x -> (x, Reduce r))
            (SymSet.elements (SymSet.remove error_sym z.z_lookahead)))
      (ItemSet.elements p.p_items) in
  shifts @ reductions

let gotos_for p =
  (* [ (A, q) | p --A--> q ] *)
  List.map (fun t -> (t.t_sym, t.t_target)) 
    (List.filter (fun t -> is_nonterm t.t_sym) p.p_trans)

