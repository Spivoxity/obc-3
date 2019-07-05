(*
 * grammar.ml
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

type assoc = Left | Right | Nonassoc | Token | Nonterm

type symbol =
  { x_id: int;
    x_name: string;
    mutable x_type: string;
    mutable x_kind: assoc;
    mutable x_prec: int;
    mutable x_genuine: bool;
    mutable x_rules: rule list;
    mutable x_nullable: bool;
    mutable x_value: int }

and rule = 
  { r_id: int;
    r_lhs: symbol;
    r_rhs: symbol list;
    r_len: int;
    r_prec: symbol option;
    r_pos: Lexing.position;
    r_semact: Lexing.position * string;
    mutable r_context: symbol list;
    mutable r_used: bool }

let symbol_table = Hashtbl.create 100
let symbol_vec = Growvect.create 100

let lookup name = 
  try Hashtbl.find symbol_table name with
    Not_found ->
      let x = { x_id = Growvect.size symbol_vec; x_name = name; 
	  x_type = ""; x_kind = Nonterm; x_genuine = false; 
	  x_nullable = false; x_prec = 0; x_rules = []; x_value = -1 } in
      Hashtbl.add symbol_table name x;
      Growvect.append symbol_vec x;
      x

let make_token name =
  let x = lookup name in x.x_kind <- Token; x

let fSym x = fStr x.x_name

let fRule r = 
  fExt (fun prf ->
    prf "$ -->" [fSym r.r_lhs];
    List.iter (fun x -> prf " $" [fSym x]) r.r_rhs)

let same_syms x y = (x.x_id = y.x_id)
let compare_syms x1 x2 = x1.x_id - x2.x_id

let is_nonterm x = x.x_kind = Nonterm
let is_token x = x.x_kind <> Nonterm
let nullable x = x.x_nullable
let has_value x = (is_nonterm x || x.x_type <> "")

let symbol_vector x = 
  Vector.create (Growvect.size symbol_vec) (fun x -> x.x_id) x

let do_syms f =
  Growvect.iter f symbol_vec

let do_nonterms f =
  do_syms (fun x -> if is_nonterm x then f x)

let do_tokens f =
  do_syms (fun x -> if is_token x then f x)

let num_toks = ref 1
let num_nts = ref 0

let fix_token name v =
  let x = make_token name in 
  x.x_value <- v;
  num_toks := max !num_toks (v+1);
  x

let root_sym = lookup "*start*"
let entry_sym = lookup "*entry*"
let error_sym = let x = make_token "error" in x.x_value <- -1; x
let eof_sym = let x = make_token "EOF" in x.x_value <- 0; x

let assign_values () =
  eof_sym.x_genuine <- true;
  do_syms (fun x ->
    if x.x_value < 0 then begin
      if x.x_kind = Nonterm then begin
	if x.x_genuine && x.x_rules = [] then
	  Error.warning "nonterminal '$' has no rules" [fSym x];
	if not (same_syms x root_sym) then
	  (x.x_value <- !num_nts; incr num_nts)
      end 
      else begin
	if x.x_genuine then
	  (x.x_value <- !num_toks; incr num_toks)
      end
    end)

let start_syms = ref []

let rule_vec = Growvect.create 100

let create_rule lhs rhs prspec action pos =
  let r = { r_id = Growvect.size rule_vec; r_lhs = lhs; r_rhs = rhs; 
      r_len = List.length rhs;  r_prec = prspec; r_semact = action; 
      r_used = false; r_pos = pos; r_context = rhs } in
  if lhs.x_kind <> Nonterm then
    Error.error "LHS of rule '$' is a token" [fRule r];
  Growvect.append rule_vec r;				
  lhs.x_rules <- lhs.x_rules @ [r];
  r

let make_rule lhs rhs = 
  ignore (create_rule lhs rhs None (Lexing.dummy_pos, "") Lexing.dummy_pos)

let compare_rules r1 r2 = r1.r_id - r2.r_id

let do_rules f = Growvect.iter f rule_vec


(* SETS OF SYMBOLS *)

module SymSet = Set.Make(struct
  type t = symbol
  let compare = compare_syms
end)

let symset_of_list xs =
  List.fold_left (fun s x -> SymSet.add x s) SymSet.empty xs

let collect sets = List.fold_left SymSet.union SymSet.empty sets

let list_of_symset s = SymSet.elements s

let fSymSet x = fMeta "{$}" [fList(fSym) (SymSet.elements x)]


(* FIXPOINTS *)

(* The following algorithm, a variant of one due to Tarjan, is based
   on depth-first search.  Each node is given a discovery time, and
   also a timestamp is computed that is the discovery time of an
   ancestor node reachable from it.  Nodes are pushed on a stack in
   order of discovery.  A node is the root of an SCC if its discovery
   time is equal to its final timestamp, and if so, it is responsible
   for popping all the nodes in the SCC off the stack, and making all
   their F values equal.

   Tarjan sets tstamp.(u) <- min tstamp.(u) dtime.(v) in the case
   where v is already visited so (u,v) is a cross edge: then tstamp.(u)
   is the minimum discovery time of any node reachable from u by a
   sequence of tree edges followed by one cross edge.

   The timestamp computed here has a different meaning: it's the dtime
   of some node reachable from v, and guaranteed to be <= the timestamp
   computed by the algorithm just described.  That's good enough to
   identify roots, and avoids having to save an array of dtimes. *)

(* A horrid pun to allow array notation to be used for vectors *)
module Array = Vector

(* fixpoint -- compute fixpoint by SCC algorithm *)
let fixpoint iter f kids =
  let infinity = 1000000 in
  let time = ref 0 in
  let tstamp = Vector.clone f (-1) in
  let stack = ref [] in

  (* traverse -- do a DFS traversal from u *)
  let rec traverse u =
    if tstamp.(u) < 0 then begin
      incr time;
      (* td is the discovery time for u *)
      let td = !time in
      tstamp.(u) <- td;
      stack := u::!stack;

      (* Visit the children, setting tstamp(u) to the discovery time
         of an ancestor reachable from u and collecting f values *)
      let visit v =
         traverse v;
         tstamp.(u) <- min tstamp.(u) tstamp.(v);
         f.(u) <- SymSet.union f.(u) f.(v) in
      List.iter visit (kids u);

      (* If u is the root of an SCC, then the top of the stack holds
         some entries v with tstamp(v) >= td.  Pop them and set their
         f values all alike *)
      let rec popscc () =
        match !stack with
            v :: stk' when tstamp.(v) >= td ->
              tstamp.(v) <- infinity; f.(v) <- f.(u);
              stack := stk'; popscc ()
          | _ -> () in
      if tstamp.(u) = td then popscc ()
    end in

  (* Traverse starting from every node *)
  iter traverse


(* PREPROCESSING *)

let do_nullable () =
  (* Focus on rules that have no terminals on the rhs *)
  let pure = ref [] in
  do_rules (fun r ->
    if List.for_all is_nonterm r.r_rhs then pure := r :: !pure);

  (* Iteratively mark the lhs of any rule where the rhs is all marked *)
  let rec loop () =
    let changed = ref false in
    List.iter (fun r ->
      if not (nullable r.r_lhs) 
	  && List.for_all nullable r.r_rhs then begin
	changed := true;
	r.r_lhs.x_nullable <- true
      end) !pure;
    if !changed then loop () in

  loop ()

let first_fun = ref (fun x -> failwith "first_fun")

let first x = !first_fun x

let rec first_seq =
  function
      [] -> SymSet.empty
    | x::xs ->
	if nullable x then 
	  SymSet.union (first x) (first_seq xs)
	else
	  first x

let do_first () =
  let first = symbol_vector SymSet.empty in
  let kids = symbol_vector SymSet.empty in
  do_tokens (fun x -> first.(x) <- SymSet.singleton x);
  do_rules (fun r ->
    let rec loop =
      function
	  [] -> ()
	| x::xs ->
	    kids.(r.r_lhs) <- SymSet.add x kids.(r.r_lhs);
	    if nullable x then loop xs in
    loop r.r_rhs);
  fixpoint do_syms first (fun x -> list_of_symset kids.(x));
  first_fun := Vector.get first  

let follow_fun = ref (fun x -> failwith "follow_fun")

let follow x = !follow_fun x

let do_follow () =
  let follow = symbol_vector SymSet.empty in
  let kids = symbol_vector SymSet.empty in
  do_rules (fun r ->
    let rec loop =
      function
	  [] -> ()
	| x::xs ->
	    follow.(x) <- SymSet.union follow.(x) (first_seq xs);
	    if List.for_all nullable xs then
	       kids.(x) <- SymSet.add r.r_lhs kids.(x);
	    loop xs in
     loop r.r_rhs);
   fixpoint do_nonterms follow (fun x -> list_of_symset kids.(x));
   follow_fun := Vector.get follow

let preprocess () =
  do_nullable ();
  do_first ();
  do_follow ()
