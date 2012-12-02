(*
 * switch.ml
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

open Symtab
open Icode
open Mach
open Dict
open Eval

(* The switching code consists of a binary tree of 2-way comparisons
with equality comparisons, range tests and jump tables at the leaves.
A jump table is acceptable if its density -- defined as the number of
non-default entries over the size of the table -- is at least rho; but
we use the adjustment that each range counts at most alpha to the
count of a jump table that covers it, because a range is very compact
representation of a list of contiguous cases.

Unaggregated: 
  one split CONST n / TESTGEQ = 4 bytes or more, 
  range test CONST a / CONST b / JRANGE = 6bytes or more, 
  total = 10 bytes or more

Aggregated:
  2 bytes per jump table entry

So we take alpha = 6 *)

let rho = 0.5				(* Minimum density of jump table *)
let maxtable = 127			(* Max size of jump table *)
let alpha = 6				(* Equivalent cases for range *)

(* The |merge| function is used twice to merge case labels in
different ways.  The idea is that |join| is a function that either
combines two adjacent elements of a (sorted) list, or raises
|Not_found|.  The |merge| function greedily merges elements with
|join| until no more merging is possible. *)

(* |merge| -- combine adjacent elements of list *)
let merge join xs = 
  let rec add x =
    function
	[] -> [x]
      | y::ys ->
	  try add (join x y) ys with Not_found -> x::y::ys in
  List.fold_right add xs []

(* We begin with a list of ranges.  We can glue together adjacent
contiguous ranges that have the same label, even if the programmer
didn't specify a range.  For example, we can convert 1, 2, 3..10 
into 1..10: *)

(* |join_ranges| -- glue together adjacent ranges, if possible *)
let join_ranges (lo1, hi1, lab1) (lo2, hi2, lab2) =
  if lo2 = integer_add hi1 (integer 1) && lab1 = lab2 then
    (lo1, hi2, lab1)
  else
    raise Not_found

(* The next stage of the algorithm works on jump tables: it starts with
many separate tables, then merges them, as long as the density of the
combined table is at least the parameter rho.  The greedy algorithm is
sub-optimal, but good enough.  There is an optimal quadratic algorithm
that uses dynamic programming.

Type |table| represents a single jump table.  Each element is a range
of values associated with the same label; if the jump table consists of
a single such range, it can be implemented with a single range check,
and no actual table is needed. *)

type table = 
  { lowest: integer; highest: integer;	(* Lower and upper bounds *)
    count: integer; 			(* Number of cases *)
    ranges: (integer * integer * codelab) list }
					(* Elements *)

(* |singleton| -- make a jump table with a single range *)
let singleton ((lo, hi, _) as r) =
  let c = integer_add (integer_sub hi lo) (integer 1) in
  { lowest = lo; highest = hi; ranges = [r];
    count = min c (integer alpha) }

(* |join_tables| -- if the result is dense enough, merge two jump tables *)
let join_tables t1 t2 =
  let c = integer_add t1.count t2.count in
  let n = integer_add (integer_sub t2.highest t1.lowest) (integer 1) in
  if float_of_integer c /. float_of_integer n >= rho 
      && n <= integer maxtable then
    { lowest = t1.lowest; highest = t2.highest; 
      count = integer_add t1.count t2.count; 
      ranges = t1.ranges @ t2.ranges }
  else
    raise Not_found

(* Next, we come to the routines that output the code.  The function
|gen_range| deals with a range of values for the same label, and
treats a range of one element as a special case.  The function
|gen_table| generates a true jump table, or a linear search if the
table would be very small.  These functions are used by |gen_tree| to
generate a binary search tree with jump tables or range tests at the
leaves. *)

let gen i = Peepopt.gen i

(* |gen_range| -- generate a range test or single comparison *)
let gen_range lob hib (lo, hi, lab) deflab =
  if lob = lo && hib = hi then begin
    gen (POP 1);
    gen (JUMP lab)
  end
  else if lo = hi then begin
    gen (CONST lo);
    gen (JUMPC (IntT, Eq, lab));
    gen (JUMP deflab)
  end
  else if hib = hi then begin
    gen (CONST lo);
    gen (JUMPC (IntT, Geq, lab));
    gen (JUMP deflab)
  end
  else if lob = lo then begin
    gen (CONST hi);
    gen (JUMPC (IntT, Leq, lab));
    gen (JUMP deflab)
  end
  else begin
    gen (CONST lo);
    gen (CONST hi);
    gen (JRANGE lab);
    gen (JUMP deflab)
  end

(* |gen_table| -- generate a jump table *)
let gen_table lob hib t deflab =
  (* Calculate the explicit list of labels *)
  let labels = 
    let rec tab u =
      function
	  [] -> []
	| (lo, hi, lab) :: rs ->
	    Util.copy (int_of_integer (integer_sub lo u)) deflab 
	      @ Util.copy (int_of_integer (integer_sub hi lo) + 1) lab 
	      @ tab (integer_add hi (integer 1)) rs in
    tab t.lowest t.ranges in
  gen (CONST t.lowest);
  gen (BINOP (IntT, Minus));
  gen (JCASE labels);
  gen (JUMP deflab)

(* |gen_tree| -- generate a binary search, with tables at the leaves *)
let rec gen_tree lob hib tables deflab =
  (* Assume lob <= x <= hib *)
  match tables with
      [] ->
	(* bug fix for empty CASE statements 27/7/01 *)
        gen (POP 1);
	gen (JUMP deflab)
    | [t] ->
	( match t.ranges with
	      [r] -> gen_range lob hib r deflab
	    | _ -> gen_table lob hib t deflab )
    | _ ->
	let lab = label () in
	let n = List.length tables in
	let k = (n+1) / 2 in
	let m = (List.nth tables k).lowest in
	gen (CONST m);
	gen (TESTGEQ lab);
	gen_tree lob (integer_sub m (integer 1)) (Util.take k tables) deflab;
	gen (LABEL lab);
	gen_tree m hib (Util.drop k tables) deflab

(* And finally, we put it all together.  Because the tree has passed
semantic checks, we may assume that there are no duplicate values. *)

(* |switch| -- generate jumping code for case statement *)
let switch cases deflab =
  let my_order (lo1, _, _) (lo2, _, _) = compare lo1 lo2 in
  gen_tree minint maxint
    (merge join_tables 
      (List.map singleton 
	(merge join_ranges 
	  (List.sort my_order cases))))
    deflab
