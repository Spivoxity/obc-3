(*
 * table.mli
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

(* The parse tables contain a number of sparse rows: a row of
actions for each state, indexed by the lookahead token; and a row
of gotos for each nonterminal, indexed by the uncovered state.
Several of these rows may be the same: for example, it's common for
multiple states to share the same action row.  So this module
manages the rows, first sharing those that appear more than once,
and second by packing them into a 'checked table'.

The checked table consists of two arrays, table and check.  Each
non-empty row v packed into the table has a distinct base b, with
the property that v[i] is non-zero only if (b+i) is within the bounds
of the arrays and check[b+i] = i; the value of v[i] is then given by
table[b+i].  The cells used by different rows can be interleaved
arbitrarily, provided that two occupied cells do not clash, and each
row has a distinct base.  Then the element v[i], stored at
table[b+i], cannot be mistaken for the element v'[j] even if b+i =
b'+j, because b <> b' implies i <> j, so check[b'+j] <> j. *)

(* row -- type of parse table rows *)
type row

(* empty_row -- the empty row *)
val empty_row : row

(* make_row -- create a row from a table of (index, value) pairs *)
val make_row : (int * int) list -> row

(* pack_rows --  pack all rows into the table *)
val pack_rows : unit -> unit

(* After pack_rows ... *)

(* start -- fetch base of row after packing *)
val start : row -> int

(* table, check -- the checked table itself *)
val table : int Growvect.t
val check : int Growvect.t
