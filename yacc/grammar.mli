(*
 * grammar.mli
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

type assoc = Left | Right | Nonassoc | Token | Nonterm

type symbol =
  { x_id: int;				(* Serial number *)
    x_name: string;			(* Spelling *)
    mutable x_type: string;		(* Type tag *)
    mutable x_kind: assoc;		(* Associativity *)
    mutable x_prec: int;		(* Precedence level *)
    mutable x_genuine: bool;		(* True for tokens known to scanner *)
    mutable x_rules: rule list;		(* List of productions with this lhs *)
    mutable x_nullable: bool;		(* True if nonterm derives empty *)
    mutable x_value: int;		(* Runtime value *) }

and rule = 
  { r_id: int;				(* Serial number *)
    r_lhs: symbol;			(* Left hand side: a nonterminal *)
    r_rhs: symbol list;			(* Right hand side *)
    r_len: int;				(* Length of RHS *)
    r_prec: symbol option;		(* Precedence specification *)
    r_pos: Lexing.position;		(* Position in grammar file *)
    r_semact: Lexing.position * string;	(* Semantic action *)
    mutable r_context: symbol list;	(* Stack context for semantic action *)
    mutable r_used: bool }		(* True if rule is ever reduced *)


(* Symbols *)

val lookup : string -> symbol
val fix_token : string -> int -> symbol

val same_syms : symbol -> symbol -> bool
val compare_syms : symbol -> symbol -> int

(* symbol_vector -- make array indexed by symbol *)
val symbol_vector: 'a -> (symbol, 'a) Vector.t

val do_syms : (symbol -> unit) -> unit
val do_nonterms : (symbol -> unit) -> unit

module SymSet : Set.S with type elt = symbol

val symset_of_list : symbol list -> SymSet.t

(* collect -- union of a list of SymSets *)
val collect : SymSet.t list -> SymSet.t

(* The grammar is augmented by productions

    *start* --> *entry* EOF
    *entry* --> *1* start1
    *entry* --> *2* start2

   etc., with additional productions for each start symbol. 
   Here *1*, *2* are tokens with numeric value 1, 2, ... *)

val root_sym : symbol (* *start* *)
val entry_sym : symbol (* *entry* *)
val eof_sym : symbol (* EOF *)
val error_sym : symbol (* error *)
val start_syms : symbol list ref (* start1, start2, etc. *)

val is_nonterm : symbol -> bool
val is_token : symbol -> bool

(* has_value -- whether a symbol is a nonterm with defined type *)
val has_value : symbol -> bool


(* Rules *)

val create_rule : symbol -> symbol list -> symbol option
	-> Lexing.position * string -> Lexing.position -> rule

val make_rule : symbol -> symbol list -> unit

val compare_rules : rule -> rule -> int

val do_rules : (rule -> unit) -> unit


(* Printf formatting *)

val fSym : symbol -> Print.arg
val fRule : rule -> Print.arg
val fSymSet : SymSet.t -> Print.arg


(* Phases of processing *)

(* This module takes an active part in the parser generation in two
ways: first, when the values that represent tokens and nonterminals at
runtime are assigned, and second, when it computes which nonterminals
are _nullable_, i.e., derive the empty string. *)

(* assign_values -- assign a runtime values to each (genuine) symbol *)
val assign_values : unit -> unit

(* num_nts -- count nonterminals *)
val num_nts : int ref

(* num_toks -- count (genuine) tokens *)
val num_toks : int ref

(* preprocess -- fill in the x_nullable fields, etc. *)
val preprocess : unit -> unit

(* nullable -- test if a nonterminal derives the empty string *)
val nullable : symbol -> bool

val first : symbol -> SymSet.t

val first_seq : symbol list -> SymSet.t

val follow : symbol -> SymSet.t


(* SCC-based fixpoint calculation is needed in various places: to
   compute First and Follow sets for SLR; and also to compute lookahead
   sets in LALR.  The |fixpoint| function encapsulates a one-pass
   fixpoint algorithm that uses Tarjan's algorithm for finding the
   strongly connected component.  Call |fixpoint iter f kids|, where
   |iter| iterates over the nodes of the graph, |f| is a vector that
   contains the seed, and |kids x| returns the successors of a node |x|.
   The final value of |f| is the least solution of the fixpoint equation,

	f(x) = f_0(x) U Union { f(y) | y in kids x }.

   The values of |f| are sets of grammar symbols. *)

(* fixpoint -- calculate fixpoints in a directed graph *)
val fixpoint : (('a -> unit) -> unit) 
  -> ('a, SymSet.t) Vector.t -> ('a -> 'a list) -> unit
