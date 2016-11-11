(*
 * lr0.mli
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

(* This module computes the LR(0) automaton for the grammar *)

(* item -- type of LR(0) items *)
type item = 
  { (* The basic item: *)
    z_rule: rule;			(* Production rule *)
    z_index: int;			(* Index in RHS *)

    (* Added by LALR *)
    mutable z_lookahead: SymSet.t }	(* Lookahead set *)

val fItem : item -> Print.arg

(* ItemSet -- ADT of sets of items *)
module ItemSet : Set.S with type elt = item

(* state -- state of LR(0) automaton *)
type state = 
  { p_id: int;
    p_items: ItemSet.t;
    mutable p_trans: transition list }

(* transition -- state transition in LR(0) machine *)
and transition = 
  { t_id: int;
    t_source: state;
    t_sym: symbol;
    t_target: state }

(* fTrans -- format a transition as (t_source.p_id, t_sym) *)
val fTrans : transition -> Print.arg

(* action -- potential parser action *)
type action =
    Shift of state
  | Reduce of rule
  | Error

(* fAction -- format a parser action for printing *)
val fAction : action -> Print.arg

(* compute_states -- compute the LR(0) automaton for the grammar *)
val compute_states : unit -> unit

(* get_trans -- find transition for state and symbol *)
val get_trans : state -> symbol -> transition

val do_states : (state -> unit) -> unit
val do_transitions : (transition -> unit) -> unit

val state_vector : 'a -> (state, 'a) Vector.t
val trans_vector : 'a -> (transition, 'a) Vector.t

(* actions_for -- all potential parser actions in a state, using lookahead *)
val actions_for : state -> (symbol * action) list

(* gotos_for -- compute all gotos from a state *)
val gotos_for : state -> (symbol * state) list
