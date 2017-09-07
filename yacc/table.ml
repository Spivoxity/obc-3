(*
 * table.ml
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

open Util

type row =
  { v_size: int;
    v_span: int;
    v_entries: (int * int) list;
    mutable v_start: int }

let empty_row = 
  { v_size = 0; v_span = 0; v_entries = []; v_start = 0 }

let start v = v.v_start

(* span -- compute max xs - min xs where xs = map fst ts } *)
let span ts =
  let (lo, hi) =
    List.fold_left (fun (a, b) (x, y) -> (min a x, max b x)) 
      (max_int, min_int) ts in
  hi - lo

let rowtab = Hashtbl.create 500
let rows = ref []

let table = Growvect.create 1000
let check = Growvect.create 1000
let unassigned = -1000			(* Unused slots in check vector *)
let taken = Hashtbl.create 500
let first_free = ref 0

(* make_row -- create a row with a specified list of entries *)
let make_row ts =
  if ts = [] then
    empty_row
  else begin
    let v = { v_size = List.length ts; v_span = span ts; v_start = -999;
		v_entries = List.sort (fun (a, _) (b, _) -> a - b) ts } in
    try Hashtbl.find rowtab v with
      Not_found -> 
	Hashtbl.add rowtab v v;
	rows := v :: !rows; v
  end

(* make_space -- expand the table up to a specified index *)
let make_space i =
  while i >= Growvect.size table do
    Growvect.append table 0; Growvect.append check unassigned
  done

(* fits -- test if a row fits in the table at a certain position *)
let fits v pos =
  pos <> 0
  && List.for_all (fun (i, _) -> 
	make_space (pos+i); Growvect.get check (pos+i) = unassigned) 
      v.v_entries
  && not (Hashtbl.mem taken pos)
  
(* insert -- fill in entries for a row at a chosen position *)
let insert v pos =
  List.iter (fun (i, j) ->
      let k = pos+i in
      Growvect.set table k j; Growvect.set check k i)
    v.v_entries;
  Hashtbl.add taken pos ()

(* pack_rows -- pack all allocated rows into the table *)
let pack_rows () = 
  (* Consider the rows in increasing order of sparsity, hoping that 
     later rows will fit in the spaces left within earlier ones *)
  let sorted =
    List.sort 
      (fun v1 v2 -> lexico (v2.v_span - v1.v_span) (v2.v_size - v1.v_size))
      !rows in
  List.iter (fun v ->
      let pos = ref (!first_free - fst (List.hd v.v_entries)) in
      while not (fits v !pos) do incr pos done;
      insert v !pos; v.v_start <- !pos;
      while !first_free < Growvect.size table 
	  && Growvect.get check !first_free <> unassigned do 
	incr first_free 
      done)
    sorted
