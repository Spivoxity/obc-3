(*
 * util.ml
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

open Print

(* take -- first n elements of list *)
let rec take n xs =
  if n = 0 || xs = [] then []
  else List.hd xs :: take (n-1) (List.tl xs)

(* drop -- all but first n elements of list *)
let rec drop n xs =
  if n = 0 || xs = [] then xs
  else drop (n-1) (List.tl xs)

(* split -- cleave initial segment that satisfies a test *)
let split p xs =
  let rec loop us vs =
    match vs with
	[] -> (List.rev us, [])
      | y::ys -> if p y then loop (y::us) ys else (List.rev us, vs) in
  loop [] xs 

(* lexico -- lexical product of orderings, represented as 'compare' functions *)
let lexico a b = if a <> 0 then a else b

(* group_sort -- sort a list of pairs and group by first component *)
let group_sort cmp xs =
  let rec group =
    function
	[] -> []
      | (x, y) :: zs -> 
	  let (us, vs) = split (fun (u, v) -> cmp u x = 0) zs in
	  (x, y :: List.map snd us) :: group vs in
  group (List.sort (fun z1 z2 -> cmp (fst z1) (fst z2)) xs)

(* ungroup -- flatten a grouped list of pairs *)
let rec ungroup =
  function
	[] -> []
      | (x, ys) :: zs ->
	  List.map (fun y -> (x, y)) ys @ ungroup zs

(* commonest -- find commonest element of list with comparison function *)
let commonest cmp xs0 =
  let rec vote n a =
    function
	[] -> a
      | x :: xs ->
	  let (us, vs) = split (fun y -> cmp x y = 0) xs in
	  let k = List.length us + 1 in
	  if k > n then vote k x vs else vote n a vs in
  if xs0 = [] then
    raise Not_found
  else
    vote 0 (List.hd xs0) (List.sort cmp xs0)

let flat_map f xs =
  let rec h = function [] -> [] | y::ys -> f y @ h ys in h xs
