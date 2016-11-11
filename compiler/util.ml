(*
 * util.ml
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

open List

(* copy -- list of n copies of a value *)
let rec copy n x =
  if n = 0 then [] else x :: copy (n-1) x

(* take -- first n elements of list *)
let rec take n xs =
  if n = 0 || xs = [] then []
  else hd xs :: take (n-1) (tl xs)

(* drop -- all but first n elements of list *)
let rec drop n xs =
  if n = 0 || xs = [] then xs
  else drop (n-1) (tl xs)

let rec range a b =
  if a > b then [] else a :: range (a+1) b

let split_at c s =
  let i = String.index s c in
  (String.sub s 0 i, String.sub s (i+1) (String.length s - i - 1))

let split_string s =
  let n = String.length s in
  let len = ref 0 and words = ref [] in
  for i = 0 to n do
    if i = n || s.[i] = ' ' || s.[i] = '\t' || s.[i] = '\n' then begin
      if !len > 0 then words := !words @ [String.sub s (i - !len) !len];
      len := 0
    end
    else begin
      incr len
    end  
  done;
  !words

let hex_of_int n = 
  if n = 0 then "0" else Printf.sprintf "0x%.8x" n

let hex_of_int32 n = 
  if n = Int32.of_int 0 then "0" else Printf.sprintf "0x%.8lx" n

let float_as_string x = Printf.sprintf "%.20e" x


(* Make a hash table *)
let make_hash n ps = 
  let table = Hashtbl.create n in
  List.iter (function (x, y) -> Hashtbl.add table x y) ps;
  table

(* Search a directory path *)
let rec search_path fn =
  function
      [] -> raise Not_found
    | d::ds ->
	let name = if d = "." then fn else Filename.concat d fn in
	try let f = open_in name in close_in f; name with
	  Sys_error _ -> search_path fn ds

let can f x = try f x; true with Not_found -> false

(* offset -- add base address and offset *)
let offset addr k = Int32.add addr (Int32.of_int k)
