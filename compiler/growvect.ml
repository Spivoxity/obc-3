(*
 * growvect.ml
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

type 'a t = 
  { mutable size: int; 
    mutable elements: 'a array }

let create n = 
  { size = 0; elements = Array.make n (Obj.magic ()) }

let clear v =
  v.size <- 0

let size v = v.size

let get v i = 
  if i >= v.size then raise (Invalid_argument "index out of bounds");
  Array.get v.elements i

let set v i x =
  if i >= v.size then raise (Invalid_argument "index out of bounds");
  Array.set v.elements i x

let append v x =
  let n = Array.length v.elements in
  if v.size >= n then begin
    let newv = Array.make (2*n) (Obj.magic ()) in
    Array.blit v.elements 0 newv 0 n;
    v.elements <- newv
  end;
  Array.set v.elements v.size x;
  v.size <- v.size+1

let iter f v =
  for i = 0 to v.size-1 do f v.elements.(i) done

let to_list v =
  let rec loop n xs =
    if n = 0 then xs else loop (n-1) (v.elements.(n-1)::xs) in
  loop v.size []
