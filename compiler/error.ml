(*
 * error.ml
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
open Lexing

let root_file = ref ""
and err_file = ref ""
and err_chan = ref stdin
and err_buf = ref (Lexing.from_string "")
and err_count = ref 0

type location = int * int

let here () = (lexeme_start !err_buf, lexeme_end !err_buf)

let set_loc (x, y) = 
  (* Very kludgy, but works for a final message from the lexer at EOF *)
  let lexbuf = !err_buf in
  lexbuf.lex_start_p <- { lexbuf.lex_start_p with pos_cnum = x };
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_cnum = y }

let no_loc = (0, 0)

(* join_locs -- range spanned by two locations *)
let join_locs (x1, y1) (x2, y2) = (x1, y2)


(* Fetching source lines *)

(* We number lines from 0, and the rest of the world starts from 1.
   Hence the fudge factors at the interface *)

let source_map = Growvect.create 200
let offset = ref 1

let num_lines () = Growvect.size source_map

let note_line pos =
  let n = Growvect.size source_map in
  Growvect.append source_map (pos, n + !offset, !err_file)

let init_errors f ch buf =
  root_file := f; err_file := f; err_chan := ch; err_buf := buf;
  note_line 0

let set_line n f =
  if !Config.debug > 0 then fprintf stderr "set_line $ $\n" [fNum n; fStr f];
  offset := n - Growvect.size source_map;
  err_file := f

let get_line k = 
  try let (pos, _, _) = Growvect.get source_map k in pos
  with Not_found -> failwith (sprintf "Can't find line $" [fNum k])

let source_line n =
  let pos0 = pos_in !err_chan in
  seek_in !err_chan (get_line (n-1));
  let s = try input_line !err_chan with End_of_file -> "" in
  seek_in !err_chan pos0;
  let n = String.length s in
  if n > 0 && s.[n-1] = '\r' then
    String.sub s 0 (n-1)
  else
    s

(* binsearch -- find k such that f k <= x < f (k+1) *)
let binsearch n f x =
  (* Assume x >= f 0 and x < f n *)
  let a = ref 0 and b = ref n in
  (* Inv: 0 <= a < b <= n, f a <= x < f b *)
  while !a+1 <> !b do
    let m = (!a + !b)/2 in
    if f m <= x then a := m else b := m
  done;
  !a

(* find_line -- line containing |p| *)
let find_line p = 
  binsearch (Growvect.size source_map) get_line p + 1

let locator k =
  let (p, n, f) = Growvect.get source_map (k-1) in (n, f)

let line_num loc = if loc = no_loc then 0 else find_line (fst loc)

let end_loc (p, q) = (q, q)

let line_start k = get_line (k-1)


(* Error messages *)

(* From this point on, everything is base 1 *)

let width = 77
let min_width = 50

let break mark s = 
  let w = width - String.length mark in
  let msg = ref s in
  while String.length !msg > w do
    let i = ref w in
    while !i > min_width && !msg.[!i] <> ' ' do decr i done;
    fprintf stderr "$$\n" [fStr mark; fStr (String.sub !msg 0 !i)];
    msg := String.sub !msg (!i+1) (String.length !msg - !i - 1)
  done;
  fprintf stderr "$$\n" [fStr mark; fStr !msg]

let spotless = ref true

let headline loc fmt args =
  if not !spotless then fprintf stderr "\n" [];
  spotless := false;
  if loc = no_loc then
    break "" (sprintf "\"$\", line 1: $" [fStr !root_file; fMeta fmt args])
  else begin
    let (x, y) = loc in
    let line0 = find_line x in
    let (n, f) = locator line0 in
    break "" (sprintf "\"$\", line $: $" [fStr f; fNum n; fMeta fmt args])
  end

let space text a b c =
  for i = b to c - 1 do
    if i < a + String.length text && text.[i-a] = '\t' then
      fprintf stderr "\t" []
    else
      fprintf stderr " " []
  done

let mark b c =
  let line1 = find_line b and line2 = find_line c in
  fprintf stderr "^" [];
  if c > b+1 then begin
    if line1 <> line2 then 
      fprintf stderr "..." []
    else begin
      for i = b+1 to c-1 do fprintf stderr "^" [] done;
    end
  end

let err_message fmt args loc =
  headline loc fmt args;
  if loc <> no_loc then begin
    let (x, y) = loc in
    let line0 = find_line x in
    let text0 = source_line line0 in
    let beg0 = line_start line0 in
    fprintf stderr "> $\n" [fStr text0];
    fprintf stderr "> " [];
    space text0 beg0 beg0 x;
    mark x y;
    fprintf stderr "\n" []	
  end

let err_message2 fmt args loc1 loc2 =
  (* Two locations; we assume loc1 < loc2 *)
  headline loc2 fmt args;
  let (x, y) = loc1 and (u, v) = loc2 in
  let line1 = find_line x and line2 = find_line u in
  let text1 = source_line line1 and text2 = source_line line2 in
  let beg1 = line_start line1 and beg2 = line_start line2 in
  fprintf stderr "> $$\n" 
    [fStr text1; fStr (if line2 > line1+1 then " ..." else "")];
  fprintf stderr "> " [];
  space text1 beg1 beg1 x;
  mark x y;
  if line1 = line2 then
    space text2 beg2 v x
  else begin
    fprintf stderr "\n" [];
    fprintf stderr "> $\n" [fStr text2];
    fprintf stderr "> " [];
    space text2 beg2 beg2 x
  end;
  mark u v;
  fprintf stderr "\n" [];
  flush stderr

let line () = line_num (here ())

let syn_error fmt args loc = 
  incr err_count;
  err_message fmt args loc

let syn_error2 fmt args loc loc2 =
  incr err_count;
  err_message2 fmt args loc loc2

let sem_error fmt args loc = 
  incr err_count;
  err_message fmt args loc

let capitalize s0 =
  let s = Bytes.of_string s0 in
  if Bytes.length s > 0 then 
    Bytes.set s 0 (Char.uppercase (Bytes.get s 0));
  Bytes.to_string s

let sem_context fmt args =
  break "> " (sprintf (capitalize fmt) args);
  flush stderr

let sem_warn fmt args loc =
  if !Config.warnings then
    err_message "warning -- $" [fMeta fmt args] loc

let fToken = 
  let f prf = prf "$" [fStr (lexeme !err_buf)] in fExt f

let sem_extend fmt args loc =
  sem_error fmt args loc;
  sem_context "(Use the -x flag to remove this restriction)" []
