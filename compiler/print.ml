(*
 * print.ml
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

type arg = vtable -> unit

and vtable = { outch : char -> unit; prf : string -> arg list -> unit }

let portable_string_of_float x =
  let s = string_of_float x in
  let n = String.length s in
  if String.contains s 'e' && s.[n-3] = '0' then
    String.sub s 0 (n-3) ^ String.sub s (n-2) 2
  else
    s

let fChr c vtab = vtab.outch c

let fStr s vtab =
  for i = 0 to String.length s - 1 do vtab.outch s.[i] done

let fNum n = fStr (string_of_int n)
let fHex n = fStr (Util.hex_of_int n)
let fFlo x = fStr (portable_string_of_float x)
let fBool b = fStr (if b then "true" else "false")
let fNum32 n = fStr (Int32.to_string n)
let fHex32 n = fStr (Util.hex_of_int32 n)

(* do_print -- the guts of printf and friends *)
let rec do_print outch fmt args0 =
  let vtab = { outch = outch; prf = do_print outch } in
  let args = ref args0 in
  for i = 0 to String.length fmt - 1 do
    if fmt.[i] <> '$' then
      outch fmt.[i]
    else begin
      (try List.hd !args vtab; args := List.tl !args
	with Invalid_argument _ -> outch '*'; outch '*'; outch '*');
    end
  done

let fExt g vtab = g vtab.prf

(* fMeta -- insert output of recursive call to printf *)
let fMeta fmt args vtab = vtab.prf fmt args

let fFixNum (n, w) = 
  fExt (fun prf ->
    let digits = string_of_int n in
    let w0 = String.length digits in
    for i = 1 to w - w0 do prf " " [] done;
    prf "$" [fStr digits])

let fSeq(cvt, sep) xs =
  fExt (fun prf ->
    if xs <> [] then begin
      prf "$" [cvt (List.hd xs)];
      List.iter (function y -> prf "$$" [fStr sep; cvt y]) (List.tl xs)
    end)

(* fList -- format a comma-separated list *)
let fList(cvt) xs = fSeq(cvt, ", ") xs

(* fprintf -- print to a file *)
let fprintf fp fmt args = do_print (output_char fp) fmt args

(* printf -- print on standard output *)
let printf fmt args = fprintf stdout fmt args; flush stdout

(* sprintf -- print to a string *)
let sprintf fmt args =
  let sbuf = Buffer.create 20 in
  do_print (Buffer.add_char sbuf) fmt args;
  Buffer.contents sbuf

open Format

let rec do_grind fmt args0 =
  let vtab = { outch = print_char; prf = do_grind } in
  let args = ref args0 in
  for i = 0 to String.length fmt - 1 do
    begin match fmt.[i] with
	'$' ->
	  begin try 
	    List.hd !args vtab;
	    args := List.tl !args 
	  with
	    Invalid_argument _ -> print_string "***"
	  end
      | ' ' -> print_space ()
      | '_' -> print_char ' '
      | '(' -> open_hvbox 2; print_char '('
      | ')' -> print_char ')'; close_box ()
      | ch -> print_char ch
    end
  done

(* |fgrindf| -- pretty-printer *)
let rec fgrindf fp fmt args =
  set_formatter_out_channel fp;
  do_grind fmt args;
  print_newline ()
