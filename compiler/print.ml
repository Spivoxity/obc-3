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

type arg = 
    Str of string 		(* String *)
  | Chr of char 		(* Character *)
  | Ext of ((string -> arg list -> unit) -> unit)  (* Extension *)

let portable_string_of_float x =
  let s = string_of_float x in
  let n = String.length s in
  if String.contains s 'e' && s.[n-3] = '0' then
    String.sub s 0 (n-3) ^ String.sub s (n-2) 2
  else
    s

let fNum n = Str (string_of_int n)
let fHex n = Str (Util.hex_of_int n)
let fFlo x = Str (portable_string_of_float x)
let fStr s = Str s
let fChr c = Chr c
let fBool b = Str (if b then "true" else "false")
let fExt f = Ext f

(* do_string -- apply function to each char of a string *)
let do_string f s =
  for i = 0 to String.length s - 1 do f s.[i] done

(* do_print -- the guts of printf and friends *)
let rec do_print out_fun fmt args =
  let n = String.length fmt in
  let i, j = ref 0, ref 0 in
  while !i < n do
    let c = fmt.[!i] in
    if c <> '$' then
      out_fun c
    else begin
      (try print_arg out_fun (List.nth args !j) 
	with Invalid_argument _ -> do_string out_fun "***");
      incr j
    end;
    incr i
  done

(* print_arg -- convert a printf argument *)
and print_arg out_fun =
  function
      Str s -> do_string out_fun s
    | Chr c -> out_fun c
    | Ext f -> f (do_print out_fun)

let fFixNum (n, w) = 
  let f prf =
    let digits = string_of_int n in
    let w0 = String.length digits in
    for i = 1 to w - w0 do prf " " [] done;
    prf "$" [Str digits] in
  fExt f

let fNum32 n = Str (Int32.to_string n)
let fHex32 n = Str (Util.hex_of_int32 n)

(* fMeta -- insert output of recursive call to printf *)
let fMeta fmt args = fExt (function prf -> prf fmt args)

let fSeq(cvt, sep) xs =
  let f prf =
    if xs <> [] then begin
      prf "$" [cvt (List.hd xs)];
      List.iter (function y -> prf "$$" [Str sep; cvt y]) (List.tl xs)
    end in
  fExt f

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
