(*
 * procio.ml
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
open Unix
open Util

let interp = ref "obxdeb"
let trace = ref false

let deb_in = ref Pervasives.stdout
let deb_out = ref Pervasives.stdin
let child = ref 0

let send fmt args =
  if !trace then printf "==> $\n" [fMeta fmt args];
  fprintf !deb_in "$\n" [fMeta fmt args];
  flush !deb_in

let receive () = 
  let line = input_line !deb_out in
  if !trace then printf "<== $\n" [fStr line];
  line

external wake : int -> unit = "ml_wake_child"

let wake_child () = wake !child


(* Peeking in memory *)

let cache = Hashtbl.create 1000

let flush_cache () = Hashtbl.clear cache

let peek n a =
  try Hashtbl.find cache (n, a) with
    Not_found ->
      send "peek$ $" [fNum n; fHex32 a];
      let line = receive () in
      let words = split_string line in
      match words with
	  ["value"; sval] -> 
	    let v = Int32.of_string sval in
	    Hashtbl.add cache (n, a) v; v
	| _ -> failwith "peek"

let peek_char addr =
  let n = peek 1 addr in char_of_int (Int32.to_int n)

let peek_float addr = 
  Int32.float_of_bits (peek 4 addr)

let peek_double addr = 
  (* The two halves of a double are always stored in little-endian order *)
  let lsw = Int64.logand (Int64.of_int32 (peek 4 addr)) 0xffffffffL in
  let msw = Int64.of_int32 (peek 4 (offset addr 4)) in
  Int64.float_of_bits (Int64.add (Int64.shift_left msw 32) lsw)

let peek_int64 addr =
  (* Always little-endian *)
  let lsw = Int64.logand (Int64.of_int32 (peek 4 addr)) 0xffffffffL in
  let msw = Int64.of_int32 (peek 4 (offset addr 4)) in
  Int64.add (Int64.shift_left msw 32) lsw

let peek_string addr =
  let max = 60 in
  let null = char_of_int 0 in
  let s = String.create (max+3) in
  let n = ref 0 in
  let ch = ref (peek_char addr) in
  while !n < max && !ch != null do
    s.[!n] <- !ch;
    incr n;
    ch := peek_char (Int32.add addr (Int32.of_int !n))
  done;
  if !ch <> null then begin
     String.fill s max 3 '.';
     n := !n+3
  end;
  String.sub s 0 !n


(* Initialization *)

let connection = ref Unix.stdin
let cleanup = ref (fun () -> ())

let startup args sock addr =
  listen sock 1;
  child := Unix.create_process !interp 
    (Array.of_list ([!interp; "-p"; addr] @ args))
    Unix.stdin Unix.stdout Unix.stderr;
  let (conn, _) = accept sock in
  if Debconf.use_inet then setsockopt conn TCP_NODELAY true;
  connection := conn;
  deb_in := out_channel_of_descr conn;
  deb_out := in_channel_of_descr conn

let init args =
  if Debconf.use_inet then begin
    let sock = socket PF_INET SOCK_STREAM 0 in
    bind sock (ADDR_INET (inet_addr_loopback, 0));
    let port = match getsockname sock with ADDR_INET (_, p) -> p 
		      | _ -> failwith "Procio.open" in
    startup args sock (string_of_int port)
  end
  else begin
    let sock = socket PF_UNIX SOCK_STREAM 0 in
    let sock_name = sprintf "/tmp/debug.$" [fNum (getpid ())] in
    cleanup := (fun () -> unlink sock_name);
    bind sock (ADDR_UNIX sock_name);
    startup args sock sock_name 
  end;
  flush_cache ()

let close () =
  shutdown !connection SHUTDOWN_ALL;
  !cleanup ()
