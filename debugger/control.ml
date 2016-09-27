(*
 * control.ml
 * 
 * This file is part of the Oxford Oberon-2 compiler
 * Copyright (c) 2008 J. M. Spivey
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
open Symtab
open Binary
open Util
open Procio

(* Interaction *)

type state =
    Ready
  | Stopped
  | LineBreak
  | BreakPoint
  | Interrupted
  | Running
  | Exited
  | Error

let state = ref Ready
let int_flag = ref false
let err_message = ref ""
let location = ref NoLoc
let monitor_version = ref ""

(* regs -- the current register contents *)
let vregs = ref { cp = Int32.zero; bp = Int32.zero; pc = Int32.zero }

let state_name () =
  match !state with
      Ready -> "Ready"
    | Stopped -> "Stopped"
    | LineBreak -> "Line break"
    | BreakPoint -> "Breakpoint"
    | Interrupted -> "Interrupted"
    | Running -> "Running"
    | Exited -> "Exited"
    | Error -> sprintf "Runtime error: $" [fStr !err_message]

let join =
  function
      [] -> ""
    | (x::xs) -> List.fold_left (fun a b -> a ^ " " ^ b) x xs

let rec continue () =
  try
    while true do
      let line = Procio.receive () in
      let words = split_string line in
      match words with
	  ["hello"; v] -> monitor_version := v
	| ["ready"] -> state := Ready; raise Exit
	| ["stop"] -> state := Stopped; raise Exit
	| ["line"] -> state := LineBreak; raise Exit
	| ["break"] -> state := BreakPoint; raise Exit
	| ["interrupt"] -> state := Interrupted; raise Exit
	| ["exit"] -> state := Exited; raise Exit
	| ["quit"] -> (* Panic exit *) exit 1

	| ("error"::line::words) -> 
	    state := Error; 
	    err_message := join words;
	    raise Exit

	| ["regs"; cp; bp; pc] ->
	    vregs := { cp = Int32.of_string cp; bp = Int32.of_string bp;
						pc = Int32.of_string pc }

	| ["module"; name; chksum] ->
	    if name <> "_Builtin" && name <> "%Main" then
	      def_module (intern name) (int_of_string chksum)

	| ["proc"; name; addr; code; size] ->
	    def_proc
	      { s_name = name; s_addr = Int32.of_string addr;
		s_code = Int32.of_string code; s_size = int_of_string size }

	| ["data"; name; addr] ->
	    def_var { v_name = name; v_addr = Int32.of_string addr }

	| ["line"; name; addr] ->
	    begin try
	      let (m, n) = split_at '.' name in
	      def_line 
		{ l_module = intern m; l_num = int_of_string n;
		  l_addr = Int32.of_string addr }
	    with 
	      Not_found -> ()
	    end

	| _ -> 
	    printf "??? $ \n" [fStr line]
    done
  with Exit -> ()

let resume cmd = 
  Procio.flush_cache ();
  state := Running;
  int_flag := false;
  Procio.send cmd []; 
  continue ();
  location := find_loc !vregs

let run () = resume "cont"

let line_test p =
  match !location with 
      Line (_, _, n) -> p n 
    | _ -> false

let step p =
  resume "step";
  while (!state = Stopped || !state = LineBreak)
      && not !int_flag && not (line_test p) do 
    resume "step" 
  done

let step_into () =
  match find_loc !vregs with
      Line (m, p, n) ->
        let bp = !vregs.bp in
        step (fun n' -> !state = LineBreak || !vregs.bp <> bp || n' <> n)
    | _ ->
        step (fun n' -> true)

let step_over () =
  match find_loc !vregs with
      Line (m, p, n) ->
	let bp = !vregs.bp in
	step (fun n' -> !vregs.bp > bp || !vregs.bp = bp && n' <> n)
    | _ ->
	step (fun n' -> true)

let step_out () =
  let bp = !vregs.bp in step (fun n' -> !vregs.bp > bp)

let interrupt () =
  int_flag := true;
  if !state = Running then Procio.wake_child ()

let start_program args =
  Procio.init args;
  state := Ready;
  continue ()

(* Traversing the stack *)

let off_BP = 0 and off_PC = 4 and off_CP = 8 

let up_frame regs =
  (* Fetch the bp and pc from the current frame head, 
     and the cp from the parent, unless bp is null *)
  let bp = peek 4 (offset regs.bp off_BP) in
  { cp = if bp = Int32.zero then Int32.zero else peek 4 (offset bp off_CP);
    bp = bp;
    pc = offset (peek 4 (offset regs.bp off_PC)) (-1) }

let backtrace f =
  let rec loop j regs =
    if regs.bp <> Int32.zero then begin
      f j regs;
      loop (j+1) (up_frame regs)
    end in
  loop 0 !vregs

let set_break m n flag =
  List.iter (fun addr -> 
      send "breakpt $ $" [fHex32 addr; fNum (if flag then 1 else 0)])
    (line_lookup m n)
