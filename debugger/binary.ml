(*
 * binary.ml
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

open Util

let addrtab = Hashtbl.create 100

(* Modules *)

let modules = ref []
let cksum_tab = Hashtbl.create 20

let def_module m ck = 
  Hashtbl.add cksum_tab m ck;
  modules := !modules @ [m]

let all_modules () = !modules
let checksum m = Hashtbl.find cksum_tab m


(* Procedures *)

type proc_data =
  { s_name: string;
    s_addr: int32;
    s_code: int32;
    s_size: int }

let proc_addrtab = Hashtbl.create 100
let proc_nametab = Hashtbl.create 100

let def_proc pdata = 
  Hashtbl.add addrtab pdata.s_addr pdata.s_name;
  Hashtbl.add proc_addrtab pdata.s_addr pdata;
  Hashtbl.add proc_nametab pdata.s_name pdata

let proc_lookup p = Hashtbl.find proc_nametab p

let curr_proc cp = 
  if cp = Int32.zero then
    proc_lookup "MAIN"
  else
    Hashtbl.find proc_addrtab cp


(* Variables *)

type var_data =
  { v_name: string;			(* Name, in the form Module.var *)
    v_addr: int32 }			(* Address *)

let var_nametab = Hashtbl.create 100

let def_var vdata = 
  Hashtbl.add var_nametab vdata.v_name vdata;
  Hashtbl.add addrtab vdata.v_addr vdata.v_name

let var_lookup name = 
  let v = Hashtbl.find var_nametab name in v.v_addr

let addr_lookup addr = Hashtbl.find addrtab addr


(* Line numbers *)

type line_data =
  { l_module: Symtab.ident;
    l_num: int;
    l_addr: int32 }

let line_addrtab = Growvect.create 200
let line_loctab = Hashtbl.create 200

let def_line ldata = 
  Growvect.append line_addrtab ldata;
  let x = (ldata.l_module, ldata.l_num) in
  let prev = try Hashtbl.find line_loctab x with Not_found -> [] in
  Hashtbl.add line_loctab x (prev @ [ldata.l_addr])

let prev_line addr =
  (* Use binary search to find the next preceding line boundary *)
  let rec search a b =
    (* Invariant line_addrtab[0..a) <= addr < line_addrtab[b..n) *)
    if a = b then a else
      let m = (a+b)/2 in
      let r = Growvect.get line_addrtab m in
      if r.l_addr <= addr then search (m+1) b else search a m in

  let n = Growvect.size line_addrtab in
  let i = search 0 n in
  if i = 0 then raise Not_found;
  Growvect.get line_addrtab (i-1)

let line_lookup m n = Hashtbl.find line_loctab (m, n)


type regs = { cp: int32; bp: int32; pc: int32 }

type source_loc = 
    NoLoc 
  | Proc of string 
  | Line of Symtab.ident * string * int

let find_loc regs =
  try
    let p = curr_proc regs.cp in
    try
      if regs.pc = Int32.zero then
	Proc p.s_name
      else begin
	let r = prev_line regs.pc (* raise Not_found if out of range *) in
	let (m, _) = split_at '.' p.s_name in
	if m <> Symtab.extern r.l_module then raise Not_found;
	Line (r.l_module, p.s_name, r.l_num)
      end
    with Not_found ->
      (* Can't find line *)
      Proc p.s_name
  with Not_found ->
    (* Proc not in table *)
    NoLoc

let clear () =
  Hashtbl.clear addrtab;
  modules := [];
  Hashtbl.clear proc_addrtab;
  Hashtbl.clear proc_nametab;
  Hashtbl.clear var_nametab;
  Growvect.clear line_addrtab;
  Hashtbl.clear line_loctab;
