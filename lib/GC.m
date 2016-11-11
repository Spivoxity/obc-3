(*
 * GC.m
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

(** Control of the garbage collector *)
MODULE GC;

IMPORT SYSTEM;

(** Collect -- perform a garbage collection *)
PROCEDURE Collect* IS "GC_Collect";
(* CODE gc_collect(bp); *)

(** HeapSize -- return the total size of the heap *)
PROCEDURE HeapSize*(): INTEGER IS "GC_HeapSize";
(* CODE ob_res.i = gc_heap_size(); *)

(** All debugging flags *)
CONST all* = "abcdghlmz";

(** Debug -- set flags for debugging the garbage collector *)
PROCEDURE Debug*(flags: ARRAY OF CHAR) IS "GC_Debug";
(* CODE gc_debug((char * ) args[0].x); *)

(** Dump -- print heap layout *)
PROCEDURE Dump* IS "GC_Dump";
(* CODE gc_dump(); *)

PROCEDURE AllocSize*(p: SYSTEM.PTR): INTEGER IS "GC_AllocSize";
(* CODE ob_res.i = gc_alloc_size(args[0].x); *)

END GC.
