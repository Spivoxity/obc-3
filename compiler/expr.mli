(*
 * expr.mli
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

open Symtab
open Tree
open Dict
open Eval
open Error

(* lookup_def -- find definition of a name, give error if none *)
val lookup_def : name -> environment -> def

(* lookup_typename -- look up a type name *)
val lookup_typename : name -> environment -> def

(* is_var -- check that expression denotes a variable *)
val is_var : expr -> bool

(* find_field -- search for field or method, or raise Not_found *)
val find_field : ident -> otype -> def

(* check_desig -- check and annotate a designator, return its type *)
val check_desig : expr -> environment -> otype

(* check_expr -- check and annotate an expression, return its type *)
val check_expr : expr -> environment -> otype

(* check_assign -- check for assignment compatibility *)
val check_assign : 
  expr -> otype -> environment -> string -> Print.arg list -> unit

(* dynamic -- check if an expression has dynamic type *)
val dynamic : expr -> bool

(* check_typetest -- check a type test *)
val check_typetest : expr -> otype -> location -> unit

(* check_call -- check a procedure call *)
val check_call : 
  expr -> expr list -> expr -> environment -> bool -> otype

(* check_const -- check a constant expression, returning type and val *)
val check_const : expr -> environment -> string -> otype * value

(* check_tconst -- check for a constant of specified type *)
val check_tconst : expr -> otype -> environment -> string -> value

(* check_qual -- convert expression to qualified name if possible *)
val check_qual : expr -> environment -> unit

val err_context : environment ref

val sem_type : otype -> unit
