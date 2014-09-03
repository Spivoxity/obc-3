(*
 * tree.mli
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

open Symtab
open Dict
open Eval
open Error

(*
This module describes the type of abstract syntax trees that is used
as the main interface between parts of the comipler.  A tree is built
by the parser, then checked by the semantic analyser, which annotates
identifiers in the tree with their definitions.  The intermediate code
generator finally traverses the tree, emitting code for each
expression or statement.

This module also contains some functions that are used to build the
tree initially; they construct nodes with default values for the
annotations.  Proper values are filled in later during semantic
analysis.
*)

(* name -- type for applied occurrences, with mutable annotations *)
type name = 
  { x_name: ident; 		(* name of the reference *)
    x_module: ident;		(* module, or current if none specified *)
    x_export: export;		(* export mark (if defining occurrence) *)
    x_loc: location; 		(* line number *)
    mutable x_def: def option } (* definition in scope *)

(* abstract syntax *)
type program = 
  Module of name * import list * block * def list ref * docstring

and import = name * ident * int ref

and block = Block of decl list * stmt * expr option * int ref | NoBlock

and decl = 
    ConstDecl of name * expr * docstring
  | VarDecl of def_kind * name list * typexpr * docstring
  | TypeDecl of (name * typexpr * docstring) list
  | ProcDecl of proc_kind * name * proc_heading * block * docstring
  | PrimDecl of name * proc_heading * string * docstring
  | ForwardDecl of proc_kind * name * proc_heading * docstring
  | DummyDecl

and proc_heading = Heading of decl list * name option

and stmt = 
  { mutable s_guts: stmt_guts;	(* The statement itself *)
    s_loc: location }		(* Starting line *)

and stmt_guts =
    Assign of expr * expr
  | ProcCall of expr
  | Return of expr option
  | IfStmt of (expr * stmt) list * stmt
  | CaseStmt of expr * (element list * stmt) list * stmt option
  | WhileStmt of (expr * stmt) list
  | RepeatStmt of stmt * expr
  | LoopStmt of stmt
  | ExitStmt
  | ForStmt of expr * expr * expr * expr * stmt * def option ref
  | WithStmt of (expr * name * stmt) list * stmt option
  | TypeCase of expr * (name * stmt) list * stmt option
  | Seq of stmt list
  | Skip
  | ErrStmt

and element = Single of expr | Range of expr * expr

and expr = 
  { mutable e_guts: expr_guts; 
    mutable e_type: otype;
    e_loc: location }		(* Position of main operator *)

and expr_guts =
    Name of name
  | Deref of expr
  | Sub of expr * expr 
  | Select of expr * name
  | Const of value * otype
  | String of symbol * int
  | Nil
  | FuncCall of expr * expr list
  | MethodCall of expr * name * expr list
  | Convert of expr
  | Monop of op * expr 
  | Binop of op * expr * expr
  | Set of element list
  | Cast of expr * name
  | TypeTest of expr * name
  | Decimal of string

and typexpr = 
  { tx_guts: type_guts;
    tx_loc: location }

and type_guts =
    TypeName of name 
  | Enum of name list
  | Pointer of typexpr
  | Array of expr * typexpr
  | Flex of typexpr
  | Record of bool * typexpr option * decl list
  | Proc of proc_heading

(* makeExpr -- construct an expr node with dummy annotations *)
val makeExpr : expr_guts * location -> expr

(* makeStmt -- construct a stmt node *)
val makeStmt : stmt_guts * location -> stmt

(* Typexpr -- construct a typexpr node *)
val makeTypexpr : type_guts * location -> typexpr

(* makeName -- construct a name node with dummy annotations *)
val makeName : ident * ident * location -> name
val makeDefId : ident * export * location -> name

val undefined : name -> bool

(* get_def -- extract definition from name or fail *)
val get_def : name -> def

(* get_name -- extract name from occurrence *)
val get_name : expr -> name

(* copy_expr -- copy an expr record *)
val copy_expr : expr -> expr

(* edit_expr -- overwrite root of expr *)
val edit_expr : expr -> expr_guts -> unit

(* is_name -- test if an expression is a name *)
val is_name : expr -> bool

(* is_string_const -- test if expression is a string constant *)
val is_string_const : expr -> bool

(* subscripts, sub_base -- analyse subscript expression *)
val subscripts : expr -> expr list
val sub_base : expr -> expr

(* safe -- test if an expression has no side effects or runtime errors *)
val safe : expr -> bool

(* QualId -- printf formatter for qualified names *)
val fQualId : name -> Print.arg

(* print_tree -- print tree as S-expression *)
val print_tree : out_channel -> program -> unit
val print_expr : out_channel -> expr -> unit
