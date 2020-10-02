(*
 * dict.mli
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
open Eval
open Mach
open Gcmap

(*
This module defines the types that represent definitions and Oberon
types in the semantic analyser, and provides an abstract data type of
'environments' used in the analyser.  There are also some functions that
operate on types.
*)

(* environment -- abstract type of environments *)
type environment

(* def_kind -- kinds of definition *)
type def_kind = 
    ConstDef of value 		(* Constant (value) *)
  | EnumDef of int		(* Enum constant (value) *)
  | StringDef 			(* String *)
  | TypeDef 			(* Type *)
  | VarDef			(* Variable *)
  | ParamDef 			(* Value parameter *)
  | CParamDef			(* Const parameter *)
  | VParamDef 			(* Var parameter *)
  | FieldDef 			(* Field of record *)
  | ProcDef			(* Procedure *)
  | PrimDef			(* Built-in primitive *)
  | ModDef of ident * environment  (* Imported module *)

type export = Private | ReadOnly | Visible

type docstring = Error.location option

(* def -- definitions in environment *)
type def = 
  { d_tag: ident; 		(* Name *)
    d_module: ident;		(* Module *)
    d_kind: def_kind; 		(* Kind of object *)
    mutable d_type: otype;	(* Type *)
    d_export: export;		(* Whether exported *)
    d_loc: Error.location;	(* Location of defining occurrence *)
    d_line: int;		(* Line number of def. occ. *)
    mutable d_used: bool;	(* Whether used *)
    mutable d_lab: symbol;	(* Label for global objects *)
    d_level: int;		(* Static level *)
    mutable d_offset: int;	(* Offset from frame base *)
    mutable d_param: int;	(* Offset of original parameter *)
    mutable d_env: environment;	(* Local env of procedure *)
    d_comment: docstring;	(* Location of doc comment *)
    mutable d_map: gcmap }	(* Pointer map *)

(* otype -- Oberon types *)
and otype = 
  { t_id: int;			(* Serial number *)
    mutable t_name: ident;	(* Name *)
    mutable t_module: ident;	(* Defining module *)
    t_level: int;		(* Lexical level *)
    t_guts: type_guts;		(* The type itself *)
    t_rep: Mach.metrics;	(* Representation in object program *)
    mutable t_desc: symbol;	(* Descriptor label *)
    t_map: gcmap }		(* Pointer map *)

and type_guts = 
    BasicType of kind		(* Basic type *)
  | EnumType of int		(* Enumeration type (nelems) *)
  | PointerType of def 		(* Pointer type (target) *)
  | ArrayType of int * otype	(* Array (bound, base type) *)
  | FlexType of otype		(* Open array *)
  | RecordType of rec_data 	(* Record *)
  | ProcType of proc_data	(* Procedure *)
  | BuiltinType of builtin	(* Built-in procedure *)

(* rec_data -- data about a record type *)
and rec_data =
  { r_depth: int;		(* Extension depth *)
    r_abstract: bool;		(* Whether abstract *)
    r_parent: otype;		(* Parent type (voidtype if none) *)
    r_loc: Error.location;      (* Location of type expr *)
    r_fields: def list;		(* List of fields *)
    mutable r_methods: def list	} (* List of methods *)

(* proc_data -- data about a procedure type *)
and proc_data =
  { p_kind: proc_kind;		(* Procedure or method *)
    p_fparams: def list;	(* Formal parameters *)
    p_pcount: int;		(* Total size of parameters in words *)
    p_result: otype }		(* Result type *)

and proc_kind = Procedure | Method | AbsMeth | Body

(* builtin -- data about a built-in procedure *)
and builtin =
  { b_name: string;
    b_id: libid;
    b_nargs: int;
    b_argtypes: otype list }

(* symfile -- info from an imported module *)
type symfile =
  { y_env: environment;
    y_checksum: int;
    y_doc: docstring;
    y_fname: string }

val numtype : otype
val bytetype : otype
val shortint : otype
val inttype : otype
val longint : otype
val character : otype
val boolean : otype
val voidtype : otype
val realtype : otype
val longreal: otype
val settype: otype
val niltype : otype
val ptrtype : otype
val longptr : otype
val sysbyte : otype
val errtype : otype
val strtype : otype
val bodytype : otype

(* basic_types -- types represented by their own name in symtab files *)
val basic_types: otype list

(* new_block -- add a new top block *)
val new_block : environment -> environment

(* add_block -- add an existing def list as top block *)
val add_block : def list -> environment -> environment

(* top_block -- return top block as a def list *)
val top_block : environment -> def list

(* pop_block -- remove top block *)
val pop_block : environment -> environment

(* define -- add a definition, raise Exit if already declared *)
val define : environment -> def -> unit

(* find_def -- search a def list or raise Not_found *)
val find_def : ident -> def list -> def

(* lookup -- search an environment or raise Not_found *)
val lookup : ident -> environment -> def

(* empty_env -- empty environment *)
val empty_env : environment

val fEnv : environment -> Print.arg


(* Functions on types *)

type typespec = type_guts * metrics * gcmap

(* new_type -- construct a new type *)
val new_type : int -> typespec -> otype

(* pointer -- make a pointer type *)
val pointer : def -> typespec

(* row -- make an array type *)
val row : int -> otype -> typespec

(* flex -- make an open array type *)
val flex : otype -> typespec

(* record -- make a record type *)
val record : bool -> otype -> Error.location -> int -> def list -> typespec

(* proctype -- make a procedure type *)
val proctype : proc_data -> typespec

(* bound -- get bound of array type *)
val bound : otype -> int

(* base_type -- get base type of pointer or array *)
val base_type : otype -> otype

(* get_record -- get data from record or record pointer type *)
val get_record : otype -> rec_data

(* ancestors -- get list of ancestors, self included *)
val ancestors : otype -> otype list

(* get_proc -- get data from procedure type *)
val get_proc : otype -> proc_data

(* make_desc -- register a type as needing a descriptor *)
val make_desc : otype -> unit

(* desc_table -- list of all types needing descritors *)
val desc_table : unit -> otype list


(* Type tests *)

(* numeric -- test if a type is numeric *)
val numeric : otype -> bool

(* integral -- test if a type is integral *)
val integral : otype -> bool

(* floating -- test for real types *)
val floating : otype -> bool

(* kind_of -- get kind code for basic type *)
val kind_of : otype -> kind

(* join_type -- lower bound for two numeric types *)
val join_type : otype -> otype -> otype

(* is_pointer -- test if a type is 'pointer to ...' *)
val is_pointer : otype -> bool

(* is_niltype -- test for the type of NIL *)
val is_niltype : otype -> bool

(* is_address -- test for types that are represented by addresses *)
val is_address : otype -> bool

(* scalar -- test if a type is not a record or array type *)
val scalar : otype -> bool

(* is_discrete -- test if a type suits FOR, CASE, etc. *)
val is_discrete : otype -> bool

(* is_enum -- test if a type is an enumerated type *)
val is_enum : otype -> bool

(* is_string -- test if a type is 'array N of char' *)
val is_string : otype -> bool

(* is_proc -- test if a type is 'proc ...' *)
val is_proc : otype -> bool

(* is_array -- test if a type is an array or open array *)
val is_array : otype -> bool

(* is_record -- test if a type is a record *)
val is_record : otype -> bool

(* is_abstract -- test if a type is an abstract record *)
val is_abstract : otype -> bool

(* is_flex -- test if a type is a flexible array *)
val is_flex : otype -> bool

(* flexity -- count flexible dimensions *)
val flexity : otype -> int

val flex_base : otype -> otype

val is_basic : otype -> bool

(* is_errtype -- test if a type is the fictitious errtype *)
val is_errtype : otype -> bool

(* The test |same_types| uses name equivalence; |equal_types| is used
   to match the parameter lists of procedures, and treats open arrays and
   procedures structurally. *)

(* same_types -- test if two types are 'the same' *)
val same_types : otype -> otype -> bool
  
(* array_match -- test if type Ta is 'array compatible' with type Tf *)
val array_match : otype -> otype -> bool

(* proc_match -- test if procedure types match *)
val proc_match : otype -> otype -> bool

(* approx_same -- test if two types are similar enough to cause confusion *)
val approx_same : otype -> otype -> bool

(* subtype -- test if one type is a subtype of another *)
val subtype : otype -> otype -> bool

(* match_args -- test two formal parameter lists for congruence *)
val match_args : def list -> def list -> bool

(* fOType -- format type for output *)
val fOType : otype -> Print.arg


(* Useful allocation stuff *)

val align : int -> int ref -> unit
val local_map : def list -> gcmap


(* Initial environment *)

val init_env : unit -> environment
val sysenv : unit -> environment


(* Debugger hooks *)

val debugger : (def -> unit) ref
