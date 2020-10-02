(*
 * symfile.ml
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

open Tree
open Dict
open Mach
open Symtab
open Print
open Error
open Eval

(*

SYNTAX OF SYMBOL FILES

The symbol file (actually prepended to the file of object code) has a
very simple syntax with next to no recursion -- so that it could be
read by a very simple parser.

    file = heading { glodef } checksum

    heading = ( SYMFILE module version lnum doc ) 

    checksum = ( CHKSUM hex )

As well as global definitions, the file can contain commands to fix up
a pointer target or add a method to a record: both of these involve
cycles in the structure.  All methods for a record are included, so
that hidden methods may be inherited and appear in the vtables of
subtypes.

    glodef =
	( TYPE ident exmark doc type )
      | ( GLOBAL ident exmark doc label type )
      | ( CONST ident exmark doc type value )
      | ( ENUM ident exmark doc type int )
      | ( STRING ident exmark length label )
      | ( PROCEDURE ident exmark lnum doc label type { local } )
      | fixup

    fixup =
        ( TARGET type type )		; Set target of pointer type
      | ( METHOD type ident exmark lnum doc slot label type { local } )
					; Add method to record type
      | ( DEF typedef )			; Define type for future use

    value =
	integer				; Integer constant
      | float				; Floating-point constant

Each user-defined types is given a unique ID by a DEF or REF clause,
Subsequently, the type can be written as its integer ID.  A REF clause
defines the next ID to refer to the type numbered 'index' in 'module'.
To make symfiles clearer for a human reader, deep nesting is avoided
by defining types for record parents, parameters and return values
before they are used.  The symfile parser is able to cope with
arbitrary nesting, however.

    type =
	INTEGER | REAL | ...		; Built in types
      | = index				; Use previously defined type
      | ! index tguts			; Define type
      | ? index module index tname tguts  ; Refer to type from another module

    tname =
	ident
      | ANON

Cycles of recursion in types are broken at pointers, whose target is
given later by a TARGET command.

    tguts =
	( POINTER )			; Pointer type (fixed up later)
      | ( (RECORD|ABSREC) name size parent-type { field } )
      | ( ARRAY bound type )
      | ( FLEX type )
      | ( (PROC|METH|ABSMETH) pcount result-type { param } )

Fields may be exported read-only or read-write, but even hidden fields may be
included for use by debuggers.

    field =
	( FIELD ident exmark doc offset type )

    local =
	param
      | fixup
      | ( LOCAL ident exmark doc offset type )

Const parameters are converted to ordinary value parameters on output,
because they are the same as far as the caller is concerned: it's the
job of the procedure writer to avoid aliasing effects.

    param =
	( (PARAM|CPARAM|VPARAM) ident exmark doc offset type )

    exmark = 				; Export mark
	empty 				; Hidden
      | "-" 				; Read-only
      | "*"				; Visible

    doc =				; Docstring
	[ integer integer ]
      | empty
*)

(* Exporting *)

let ntypes = ref 0
let out_table = Hashtbl.create 100

let find_type t =
  Hashtbl.find out_table (t.t_module, t.t_id)

let fixups = ref []
let methods = ref []

let check = ref 0
let nl = ref true
let indent = ref 0

let xprintf fmt args = 
  let out ch =
    (* Poor man's version of MD5 *)
    check := 17 * !check + int_of_char ch;
    if !nl then begin 
      print_string "!! "; 
      for i = 1 to !indent do print_string "  " done;
      nl := false 
    end;
    print_char ch;
    match ch with
        '\n' -> nl := true
      | '(' -> incr indent
      | ')' -> decr indent
      | _ -> ()in
  do_print out fmt args

let fExm =
  function 
    Private -> fStr "" | ReadOnly -> fStr "-" | Visible -> fStr "*"

let fDoc =
  function
      Some (a, b) -> fMeta " [$ $]" [fNum a; fNum b]
    | None -> fStr ""

let fValu =
  function
      IntVal x -> fInteger x
    | FloVal x -> fStr (Util.float_as_string x)

let rec out_debug d =
  let nonparam d = match d.d_kind with
	ParamDef | VParamDef | CParamDef -> false | _ -> true in
  List.iter out_def (List.filter nonparam (top_block d.d_env))

and out_def d =
  prep_parents d.d_type;
  let def kind =
    xprintf "\n($ #$$$ $ $)" [fStr kind; fId d.d_tag; fExm d.d_export; 
	fDoc d.d_comment; fNum d.d_offset; fType d.d_type] in
  match d.d_kind with
      ConstDef x -> 
	xprintf "\n(CONST #$$$ $ $)" 
	  [fId d.d_tag; fExm d.d_export; fDoc d.d_comment; 
	    fType d.d_type; fValu x]
    | EnumDef n ->
	xprintf "\n(ENUM #$$$ $ $)"
	  [fId d.d_tag; fExm d.d_export; fDoc d.d_comment;
	    fType d.d_type; fNum n]
    | StringDef -> 
	xprintf "\n(STRING #$$$ $ #$)" 
	  [fId d.d_tag; fExm d.d_export; fDoc d.d_comment; 
	    fNum (bound d.d_type); fSym d.d_lab]
    | TypeDef -> 
	xprintf "\n(TYPE #$$$ $)" 
	  [fId d.d_tag; fExm d.d_export; fDoc d.d_comment; fType d.d_type]
    | VarDef -> 
	if d.d_level = 0 then
	  xprintf "\n(GLOBAL #$$$ #$ $)" 
	    [fId d.d_tag; fExm d.d_export; fDoc d.d_comment; 
	      fSym d.d_lab; fType d.d_type]
	else
	  def "LOCAL"
    | VParamDef -> def "VPARAM"
    | CParamDef -> def "CPARAM"
    | ParamDef -> def "PARAM"
    | FieldDef -> def "FIELD"
    | ProcDef -> 
	begin
	  let p = get_proc d.d_type in
	  match p.p_kind with
	      Procedure ->
		xprintf "\n(PROCEDURE #$$ $$ #$ $" 
		  [fId d.d_tag; fExm d.d_export; fNum d.d_line;
		    fDoc d.d_comment; fSym d.d_lab; fType d.d_type];
		if !Config.debug_info then out_debug d;
		xprintf ")" []
	    | Body ->
		if !Config.debug_info then
		  xprintf "\n(BODY #$ #$)" [fId d.d_tag; fSym d.d_lab];
	    | _ -> failwith "out_def"
	end
    | _ -> ()

(* fType is a formatter for types, but (to save a lot of clutter),
   it assumes that its argument prf is actually xprintf *)
and fType t = fExt (function prf -> out_type (* prf *) t)

and out_type t =
  if same_types t numtype then
      xprintf "INTCONST" []
    else if is_basic t then
      (* Allow SYSTEM.BYTE etc. *)
      xprintf "$" [fQual (t.t_module, t.t_name)]
    else
      try xprintf "=$" [fNum (find_type t)] with 
	Not_found -> def_type t

and def_type t =
  incr ntypes;
  let k = !ntypes in
  Hashtbl.add out_table (t.t_module, t.t_id) k;
  if t.t_module = !current then
    xprintf "!$ " [fNum k]
  else
    xprintf "?$ #$ $ $ " [fNum k; fId t.t_module; fNum t.t_id; 
      (if t.t_name = anon then fStr "ANON" 
	else fMeta "#$" [fId t.t_name])];
  match t.t_guts with
      PointerType d -> 
	xprintf "(POINTER)" []; 
	fixups := (k, d.d_type) :: !fixups
    | EnumType n -> 
	xprintf "(ENUM $)" [fNum n]
    | ArrayType (n, t1) -> 
	xprintf "(ARRAY $ $)" [fNum n; fType t1]
    | FlexType t1 -> 
	xprintf "(FLEX $)" [fType t1]
    | RecordType r -> 
	xprintf "(RECORD #$ $ $" 
	  [fSym t.t_desc; fNum t.t_rep.m_size; fType r.r_parent]; 
	(* We need to list all fields in case they contain pointers
	   that must be included in the pointer maps of subtypes. *)
	List.iter out_def r.r_fields;
	xprintf ")" [];
	(* All methods must be listed, to allow hidden methods to
	   be inherited *)
	methods := !methods @ List.map (function d -> (k, d)) r.r_methods
    | ProcType p -> 
	let kw = match p.p_kind with Procedure -> "PROC" | Method -> "METH" 
				| AbsMeth -> "ABSMETH" | Body -> "?BODY?" in
	xprintf "($ $ $" [fStr kw; fNum p.p_pcount; fType p.p_result]; 
	List.iter out_def p.p_fparams;
	xprintf ")" []
    | _ -> failwith "<unknown type>"

and prep_parents t =
  match t.t_guts with
      ArrayType (n, t1) -> prep_parents t1
    | FlexType t1 -> prep_parents t1
    | RecordType r ->
	prep_type r.r_parent;
	List.iter 
	  (function d -> 
	    if d.d_export <> Private then prep_parents d.d_type) 
	  r.r_fields
    | ProcType p ->
	List.iter (function d -> prep_type d.d_type) p.p_fparams;
	prep_type p.p_result
    | _ -> ()

and prep_type t = 
  if not (is_basic t) then begin
    try ignore (find_type t) with
      Not_found ->
        xprintf "\n(DEF " []; ignore (def_type t); xprintf ")" []
  end

let do_fixups () =
  while !fixups <> [] || !methods <> [] do
    if !fixups <> [] then begin
      let (k, t) = List.hd !fixups in
      fixups := List.tl !fixups;
      prep_parents t;
      xprintf "\n(TARGET =$ $)" [fNum k; fType t]; 
    end 
    else if !methods <> [] then begin
      let (k, d) = List.hd !methods in
      methods := List.tl !methods;
      match d.d_kind with
	  ProcDef ->
	    prep_parents d.d_type;
	    xprintf "\n(METHOD =$ #$$ $$ $ #$ $" 
	      [fNum k; fId d.d_tag; fExm d.d_export; fNum d.d_line;
		fDoc d.d_comment; fNum d.d_offset; fSym d.d_lab; 
		fType d.d_type];
	    if !Config.debug_info then out_debug d;
	    xprintf ")" []
	| _ -> failwith "out_method"
    end
  done
    
let export m doc glodefs fname = 
  ntypes := 0; 
  Hashtbl.clear out_table;
  xprintf "(SYMFILE #$ $ #$ $$ #$)" 
    [fId m.d_tag; fHex Config.signature; fSym m.d_lab; 
      fNum m.d_line; fDoc doc; fStr fname];
  List.iter 
    (function d -> 
      if !Config.debug_info || d.d_export <> Private then out_def d; 
      do_fixups ()) 
    glodefs;
  let chksum = !check land 0x7fffffff in
  xprintf "\n(CHKSUM $)\n\n" [fHex chksum];
  chksum


(* Importing *)

let import name = 
  let fname = Util.search_path (extern name ^ ".k") !Config.libpath in
  let chan = open_in fname in
  let lexbuf = Lexing.from_channel chan in
  Symlex.line := 1;
  let symfile = 
    try Symparse.file Symlex.token lexbuf 
    with Yyparse.Parse_error ->
      failwith (sprintf "symfile syntax at $ (line $ in $)" 
	[fStr (Lexing.lexeme lexbuf); fNum !Symlex.line; fStr fname]) in
  close_in chan;
  symfile

