/*
 * symparse.mly
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
 */

%{
open Dict
open Symtab
open Error
open Print
open Mach
open Eval
%}

%token 			ARRAY CONST PARAM DEF END ENUM FIELD TARGET
%token 			FLEX METH METHOD POINTER PROC PROCEDURE RECORD USE
%token 			STRING TYPE GLOBAL VOID VPARAM SYMFILE ANON CPARAM
%token			LOCAL ABSREC ABSMETH
%token			BRA KET SEMI QUERY PLING
%token<string> 		TAG NUM HEX FLO
%token<Dict.otype>	BASICTYPE
%token<Dict.export> 	MARK

%start <Dict.environment * int * Dict.docstring> file

%{
let modname = ref anon
let desc = ref nosym
let env = ref empty_env
let level = ref 0

let make_def x v k t ln doc =
  { d_tag = x; d_module = !modname; d_export = v; d_kind = k; 
    d_used = true; d_loc = no_loc; d_line = ln; d_type = t; d_lab = nosym;
    d_level = !level; d_offset = 0; d_param = 0; d_comment = doc;  
    d_env = empty_env; d_map = [] }

let install d =
  try define !env d with
    Exit -> failwith (sprintf "multiple defs of $" [fId d.d_tag])

let unpack t =
  let p = get_proc t in
  incr level; env := add_block !env p.p_fparams

let debug d = !Dict.debugger d

let in_table = Growvect.create 100

let def_type k m n (g, r, map) =
  let t = { t_id = n; t_name = anon; t_module = m; t_level = 0; 
		t_guts = g; t_rep = r; t_desc = nosym; t_map = map } in
  if is_record t then begin
    t.t_desc <- !desc;
    let d = make_def anon Private TypeDef t 0 None in
    d.d_lab <- !desc; debug d
  end;

  while Growvect.size in_table <= k do
    Growvect.append in_table errtype
  done;
  Growvect.set in_table k t; t

let use_type k =
  Growvect.get in_table k
%}

%%

file :
    header doc defs END HEX	{ (!env, int_of_string $HEX, $doc) } ;

header :
    SYMFILE ident HEX symbol int
      { if int_of_string $HEX <> Config.signature then begin
	  sem_error 
	    "symbol table for '$' is from wrong version of compiler"
	    [fId $ident] no_loc;
	  exit 2
	end;
	modname := $ident; env := new_block empty_env; level := 0;
        let d = 
	  make_def (intern "*body*") Private ProcDef bodytype $int None in
	d.d_lab <- $symbol; debug d } ;

defs :
    /* EMPTY */			{ () }
  | defs def			{ () } ;

def :
    TYPE ident mark doc otype	
      { if $otype.t_name = anon then $otype.t_name <- $ident;
	install (make_def $ident $mark TypeDef $otype 0 $doc) }
  | GLOBAL ident mark doc symbol otype	
      { let d = make_def $ident $mark VarDef $otype 0 $doc in
	d.d_lab <- $symbol; debug d; install d }
  | CONST ident mark doc otype const 
      { install (make_def $ident $mark (ConstDef $const) $otype 0 $doc) }
  | STRING ident mark doc int symbol 
      { let d = make_def $ident $mark StringDef
		  (new_type 0 (row $int character)) 0 $doc in
	d.d_lab <- $symbol; install d }
  | kind ident mark doc int otype	
      { let d = make_def $ident $mark $kind $otype 0 $doc in
	d.d_offset <- $int; install d }
  | procdecl defs SEMI		
      { let d = $procdecl in 
	d.d_env <- !env; 
	decr level; env := pop_block !env }
  | TARGET int otype	        
      { let t0 = use_type $int in
	match t0.t_guts with
	    PointerType d -> d.d_type <- $otype
	  | _ -> failwith "TARGET" }
  | DEF typedef			{ () } ;

procdecl :
    PROCEDURE ident mark int doc symbol otype
      { let d = make_def $ident $mark ProcDef $otype $int $doc in
	d.d_lab <- $symbol;
	debug d; install d; unpack $otype; d }
  | METHOD int.1 ident mark int.3 doc int.2 symbol otype
      { let t0 = use_type $int.1 in
	let r = get_record t0 in
	let d = make_def $ident $mark ProcDef $otype $int.3 $doc in
	d.d_offset <- $int.2; d.d_lab <- $symbol;
	r.r_methods <- r.r_methods @ [d]; 
	debug d; unpack $otype; d } ;

kind :
    LOCAL			{ VarDef }
  | PARAM			{ ParamDef }
  | CPARAM			{ CParamDef }
  | VPARAM			{ VParamDef } 
  | FIELD			{ FieldDef } ;

mark :
    /* EMPTY */			{ Private }
  | MARK			{ $MARK } ;

otype :
    int				{ use_type $int }
  | BASICTYPE			{ $BASICTYPE } 
  | typedef			{ $typedef } ;

typedef :
    PLING int tguts		
      { def_type $int !modname $int $tguts }
  | QUERY int.1 ident int.2 tname tguts	
      { let t = def_type $int.1 $ident $int.2 $tguts in
	t.t_name <- $tname; t } ;

tguts :
    POINTER			
      { (PointerType (make_def anon Private TypeDef voidtype 0 None), 
	  addr_rep, [GC_Offset 0]) }
  | ENUM int			
      { (EnumType $int, int_rep, []) }
  | recflag symbol int otype push defs SEMI
      { desc := $symbol; 
	let t = record $recflag $otype $int (top_block !env) in
	decr level; env := pop_block !env; t }
  | ARRAY int otype		{ row $int $otype }
  | FLEX otype			{ flex $otype }
  | prockind int otype push defs SEMI
      { let p = { p_kind = $prockind; p_pcount = $int; p_result = $otype; 
	    p_fparams = top_block !env } in
        decr level; env := pop_block !env;
	(ProcType p, addr_rep, []) } ;

push :
    /* EMPTY */			{ incr level; env := new_block !env } ;

recflag :
    RECORD			{ false }
  | ABSREC			{ true } ;

tname :
    ident			{ $ident }
  | ANON			{ anon } ;

ident :
    TAG				{ intern $TAG } ;

symbol :
    TAG				{ $TAG } ;

const :
    NUM				{ IntVal (integer_of_string $NUM) }
  | FLO				{ FloVal (float_of_string $FLO) } ;

int :
    NUM				{ int_of_string $NUM } ;

prockind :
    PROC			{ Procedure }
  | METH			{ Method } 
  | ABSMETH			{ AbsMeth } ;

doc :
    BRA int.1 int.2 KET		{ Some ($int.1, $int.2) }
  | /* EMPTY */			{ None } ;
