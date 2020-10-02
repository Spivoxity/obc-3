/*
 * symparse.mly
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
 */

%{
open Dict
open Symtab
open Error
open Print
open Mach
open Eval
open Gcmap
%}

%token 			ARRAY CHKSUM CONST PARAM DEF END ENUM FIELD TARGET
%token 			FLEX METH METHOD POINTER PROC PROCEDURE RECORD USE
%token 			STRING TYPE GLOBAL VOID VPARAM SYMFILE ANON CPARAM
%token			LOCAL ABSREC ABSMETH
%token			LPAR RPAR BRA KET QUERY PLING EQUAL BADTOK
%token<string> 		TAG NUM HEX FLO
%token<Dict.otype>	BASICTYPE
%token<Dict.export> 	MARK

%start <Dict.symfile> file

%{
let modname = ref anon
let desc = ref nosym
let env = ref empty_env
let level = ref 0

let make_def x v k t ln doc =
  { d_tag = x; d_module = !modname; d_export = v; d_kind = k; 
    d_used = true; d_loc = no_loc; d_line = ln; d_type = t; d_lab = nosym;
    d_level = !level; d_offset = 0; d_param = 0; d_comment = doc;  
    d_env = empty_env; d_map = null_map }

let install d =
  try define !env d with
    Exit -> failwith (sprintf "multiple defs of $" [fId d.d_tag])

let unpack t =
  let p = get_proc t in
  incr level; env := add_block p.p_fparams !env 

let debug d = !Dict.debugger d

let in_table = Hashtbl.create 100

let def_type k m n (g, r, map) =
  let t = { t_id = n; t_name = anon; t_module = m; t_level = 0; 
		t_guts = g; t_rep = r; t_desc = nosym; t_map = map } in
  if is_record t then begin
    t.t_desc <- !desc;
    let d = make_def anon Private TypeDef t 0 None in
    d.d_lab <- !desc; debug d
  end;
  Hashtbl.add in_table k t; t

let use_type k = Hashtbl.find in_table k
%}

%%

file :
    header defs chksum
    { { y_env = !env; y_checksum = $chksum; y_doc = fst $header;
        y_fname = snd $header } } ;

header :
    LPAR SYMFILE modname version symbol@s1 int doc symbol@s2 RPAR
      { env := new_block empty_env; level := 0;
        let d = 
	  make_def (intern "*body*") Private ProcDef bodytype $int None in
	d.d_lab <- $s1; debug d;
        ($doc, $s2) } ;

modname :
    ident			{ modname := $ident } ;

version :
    HEX
      { if int_of_string $HEX <> Config.signature then begin
	  sem_error 
	    "symbol table for '$' is from wrong version of compiler"
	    [fId !modname] no_loc;
	  exit 2
	end } ;

chksum :
    LPAR CHKSUM HEX RPAR	{ int_of_string $HEX } ;

defs :
    /* EMPTY */			{ () }
  | defs def			{ () } ;

def :
    LPAR TYPE ident exmark doc otype RPAR
      { if $otype.t_name = anon then $otype.t_name <- $ident;
	install (make_def $ident $exmark TypeDef $otype 0 $doc) }
  | LPAR GLOBAL ident exmark doc symbol otype RPAR
      { let d = make_def $ident $exmark VarDef $otype 0 $doc in
	d.d_lab <- $symbol; debug d; install d }
  | LPAR CONST ident exmark doc otype const RPAR
      { install (make_def $ident $exmark (ConstDef $const) $otype 0 $doc) }
  | LPAR ENUM ident exmark doc otype int RPAR
      { install (make_def $ident $exmark (EnumDef $int) $otype 0 $doc) }
  | LPAR STRING ident exmark doc int symbol RPAR
      { let d = make_def $ident $exmark StringDef
		  (new_type 0 (row $int character)) 0 $doc in
	d.d_lab <- $symbol; install d }
  | LPAR kind ident exmark doc int otype RPAR
      { let d = make_def $ident $exmark $kind $otype 0 $doc in
	d.d_offset <- $int; install d }
  | LPAR procdecl defs RPAR
      { let d = $procdecl in 
	d.d_env <- !env; 
	decr level; env := pop_block !env }
  | LPAR TARGET otype@t0 otype@t1 RPAR
      { match $t0.t_guts with
	    PointerType d -> d.d_type <- $t1
	  | _ -> failwith "TARGET" }
  | LPAR DEF otype RPAR		{ () } ;

procdecl :
    PROCEDURE ident exmark int doc symbol otype
      { let d = make_def $ident $exmark ProcDef $otype $int $doc in
	d.d_lab <- $symbol;
	debug d; install d; unpack $otype; d }
  | METHOD otype@t0 ident exmark int@i3 doc int@i2 symbol otype@t1
      { let r = get_record $t0 in
	let d = make_def $ident $exmark ProcDef $t1 $i3 $doc in
	d.d_offset <- $i2; d.d_lab <- $symbol;
	r.r_methods <- r.r_methods @ [d]; 
	debug d; unpack $t1; d } ;

kind :
    LOCAL			{ VarDef }
  | PARAM			{ ParamDef }
  | CPARAM			{ CParamDef }
  | VPARAM			{ VParamDef } 
  | FIELD			{ FieldDef } ;

exmark :
    /* EMPTY */			{ Private }
  | MARK			{ $MARK } ;

otype :
    BASICTYPE			{ $BASICTYPE } 
  | EQUAL int			{ use_type $int }
  | PLING int tguts		
      { def_type $int !modname $int $tguts }
  | QUERY int@i1 ident int@i2 tname tguts	
      { let t = def_type $i1 $ident $i2 $tguts in
	t.t_name <- $tname; t } ;

tguts :
    LPAR POINTER RPAR
      { pointer (make_def anon Private TypeDef voidtype 0 None) }
  | LPAR ENUM int RPAR
      { (EnumType $int, int_rep, null_map) }
  | LPAR recflag symbol int otype push defs RPAR
      { desc := $symbol; 
	let t = record $recflag $otype no_loc $int (top_block !env) in
	decr level; env := pop_block !env; t }
  | LPAR ARRAY int otype RPAR	{ row $int $otype }
  | LPAR FLEX otype RPAR	{ flex $otype }
  | LPAR prockind int otype push defs RPAR
      { let p = { p_kind = $prockind; p_pcount = $int; p_result = $otype; 
	    p_fparams = top_block !env } in
        decr level; env := pop_block !env;
	proctype p } ;

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
     BRA int@i1 int@i2 KET	{ Some ($i1, $i2) }
  | /* EMPTY */			{ None } ;
