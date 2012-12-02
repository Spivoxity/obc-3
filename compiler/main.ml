(*
 * main.ml
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
open Tree

let copyright = "Copyright (C) 1999 J. M. Spivey"

let error_token lexbuf =
  let tok = Lexing.lexeme lexbuf in
  if tok.[0] = '\000' then "EOF" else tok

let export (Module (m, _, _, defs, doc)) =
  Symfile.export (get_def m) doc !defs

let compiler base_name in_file =
  Symtab.current := Symtab.intern base_name;
  Lexer.init ();
  let chan = try open_in_bin in_file 
    with Sys_error s -> fprintf stderr "$\n" [fStr s]; exit 1 in
  let lexbuf = Lexing.from_channel chan in
  Error.init_errors in_file chan lexbuf;
  let prog = 
    try Parser.program Lexer.token lexbuf with
      Yyparse.Parse_error -> exit 1 in
  if !Error.err_count > 0 then exit 1;
  Check.annotate prog;
  if !Error.err_count > 0 then exit 1;
  if !Config.debug > 0 then print_tree stdout prog;
  let stamp = export prog in
  Igen.translate stamp prog

let crash fmt args =
  fprintf stderr "*** Oxford Oberon-2 compiler version $\n" 
    [fStr Config.version];
  fprintf stderr "*** Internal compiler error: $\n" [fMeta fmt args];
  fprintf stderr "*** (This message should never be displayed: it may\n" [];
  fprintf stderr "***   indicate a bug in the Oberon compiler.\n" [];
  fprintf stderr "***   Please save your program as evidence and report\n" [];
  fprintf stderr "***   the error to '$'.)\n" [fStr Config.bugaddr];
  exit 3

let catch_failure f x =
  try f x with
      Failure s -> 
	crash "$" [fStr s]
    | Expr_failure (s, e) ->
	print_expr stderr e;
	crash "$" [fStr s]
    | Invalid_argument s ->
	crash "invalid argument to '$'" [fStr s]
    | x ->
	crash "exception '$'" [fStr (Printexc.to_string x)]

let print_version () =
  fprintf stderr "Oxford Oberon-2 compiler version $\n" [fStr Config.version];
  exit 0

let spec =
  Arg.align [ 
    "-O0", Arg.Clear Config.optflag, " Turn off peephole optimiser";
    "-O", Arg.Set Config.optflag, " Turn on peephole optimiser";
    "-b", Arg.Clear Config.boundchk, " Disable runtime checks";
    "-g", Arg.Set Config.debug_info, " Output debugging symbols";
    "-v", Arg.Unit print_version, " Print version and exit";
    "-w", Arg.Clear Config.warnings, " Turn off warnings";
    "-x", Arg.Set Config.extensions, " Enable language extensions";
    "-pl", Arg.Set Config.linecount, 
      " Output line numbers for profiling or debugging";
    "-I", Arg.String (function s -> Config.libpath := !Config.libpath @ [s]),
      "dir Add dir as search directory for imported modules";
    "-d", Arg.Int Config.set_debug, "n Set debugging level to n";
    "-rsb", Arg.Set Config.lcflag, " Keywords and built-ins in lower case" ]

let usage () = Arg.usage spec "Usage:"; exit 2

let obc = 
  let fns = ref [] in
  Arg.parse spec (function s -> fns := !fns @ [s]) "Usage:";
  if List.length !fns <> 1 then usage ();
  let in_file = List.hd !fns in
  let base = Filename.basename in_file in
  let base_name =
    if Filename.check_suffix in_file ".m" then 
      Filename.chop_suffix base ".m"
    else if Filename.check_suffix in_file ".mod" then 
      Filename.chop_suffix base ".mod"
    else
      usage () in
  if !Config.debug = 0 then
    catch_failure (compiler base_name) in_file
  else
    compiler base_name in_file;

  exit 0
