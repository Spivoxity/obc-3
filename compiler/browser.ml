(*
 * browser.ml
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

open Print
open Symtab
open Symfile
open Eval
open Dict
open Config

let copyright = "Copyright (C) 2001 J. M. Spivey"

let source = ref stdin
let show_doc = ref false

let normalize s =
  let rec loop i j =
    if j >= Bytes.length s then
      Bytes.sub_string s 0 i
    else if Bytes.get s j = '\r' then
      loop i (j+1)
    else begin
      Bytes.set s i (Bytes.get s j);
      loop (i+1) (j+1)
    end in
  loop 0 0

let get_text a b =
  let pos0 = pos_in !source in
  try 
    let buf = Bytes.create (b-a) in
    seek_in !source a;
    really_input !source buf 0 (b-a);
    seek_in !source pos0;
    normalize buf
  with End_of_file -> 
    seek_in !source pos0;
    "*oops*" 

let fDoc =
  function
      Some (a, b) ->
	if !show_doc then fMeta "$\n" [fStr (get_text a b)] else fStr ""
    | None ->
	fStr ""

let rec split p =
  function
     [] -> ([], [])
   | (x::xs) ->
       if p x then
	 let (ys, zs) = split p xs in (x::ys, zs)
       else
	 ([], x::xs)

let fDef d = fId d.d_tag

let fExp1 x = fStr (if x = ReadOnly then "-" else "")

let rec fType t =
  let f prf = show_type prf t in fExt f

and show_type prf t =
  if t.t_name <> anon then begin
    if t.t_module <> !current && t.t_module <> anon then 
      prf "$." [fId t.t_module];
    prf "$" [fId t.t_name]
  end
  else if is_record t then
    prf "<record>" []
  else
    prf "$" [fType1 t]

and fType1 t = 
    match t.t_guts with
	PointerType d -> 
	  fMeta "POINTER TO $" [fType d.d_type]
      | ArrayType (n, t1) ->
	  fMeta "ARRAY $ OF $" [fNum n; fType t1]
      | FlexType t1 ->
	  fMeta "ARRAY OF $" [fType t1]
      | RecordType r ->
	  fExt (fun prf ->
	    prf "RECORD" [];
	    if not (same_types r.r_parent voidtype) then
	      prf " ($)" [fType r.r_parent];
	    prf "\n" [];
	    List.iter (show_field prf) r.r_fields;
	    List.iter (show_method prf) r.r_methods;
	    prf "    END" [])
      | ProcType p ->
	  fMeta "PROCEDURE $" [fHeading p.p_fparams p.p_result]
      | EnumType n ->
	  fMeta "($)" [fStr "<enum>"]
      | _ -> 
	  fStr "<bad type>"

and show_field prf d =
  if d.d_export <> Private then begin
    if d.d_comment <> None then prf "      $" [fDoc d.d_comment];
    prf "      $$: $;\n" [fId d.d_tag; fExp1 d.d_export; fType d.d_type]
  end

and show_method prf d =
  if d.d_export <> Private then begin
    let p = get_proc d.d_type in
    if d.d_comment <> None then prf "      $" [fDoc d.d_comment];
    prf "      PROCEDURE ($) $" [fParams [List.hd p.p_fparams]; fId d.d_tag];
    prf "$" [fHeading (List.tl p.p_fparams) p.p_result];
    prf ";\n" []
  end

and fParams fparams =
  let match_param d1 d2 = 
    d1.d_kind = d2.d_kind && same_types d1.d_type d2.d_type in
  let rec group = 
    function
        [] -> []
      | (d::ds) ->
	  let (ds1, ds2) = split (match_param d) ds in
	  (d.d_kind, d.d_type, d::ds1) :: group ds2 in
  let fGroup (k, t, ds) =
    fMeta "$$: $"
      [fStr (if k = VParamDef then "VAR " else "");
	fList(fDef) ds; fType t] in
  fMeta "($)" [fSeq(fGroup, "; ") (group fparams)]

and fHeading fparams result =
  if not (same_types result voidtype) then
    fMeta "$: $" [fParams fparams; fType result]
  else if fparams <> [] then
    fMeta "$" [fParams fparams]
  else 
    fStr ""

let bit k n = 
  (integer_bitand n (integer_lsl (integer 1) k) <> integer 0)

let rec segs k n =
  if k >= 32 then []
  else if bit k n then segs1 k (k+1) n
  else segs (k+1) n
and segs1 k0 k n =
  if k >= 32 then [(k0, 31)]
  else if bit k n then segs1 k0 (k+1) n
  else (k0, k-1) :: segs (k+1) n

let fSeg (lo, hi) =
  if lo = hi then fNum lo else fMeta "$..$" [fNum lo; fNum hi]

let fValExp t v =
  if same_types t character then
    fMeta "'$'" [fChr (char_of_integer (int_value v))]
  else if same_types t boolean then
    if int_value v <> integer 0 then fStr "TRUE" else fStr "FALSE"
  else if same_types t settype then
    fMeta "{$}" [fList(fSeg) (segs 0 (int_value v))]
  else
    fVal v

let print_def d =
  if d.d_export <> Private then begin
    match d.d_kind with
	ConstDef v ->
	  printf "$CONST $ = $;\n\n" 
	    [fDoc d.d_comment; fId d.d_tag; fValExp d.d_type v]
      | EnumDef _ -> ()
      | StringDef ->
	  printf "$CONST $ = <a string constant>;\n\n" 
	    [fDoc d.d_comment; fId d.d_tag]
      | VarDef ->
	  printf "$VAR $$: $;\n\n" 
	    [fDoc d.d_comment; fId d.d_tag; fExp1 d.d_export; fType d.d_type]
      | TypeDef ->
	  printf "$TYPE $ = $;\n\n" 
	    [fDoc d.d_comment; fId d.d_tag; fType1 d.d_type]
      | ProcDef ->
	  let p = get_proc d.d_type in
	  printf "$PROCEDURE $$;\n\n" 
	    [fDoc d.d_comment; fId d.d_tag; fHeading p.p_fparams p.p_result]
      | _ ->
	  printf "<unknown def>\n" []
  end

let print_version () =
  fprintf stderr "Oxford Oberon-2 browser version $ [build $]\n" 
    [fStr Config.version; fStr Revid.hash];
  exit 0

let spec =
  Arg.align
    [ "-I", Arg.String (function s -> libpath := !libpath @ [s]),
	"dir Search directory for imported modules";
      "-v", Arg.Unit print_version, "Print version and exit";
      "-d", Arg.Int (function n -> debug := n), "n Set debugging level" ]

let usage () = Arg.usage spec "Usage:"; exit 2

let main () =
  let fns = ref [] in
  Arg.parse spec (function s -> fns := !fns @ [s]) "Usage:";
  if List.length !fns <> 1 then usage ();
  let modname = List.hd !fns in
  current := intern modname;
  let symfile =  
    try Symfile.import !current with Not_found ->
      fprintf stderr "? The interface file for '$' cannot be found\n" 
	[fStr modname];
      exit 1 in

  begin try
    source := 
      open_in_bin (Util.search_path symfile.y_fname !Config.libpath);
    show_doc := true
  with Not_found ->
    fprintf stderr "? Can't find source file: omitting comments\n" [];
    flush stderr
  end;

  printf "$DEFINITION $;\n\n" [fDoc symfile.y_doc; fStr modname];
  List.iter print_def (top_block symfile.y_env);
  printf "END $.\n" [fStr modname]

let crash fmt args =
  fprintf stderr "*** Internal compiler error: $\n" [fMeta fmt args];
  fprintf stderr "*** (This message should never be displayed: it may\n" [];
  fprintf stderr "***   indicate a bug in the Oberon compiler.\n" [];
  fprintf stderr "***   Please save your program as evidence and report\n" [];
  fprintf stderr "***   the error to '$'.)\n" [fStr bugaddr];
  exit 3

let catch_failure f x =
  try f x with
      Failure s -> 
	crash "$" [fStr s]
    | Invalid_argument s ->
	crash "invalid argument to $" [fStr s]
    | Not_found ->
	crash "Not_found exception" []
    | Exit ->
	crash "Exit exception" []
    | x ->
	crash "Unknown exception\n***\t$" [fStr (Printexc.to_string x)]

let obb = catch_failure main (); exit 0

