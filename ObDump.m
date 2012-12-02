(*
 * ObDump.m
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

MODULE ObDump;

(* This program reads the file of line counts output by "obprof -l"
   and prints an annotated listing of the source file. *)

IMPORT Files, Out, Conv, Err, Strings, Args;

(* Line counts are kept in a hash table indexed by (module, line). *)

CONST HSIZE = 1024;

TYPE
  modname = ARRAY 20 OF CHAR;
  blob = POINTER TO blobrec;
  blobrec =
    RECORD
      module: modname;
      line: INTEGER;
      count: INTEGER;
      link: blob
    END;

VAR
  htable: ARRAY HSIZE OF blob;

(* Find -- find or create a hash table entry for (module, line) *)
PROCEDURE Find(VAR module: ARRAY OF CHAR; 
			line: INTEGER; create: BOOLEAN): blob;
  VAR h, i: INTEGER; p: blob;
BEGIN
  (* Compute the hash function *)
  h := 0; i := 0;
  WHILE module[i] # 0X DO 
    h := 5 * h + ORD(module[i]);
    i := i+1
  END;
  h := (h + line) MOD HSIZE;

  p := htable[h];
  WHILE (p # NIL) & ((p.module # module) OR (p.line # line)) DO
    p := p.link
  END;

  IF (p = NIL) & create THEN
    NEW(p);
    COPY(module, p.module);
    p.line := line;
    p.count := 0;
    p.link := htable[h];
    htable[h] := p
  END;

  RETURN p
END Find;
  
VAR
  field: ARRAY 10 OF ARRAY 20 OF CHAR;

(* ReadRec -- read a record from "obprof.out" into the field array *)
PROCEDURE ReadRec(f: Files.File): INTEGER;
  VAR c: CHAR; n, i: INTEGER;
BEGIN
  IF Files.Eof(f) THEN RETURN -1 END;

  n := 0; i := 0;
  LOOP
    Files.ReadChar(f, c);
    IF (c = 0AX) OR (c = ' ') THEN
      field[n][i] := 0X;
      n := n+1; i := 0;
      IF c = 0AX THEN EXIT END
    END;
    field[n][i] := c;
    i := i+1
  END;

  RETURN n
END ReadRec;

(* LoadProf -- load profiling data from "obprof.out" *)
PROCEDURE LoadProf;
  VAR 
    f: Files.File;
    p: blob;
    count: INTEGER;
BEGIN
  f := Files.Open("obprof.out", "r");
  IF f = NIL THEN
    Err.String("obdump: can't read obprof.out"); Err.Ln;
    HALT(1)
  END;

  WHILE ReadRec(f) >= 0 DO
    count := Conv.IntVal(field[2]);
    IF count > 0 THEN
      p := Find(field[0], Conv.IntVal(field[1]), TRUE);
      p.count := count
    END
  END;

  Files.Close(f)
END LoadProf;
    
VAR lbuf: ARRAY 256 OF CHAR;

(* ReadLine -- read a source line into lbuf *)
PROCEDURE ReadLine(f: Files.File): BOOLEAN;
  VAR c: CHAR; i: INTEGER;
BEGIN
  i := 0;
  LOOP
    IF Files.Eof(f) THEN RETURN FALSE END;
    Files.ReadChar(f, c);
    IF c = 0AX THEN EXIT END;
    lbuf[i] := c; i := i+1
  END;

  lbuf[i] := 0X;
  RETURN TRUE
END ReadLine;

(* Truncate -- extract the module name from the name of a source file *)
PROCEDURE Truncate(VAR name: ARRAY OF CHAR);
  VAR i, j: INTEGER;
BEGIN
  i := Strings.Length(name) - 1;
  WHILE (i >= 0) & (name[i] # '.') & (name[i] # '/') DO i := i-1 END;
  IF (i >= 0) & (name[i] = '.') THEN name[i] := 0X END;

  WHILE (i >= 0) & (name[i] # '/') DO i := i-1 END;
  IF i >= 0 THEN
    i := i+1; j := 0;
    WHILE name[i+j] # 0X DO name[j] := name[i+j]; j := j+1 END;
    name[j] := 0X
  END
END Truncate;

(* Dump -- format a source file with line counts *)
PROCEDURE Dump(fname: ARRAY OF CHAR);
  VAR 
    n: INTEGER;
    p: blob;
    f: Files.File;
BEGIN
  f := Files.Open(fname, "r");
  IF f = NIL THEN
    Err.String("obdump: can't read "); Err.String(fname); Err.Ln;
    HALT(1)
  END;

  Truncate(fname);

  n := 0;
  WHILE ReadLine(f) DO
    n := n+1;
    p := Find(fname, n, FALSE);
    IF p # NIL THEN
      Out.Int(p.count, 7)
    ELSE
      Out.String("       ")
    END;
    Out.Char('|');
    Out.String(lbuf);
    Out.Ln
  END
END Dump;

BEGIN
  VAR 
    i: INTEGER;
    abuf: ARRAY 80 OF CHAR;
  BEGIN
    LoadProf;
    FOR i := 1 TO Args.argc-1 DO
      Args.GetArg(i, abuf);
      Dump(abuf)
    END
  END
END ObDump.    
