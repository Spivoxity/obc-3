(*
 * In.m
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

(** Simple input from the standard input *)
MODULE In;

IMPORT Files, Conv;

(* On Linux, a flush of stdout happens automatically on input from
   stdin, if both are connected to the terminal.  I wonder if this is
   true for other Unixes too? *)

(** Done -- indicates whether the last operation succeeded *)
VAR Done-: BOOLEAN;

(** Open -- open or reopen the standard input *)
PROCEDURE Open*;
BEGIN
  Done := TRUE
END Open;

PROCEDURE IsDigit(c: CHAR): BOOLEAN;
BEGIN
  RETURN (c >= '0') & (c <= '9')
END IsDigit;

PROCEDURE IsSpace(c: CHAR): BOOLEAN;
BEGIN
  RETURN (c = ' ') OR (c = 09X) OR (c = 0AX)
END IsSpace;

(** Char -- input a character *)
PROCEDURE Char*(VAR c: CHAR);
BEGIN
  IF ~Done THEN RETURN END;
  IF Files.Eof(Files.stdin) THEN
    Done := FALSE
  ELSE
    Files.ReadChar(Files.stdin, c)
  END
END Char;

PROCEDURE ScanInt(VAR buf: ARRAY OF CHAR);
  VAR c: CHAR; i: INTEGER;
BEGIN
  i := 0; Char(c);
  WHILE Done & IsSpace(c) DO Char(c) END;
  IF c = '-' THEN buf[i] := c; INC(i); Char(c) END;
  IF ~IsDigit(c) THEN Done := FALSE; RETURN END;
  WHILE Done & IsDigit(c) DO
    buf[i] := c; INC(i); Char(c)
  END;
  buf[i] := 0X;
END ScanInt;

(** Int -- input an integer *)
PROCEDURE Int*(VAR n: INTEGER);
  VAR buf: ARRAY 16 OF CHAR; 
BEGIN
  IF ~Done THEN RETURN END;
  ScanInt(buf);
  n := Conv.IntVal(buf)
END Int;

PROCEDURE LongInt*(VAR n: LONGINT);
  VAR buf: ARRAY 32 OF CHAR;
BEGIN
  IF ~Done THEN RETURN END;
  ScanInt(buf);
  n := Conv.LongIntVal(buf)
END LongInt;

PROCEDURE ScanReal(VAR buf: ARRAY OF CHAR);
  VAR c: CHAR; i: INTEGER;
BEGIN
  i := 0; Char(c);
  WHILE Done & IsSpace(c) DO Char(c) END;
  IF c = '-' THEN buf[i] := c; INC(i); Char(c) END;
  IF (c # '.') & ~IsDigit(c) THEN Done := FALSE; RETURN END;
  WHILE Done & IsDigit(c) DO buf[i] := c; INC(i); Char(c) END;
  IF c = '.' THEN 
    buf[i] := c; INC(i); Char(c);
    WHILE Done & IsDigit(c) DO buf[i] := c; INC(i); Char(c) END
  END;
  buf[i] := 0X
END ScanReal;

(** Real -- input a real number *)
PROCEDURE Real*(VAR x: REAL);
  VAR buf: ARRAY 64 OF CHAR; 
BEGIN
  IF ~Done THEN RETURN END;
  ScanReal(buf);
  x := Conv.RealVal(buf)
END Real;

(** LongReal -- input a long real *)
PROCEDURE LongReal*(VAR x: LONGREAL);
  VAR buf: ARRAY 64 OF CHAR;
BEGIN
  IF ~Done THEN RETURN END;
  ScanReal(buf);
  x := Conv.LongRealVal(buf)
END LongReal;

(** Line -- input a line as a string *)
PROCEDURE Line*(VAR s: ARRAY OF CHAR);
  VAR i: INTEGER; c: CHAR;
BEGIN
  IF ~Done THEN RETURN END;
  i := 0;
  LOOP
    IF i >= LEN(s)-1 THEN EXIT END;
    Char(c);
    IF ~Done OR (c = 0AX) THEN EXIT END;
    s[i] := c;
    INC(i)
  END;
  s[i] := 0X
END Line;

BEGIN
  Done := TRUE
END In.
