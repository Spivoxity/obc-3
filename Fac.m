(*
 * Fac.m
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

MODULE Fac;

IMPORT Args, Conv, Out, Files, GC, Math;

VAR n: SHORTINT;
  buf: ARRAY 10 OF CHAR;
  out: Files.File;

PROCEDURE ^ Fac(n: INTEGER): LONGINT;

PROCEDURE byte(c: CHAR);
  VAR x: CHAR;
BEGIN
  x := c;
  IF x # 'A' THEN
    Out.String("Oops! There's a problem with byte sex!"); Out.Ln;
    HALT(2)
  END
END byte;

PROCEDURE Fac(n: INTEGER; f: LONGINT): LONGINT;
BEGIN
  IF n = 0 THEN 
    RETURN f 
  ELSE 
    RETURN Fac(n-1, n*f)
  END
END Fac;

BEGIN
  Args.GetArg(1, buf);
  n := SHORT(Conv.IntVal(buf));

  byte('A');

  Out.Char(CAP('t')); Out.String("he factorial of ");
  out := Files.stdout; Files.WriteInt(out, n, 0); 
  Out.String(" is "); Out.LongInt(Fac(n, 1), 0); Out.Ln;

  GC.Collect;

  ASSERT(ABS(Math.Sin(Math.pi / 6.0) - 0.5) < 1.0E-6);

  ASSERT(GC.HeapSize() > 0);
END Fac.
