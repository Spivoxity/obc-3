(*
 * Out.m
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

(** Output on the standard output *)
MODULE Out;

IMPORT Files;

(** Int -- output an integer with specified width *)
PROCEDURE Int*(n: INTEGER; w: INTEGER); 
BEGIN 
  Files.WriteInt(Files.stdout, n, w) 
END Int;

(** LongInt -- output a long integer with specified width *)
PROCEDURE LongInt*(n: LONGINT; w: INTEGER);
BEGIN
  Files.WriteLongInt(Files.stdout, n, w)
END LongInt;

(** Real -- output a real number in scientific notation *)
PROCEDURE Real*(x: REAL);
BEGIN
  Files.WriteReal(Files.stdout, x)
END Real;

(** LongReal -- output a long real in scientific notation *)
PROCEDURE LongReal*(x: LONGREAL);
BEGIN
  Files.WriteLongReal(Files.stdout, x)
END LongReal;

(** Fixed -- output a long real in fixed decimal notation *)
PROCEDURE Fixed*(x: LONGREAL; width, dec: INTEGER);
BEGIN
  Files.WriteFixed(Files.stdout, x, width, dec)
END Fixed;

(** Char -- output a character *)
PROCEDURE Char*(c: CHAR);
BEGIN
  Files.WriteChar(Files.stdout, c)
END Char;

(** String -- output a null-terminated string *)
PROCEDURE String*(CONST s: ARRAY OF CHAR);
BEGIN
  Files.WriteString(Files.stdout, s)
END String;

(** Ln -- output a newline *)
PROCEDURE Ln*;
BEGIN
  Files.WriteLn(Files.stdout)
END Ln;

END Out.
