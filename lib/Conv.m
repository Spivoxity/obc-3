(*
 * Conv.m
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

MODULE Conv;

(** IntVal -- convert string to integer *)
PROCEDURE IntVal*(CONST s: ARRAY OF CHAR): INTEGER IS "Conv_IntVal";
(* CODE ob_res.i = atoi((char * ) args[0].x); *)

(** LongIntVal -- convert string to long integer *)
PROCEDURE LongIntVal*(CONST s: ARRAY OF CHAR): LONGINT IS "Conv_LongIntVal";
(* CODE put_long(&ob_res, strtoll((char * ) args[0].x, NULL, 10)); *)

(** RealVal -- convert string to real *)
PROCEDURE RealVal*(CONST s: ARRAY OF CHAR): REAL;
BEGIN
  RETURN SHORT(LongRealVal(s))
END RealVal;

(** LongRealVal -- convert string to long real *)
PROCEDURE LongRealVal*(CONST s: ARRAY OF CHAR): LONGREAL IS "Conv_LongRealVal";
(* CODE put_double(&ob_res, atof((char * ) args[0].x)); *)

(** ConvInt -- convert integer to string and store in buffer *)
PROCEDURE ConvInt*(n: INTEGER; VAR s: ARRAY OF CHAR) IS "Conv_ConvInt";
(* CODE snprintf((char * ) args[1].x, args[2].i, "%d", args[0].i); *)

END Conv.
