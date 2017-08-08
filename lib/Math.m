(*
 * Math.m
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

(** Mathematical functions for single precision *)
MODULE Math;

IMPORT DynLink;

(* COPY #include <math.h> *)

CONST
  pi* = 3.1415926535897932385;
  e* =  2.7182818284590452354;

PROCEDURE Sqrt*(x: REAL): REAL IS "Math_Sqrt";
(* CODE ob_res.f = sqrtf(args[0].f); *)

PROCEDURE Sin*(x: REAL): REAL IS "Math_Sin";
(* CODE ob_res.f = sinf(args[0].f); *)

PROCEDURE Cos*(x: REAL): REAL IS "Math_Cos";
(* CODE ob_res.f = cosf(args[0].f); *)

PROCEDURE Tan*(x: REAL): REAL IS "Math_Tan";
(* CODE ob_res.f = tanf(args[0].f); *)

PROCEDURE Arctan*(x: REAL): REAL IS "Math_Atan";
(* CODE ob_res.f = atanf(args[0].f); *)

PROCEDURE Arctan2*(y, x: REAL): REAL IS "Math_Atan2";
(* CODE ob_res.f = atan2f(args[0].f, args[1].f); *)

PROCEDURE Exp*(x: REAL): REAL IS "Math_Exp";
(* CODE ob_res.f = expf(args[0].f); *)

PROCEDURE Ln*(x: REAL): REAL IS "Math_Ln";
(* CODE ob_res.f = logf(args[0].f); *)

BEGIN
  DynLink.Load("Math");
END Math.
