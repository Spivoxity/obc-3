(*
 * MathL.m
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

MODULE MathL;

IMPORT DynLink;

(* COPY #include <math.h> *)

CONST
  pi* = 3.1415926535897932385D0;
  e* =  2.7182818284590452354D0;

PROCEDURE Sqrt*(x: LONGREAL): LONGREAL IS "*MathL_Sqrt";
(* CODE put_double(&ob_dres, sqrt(get_double(&args[0]))); *)

PROCEDURE Sin*(x: LONGREAL): LONGREAL IS "*MathL_Sin";
(* CODE put_double(&ob_dres, sin(get_double(&args[0]))); *)

PROCEDURE Cos*(x: LONGREAL): LONGREAL IS "*MathL_Cos";
(* CODE put_double(&ob_dres, cos(get_double(&args[0]))); *)

PROCEDURE Tan*(x: LONGREAL): LONGREAL IS "*MathL_Tan";
(* CODE put_double(&ob_dres, tan(get_double(&args[0]))); *)

PROCEDURE Arctan*(x: LONGREAL): LONGREAL IS "*MathL_Atan";
(* CODE put_double(&ob_dres, atan(get_double(&args[0]))); *)

PROCEDURE Arctan2*(y, x: LONGREAL): LONGREAL IS "*MathL_Atan2";
(* CODE put_double(&ob_dres, 
   	           atan2(get_double(&args[0]), get_double(&args[2]))); *)

PROCEDURE Exp*(x: LONGREAL): LONGREAL IS "*MathL_Exp";
(* CODE put_double(&ob_dres, exp(get_double(&args[0]))); *)

PROCEDURE Ln*(x: LONGREAL): LONGREAL IS "*MathL_Ln";
(* CODE put_double(&ob_dres, log(get_double(&args[0]))); *)

BEGIN
  DynLink.Load("MathL");
END MathL.
