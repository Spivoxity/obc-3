(*
 * MathL.m
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

MODULE MathL;

(* COPY #include <math.h> *)

CONST
  pi* = 3.1415926535897932385D0;
  e* =  2.7182818284590452354D0;

PROCEDURE Sqrt*(x: LONGREAL): LONGREAL IS "sqrt";

PROCEDURE Sin*(x: LONGREAL): LONGREAL IS "sin";

PROCEDURE Cos*(x: LONGREAL): LONGREAL IS "cos";

PROCEDURE Tan*(x: LONGREAL): LONGREAL IS "tan";

PROCEDURE Arctan*(x: LONGREAL): LONGREAL IS "atan";

PROCEDURE Arctan2*(y, x: LONGREAL): LONGREAL IS "atan2";

PROCEDURE Exp*(x: LONGREAL): LONGREAL IS "exp";

PROCEDURE Ln*(x: LONGREAL): LONGREAL IS "log";

PROCEDURE Power*(x, y: LONGREAL): LONGREAL IS "pow";

END MathL.
