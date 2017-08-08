(*
 * Args.m
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

(** Args -- access to program arguments and evironment *)
MODULE Args;

(** argc -- number of arguments *)
VAR argc-: INTEGER;

(** GetArg -- fetch an argument into a string buffer *)
PROCEDURE GetArg*(n: INTEGER; VAR s: ARRAY OF CHAR);
BEGIN
  GetArg0(n, s, LEN(s))
END GetArg;

PROCEDURE GetArg0(n: INTEGER;
                  VAR s: ARRAY OF CHAR; len: INTEGER) IS "GetArg0";

(** GetEnv -- fetch an environment variable into a string buffer *)
PROCEDURE GetEnv*(name: ARRAY OF CHAR; VAR s: ARRAY OF CHAR);
BEGIN
  GetEnv0(name, s, LEN(s))
END GetEnv;

PROCEDURE GetEnv0(name: ARRAY OF CHAR;
                  VAR s: ARRAY OF CHAR; len: INTEGER) IS "GetEnv0";

PROCEDURE SetArgc(VAR ac: INTEGER) IS "SetArgc";

BEGIN
  SetArgc(argc)
END Args.

--CODE--

#include "obx.h"

void GetArg0(int n, char *s, int len) {
     const char *t = (0 <= n && n < saved_argc ? 
	              saved_argv[n] : "");
     obcopy(s, t, len);
}

void GetEnv0(char *name, char *s, int len) {
     const char *t = getenv(name);
     if (t == NULL) t = "";
     obcopy(s, t, len);
}

void SetArgc(int *ac) {
  *ac = saved_argc;
}


