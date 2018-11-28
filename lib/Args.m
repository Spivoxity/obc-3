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
PROCEDURE GetArg*(n: INTEGER; VAR s: ARRAY OF CHAR) IS "*GetArg";

(** GetEnv -- fetch an environment variable into a string buffer *)
PROCEDURE GetEnv*(name: ARRAY OF CHAR; VAR s: ARRAY OF CHAR) IS "*GetEnv";

PROCEDURE GetArgc(): INTEGER IS "GetArgc";

BEGIN
  argc := GetArgc()
END Args.

--CODE--

#include "obx.h"
#include <stdlib.h>

void GetArg(value *bp) {
     int n = bp[HEAD+0].i;
     char *s = (char *) pointer(bp[HEAD+1]);
     int len = bp[HEAD+2].i;
     const char *t =
          (0 <= n && n < saved_argc ? saved_argv[n] : "");
     obcopy(s, len, t, 0, bp);
}

void GetEnv(value *bp) {
     char *name = (char *) pointer(bp[HEAD+0]);
     char *s = (char *) pointer(bp[HEAD+2]);
     int len = bp[HEAD+3].i;
     const char *t = getenv(name);
     if (t == NULL) t = "";
     obcopy(s, len, t, 0, bp);
}

int GetArgc(void) {
  return saved_argc;
}


