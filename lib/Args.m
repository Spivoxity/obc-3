(*
 * Args.m
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

(** Args -- access to program arguments and evironment *)
MODULE Args;

(** argc -- number of arguments *)
VAR argc-: INTEGER;

(** GetArg -- fetch an argument into a string buffer *)
PROCEDURE GetArg*(n: INTEGER; VAR s: ARRAY OF CHAR) IS "Args_GetArg";
(* CODE 
     const char *t = (0 <= args[0].i && args[0].i < saved_argc ? 
	   	      saved_argv[args[0].i] : "");
     obcopy((char * ) args[1].x, t, args[2].i); *)

(** GetEnv -- fetch an environment variable into a string buffer *)
PROCEDURE GetEnv*(name: ARRAY OF CHAR; VAR s: ARRAY OF CHAR) IS "Args_GetEnv";
(* CODE 
     const char *t = getenv((char * ) args[0].x);
     if (t == NULL) t = "";
     obcopy((char * ) args[2].x, t, args[3].i); *)

PROCEDURE SetArgc(VAR ac: INTEGER) IS "Args_SetArgc";
(* CODE ( *(args[0].p)).i = saved_argc; *)

BEGIN
  SetArgc(argc)
END Args.
