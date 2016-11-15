(*
 * Files.m
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

(** General input-output on files *)
MODULE Files;

IMPORT SYSTEM;

TYPE 
  (** File -- type of open files *)
  File* = POINTER TO FileDesc;
  FileDesc = RECORD file: SYSTEM.PTR END;

VAR
  (** stdin -- standard input file *)
  stdin*: File;

  (** stdout -- standard output file *)
  stdout*: File;

  (** stderr -- standard error file *)
  stderr*: File;

(* COPY
static FILE *get_file(value *p, value *cp, value *bp) {
     if (p == NULL || p[0].x == NULL) liberror("file is not open");
     return (FILE * ) p[0].x;
}

#define file_arg(a) get_file(a.p, cp, bp)
*)

PROCEDURE PrimOpen(name, mode: ARRAY OF CHAR): SYSTEM.PTR IS "Files_PrimOpen";
(* CODE ob_res.x = (uchar * ) fopen((char * )args[0].x, (char * )args[2].x); *)

PROCEDURE PrimFDOpen(fd: INTEGER;
	  		mode: ARRAY OF CHAR): SYSTEM.PTR IS "File_PrimFDOpen";
(* CODE ob_res.x = (uchar * ) fdopen(args[0].i, (char * ) args[1].x); *)

(** Open -- open a file by name; return NIL if not found *)
PROCEDURE Open*(CONST name, mode: ARRAY OF CHAR): File;
  VAR fp: SYSTEM.PTR; f: File;
BEGIN
  fp := PrimOpen(name, mode);
  IF fp = NIL THEN RETURN NIL END;
  NEW(f); f.file := fp; RETURN f
END Open;

(** FDOpen -- open a file given a file descriptor, or return NIL *)
PROCEDURE FDOpen*(fd: INTEGER; CONST mode: ARRAY OF CHAR): File;
  VAR fp: SYSTEM.PTR; f: File;
BEGIN
  fp := PrimFDOpen(fd, mode);
  IF fp = NIL THEN RETURN NIL END;
  NEW(f); f.file := fp; RETURN f
END FDOpen;

(** Close -- close a file *)
PROCEDURE Close*(fp: File) IS "Files_Close";
(* CODE fclose(file_arg(args[0])); args[0].p[0].x = NULL; *)

(** Eof -- test of end of file *)
PROCEDURE Eof*(fp: File): BOOLEAN IS "Files_Eof";
(* CODE 
     FILE *fp = file_arg(args[0]);
     int c = obgetc(fp); 
     mybool r = (c == EOF);
     if (!r) ungetc(c, fp); 
     ob_res.i = r; *)

(** Flush -- ensure buffered output has been written out *)
PROCEDURE Flush*(fp: File) IS "Files_Flush";
(* CODE fflush(file_arg(args[0])); *)

(** ReadChar -- read a character *)
PROCEDURE ReadChar*(f: File; VAR c: CHAR) IS "Files_ReadChar";
(* CODE *(args[1].x) = obgetc(file_arg(args[0])); *)

(** WriteInt -- output an integer with a specified width *)
PROCEDURE WriteInt*(f: File; n: INTEGER; width: INTEGER) IS "Files_WriteInt";
(* CODE fprintf(file_arg(args[0]), "%*d", args[2].i, args[1].i); *)

(** WriteLongInt -- output a long integer with specified width *)
PROCEDURE WriteLongInt*(f: File; n: LONGINT; 
				width: INTEGER) IS "Files_WriteLongInt";
(* CODE 
#ifdef __MINGW32__
     const char *fmt = "%*I64d";
#else
     const char *fmt = "%*lld";
#endif
     fprintf(file_arg(args[0]), fmt, args[3].i, get_long(&args[1]));

#if 0
     char buf[32];
     char *p = &buf[32];
     long long int x = get_long(&args[1]);
     unsigned long long int y = (x < 0 ? -x : x);
     *(--p) = '\0';
     do { *(--p) = (y % 10) + '0'; y /= 10; } while (y != 0);
     if (x < 0) *(--p) = '-';
     fprintf(file_arg(args[0]), "%*s", args[3].i, p);   
#endif
*)

(** WriteReal -- output a real in scientific notation *)
PROCEDURE WriteReal*(f: File; x: REAL) IS "Files_WriteReal";
(* CODE fprintf(file_arg(args[0]), "%#G", args[1].f); *)

(** WriteLongReal -- output a long real in scientific notation *)
PROCEDURE WriteLongReal*(f: File; x: LONGREAL) IS "Files_WriteLongReal";
(* CODE fprintf(file_arg(args[0]), "%#.12G", get_double(&args[1])); *)

(** WriteFixed -- output a long real in fixed decimal notation *)
PROCEDURE WriteFixed*(f: File; x: LONGREAL; 
				width, dec: INTEGER) IS "Files_WriteFixed";
(* CODE 
     fprintf(file_arg(args[0]), "%*.*f", 
	     args[3].i, args[4].i, get_double(&args[1])); *)

(** WriteChar -- output a character *)
PROCEDURE WriteChar*(f: File; c: CHAR) IS "Files_WriteChar";
(* CODE fprintf(file_arg(args[0]), "%c", align_byte(args[1].i)); *)

(** WriteString -- output a null-terminated string *)
PROCEDURE WriteString*(f: File; CONST s: ARRAY OF CHAR) IS "Files_WriteString";
(* CODE fprintf(file_arg(args[0]), "%.*s", args[2].i, args[1].x); *)

(** WriteLn -- output a newline *)
PROCEDURE WriteLn*(f: File) IS "Files_WriteLn";
(* CODE putc('\n', file_arg(args[0])); *)

(** Read -- read an arbitary binary object *)
PROCEDURE Read*(f: File; VAR buf: ARRAY OF SYSTEM.BYTE) IS "Files_Read";
(* CODE int UNUSED nread =
     fread(args[1].x, args[2].i, 1, file_arg(args[0])); *)

(** Write -- write an arbitary binary object *)
PROCEDURE Write*(f: File; VAR buf: ARRAY OF SYSTEM.BYTE) IS "Files_Write";
(* CODE int UNUSED nwritten = 
     fwrite(args[1].x, args[2].i, 1, file_arg(args[0])); *)

(** Seek -- set the file pointer to a specified offset *)
PROCEDURE Seek*(f: File; offset, whence: INTEGER) IS "Files_Seek";
(* CODE fseek(file_arg(args[0]), args[1].i, args[2].i); *)

CONST 
  (** SeekSet -- "whence" argument for Seek to set absolute postition *)
  SeekSet* = 0; 
  (** SeekCur -- "whence" argument to set position relative to current pos *)
  SeekCur* = 1; 
  (** SeekEnd -- "whence" argument to set position relative to end of file *)
  SeekEnd* = 2;

(** Tell -- return current file postion *)
PROCEDURE Tell*(f: File): INTEGER IS "Files_Tell";
(* CODE ob_res.i = ftell(file_arg(args[0])); *)

PROCEDURE Init(VAR in, out, err: SYSTEM.PTR) IS "Files_Init";
(* CODE 
     ( * (args[0].p)).x = (uchar * ) stdin; 
     ( * (args[1].p)).x = (uchar * ) stdout;
     ( * (args[2].p)).x = (uchar * ) stderr; *)

BEGIN
  NEW(stdin); NEW(stdout); NEW(stderr);
  Init(stdin.file, stdout.file, stderr.file)
END Files.
