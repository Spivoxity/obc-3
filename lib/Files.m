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
  RawFile = SYSTEM.LONGPTR;

  (** File -- type of open files *)
  File* = POINTER TO FileDesc;
  FileDesc = RECORD file*: RawFile END;

VAR
  (** stdin -- standard input file *)
  stdin*: File;

  (** stdout -- standard output file *)
  stdout*: File;

  (** stderr -- standard error file *)
  stderr*: File;

(* Interface routines *)

(** Open -- open a file by name; return NIL if not found *)
PROCEDURE Open*(CONST name, mode: ARRAY OF CHAR): File;
  VAR fp: RawFile; f: File;
BEGIN
  fp := fopen(name, mode);
  IF fp = SYSTEM.VAL(RawFile, LONG(0)) THEN RETURN NIL END;
  NEW(f); f.file := fp; RETURN f
END Open;

(** FDOpen -- open a file given a file descriptor, or return NIL *)
PROCEDURE FDOpen*(fd: INTEGER; CONST mode: ARRAY OF CHAR): File;
  VAR fp: RawFile; f: File;
BEGIN
  fp := fdopen(fd, mode);
  IF fp = SYSTEM.VAL(RawFile, LONG(0)) THEN RETURN NIL END;
  NEW(f); f.file := fp; RETURN f
END FDOpen;

(** Close -- close a file *)
PROCEDURE Close*(f: File);
BEGIN
  fclose(raw(f));
  f.file := SYSTEM.VAL(RawFile, LONG(0))
END Close;

(** Eof -- test of end of file *)
PROCEDURE Eof*(f: File): BOOLEAN;
  CONST EOF = -1;
  VAR c: INTEGER; fp: RawFile;
BEGIN
  fp := raw(f);
  c := obgetc(fp);
  IF c = EOF THEN
    RETURN TRUE
  ELSE
    ungetc(CHR(c), fp);
    RETURN FALSE
  END
END Eof;

(** Flush -- ensure buffered output has been written out *)
PROCEDURE Flush*(f: File); BEGIN fflush(raw(f)) END Flush;

(** ReadChar -- read a character *)
PROCEDURE ReadChar*(f: File; VAR c: CHAR);
BEGIN c := CHR(obgetc(raw(f))) END ReadChar;

(** WriteInt -- output an integer with a specified width *)
PROCEDURE WriteInt*(f: File; n: INTEGER; width: INTEGER);
BEGIN FmtInt(raw(f), n, width) END WriteInt;

(** WriteLongInt -- output a long integer with specified width *)
PROCEDURE WriteLongInt*(f: File; n: LONGINT; width: INTEGER);
BEGIN FmtLong(raw(f), n, width) END WriteLongInt;

(** WriteReal -- output a real in scientific notation *)
PROCEDURE WriteReal*(f: File; x: REAL);
BEGIN FmtReal(raw(f), x) END WriteReal;

(** WriteLongReal -- output a long real in scientific notation *)
PROCEDURE WriteLongReal*(f: File; x: LONGREAL);
BEGIN FmtLongReal(raw(f), x) END WriteLongReal;

(** WriteFixed -- output a long real in fixed decimal notation *)
PROCEDURE WriteFixed*(f: File; x: LONGREAL; width, dec: INTEGER);
BEGIN FmtFixed(raw(f), x, width, dec) END WriteFixed;

(** WriteChar -- output a character *)
PROCEDURE WriteChar*(f: File; c: CHAR);
BEGIN fputc(c, raw(f)) END WriteChar;

(** WriteString -- output a null-terminated string *)
PROCEDURE WriteString*(f: File; CONST s: ARRAY OF CHAR);
BEGIN FmtString(raw(f), s, LEN(s)) END WriteString;

(** WriteLn -- output a newline *)
PROCEDURE WriteLn*(f: File);
BEGIN WriteChar(f, CHR(10)) END WriteLn;

(** Read -- read an arbitary binary object *)
PROCEDURE Read*(f: File; VAR buf: ARRAY OF SYSTEM.BYTE);
BEGIN fread(buf, LEN(buf), 1, raw(f)) END Read;

(** Write -- write an arbitary binary object *)
PROCEDURE Write*(f: File; VAR buf: ARRAY OF SYSTEM.BYTE);
BEGIN fwrite(buf, LEN(buf), 1, raw(f)) END Write;

(** Seek -- set the file pointer to a specified offset *)
PROCEDURE Seek*(f: File; offset, whence: INTEGER);
BEGIN fseek(raw(f), offset, whence) END Seek;

CONST 
  (** SeekSet -- "whence" argument for Seek to set absolute postition *)
  SeekSet* = 0; 
  (** SeekCur -- "whence" argument to set position relative to current pos *)
  SeekCur* = 1; 
  (** SeekEnd -- "whence" argument to set position relative to end of file *)
  SeekEnd* = 2;

(** Tell -- return current file postion *)
PROCEDURE Tell*(f: File): INTEGER;
BEGIN RETURN ftell(raw(f)) END Tell;


(* The implementation *)

PROCEDURE raw(f: File): RawFile;
BEGIN
  IF (f = NIL) OR (f.file = SYSTEM.VAL(RawFile, LONG(0))) THEN
    SYSTEM.LIBERROR("file is not open")
  END;
  RETURN f.file
END raw;

(* Wrappers around functions from stdio *)
PROCEDURE fopen(name, mode: ARRAY OF CHAR): RawFile IS "fopen";
PROCEDURE fdopen(fd: INTEGER; mode: ARRAY OF CHAR): RawFile IS "fdopen";
PROCEDURE fclose(fp: RawFile) IS "fclose";
PROCEDURE fflush(fp: RawFile) IS "fflush";
PROCEDURE obgetc(fp: RawFile): INTEGER IS "obgetc";
PROCEDURE ungetc(c: CHAR; fp: RawFile) IS "ungetc";
PROCEDURE fputc(c: CHAR; fp: RawFile) IS "fputc";
PROCEDURE fseek(fp: RawFile; offset, whence: INTEGER) IS "fseek";
PROCEDURE ftell(fp: RawFile): INTEGER IS "ftell";
PROCEDURE fread(VAR buf: ARRAY OF SYSTEM.BYTE; n, s: INTEGER;
  fp: RawFile): INTEGER IS "fread";
PROCEDURE fwrite(VAR buf: ARRAY OF SYSTEM.BYTE; n, s: INTEGER; fp: RawFile)
  IS "fwrite";

(* Wrappers around printf calls *)
PROCEDURE FmtInt(fp: RawFile; n, w: INTEGER) IS "FmtInt";
PROCEDURE FmtLong(fp: RawFile; n: LONGINT; w: INTEGER) IS "FmtLong";
PROCEDURE FmtReal(fp: RawFile; x: REAL) IS "FmtReal";
PROCEDURE FmtLongReal(fp: RawFile; x: LONGREAL) IS "FmtLongReal";
PROCEDURE FmtFixed(f: RawFile; x: LONGREAL; width, dec: INTEGER)
  IS "FmtFixed";
PROCEDURE FmtString(fp: RawFile; CONST s: ARRAY OF CHAR; len: INTEGER)
  IS "FmtString";

PROCEDURE Init(VAR in, out, err: RawFile) IS "InitFiles";

BEGIN
  NEW(stdin); NEW(stdout); NEW(stderr);
  Init(stdin.file, stdout.file, stderr.file)
END Files.

--CODE--

#include "obx.h"

void FmtInt(FILE *fp, int n, int w) {
     fprintf(fp, "%*d", w, n);
}

void FmtLong(FILE *fp, longint n, int w) {
#ifdef __MINGW32__
     const char *fmt = "%*I64d";
#else
     const char *fmt = "%*lld";
#endif
     fprintf(fp, fmt, w, n);
}

void FmtReal(FILE *fp, float x) {
     fprintf(fp, "%#G", x);
}

void FmtLongReal(FILE *fp, double x) {
     fprintf(fp, "%#.12G", x);
}

void FmtFixed(FILE *fp, double x, int width, int dec) {
     fprintf(fp, "%*.*f", width, dec, x);
}

void FmtString(FILE *fp, char *s, int len) {
     fprintf(fp, "%.*s", len, s);
}

void InitFiles(value *in, value *out, value *err) {
     put_long(in, (ptrtype) stdin); 
     put_long(out, (ptrtype) stdout);
     put_long(err, (ptrtype) stderr);
}

