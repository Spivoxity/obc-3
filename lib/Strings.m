(*
 * Strings.m
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

(** Operations on null-terminated strings *)
MODULE Strings;

(** Length -- length of a string *)
PROCEDURE Length*(CONST s: ARRAY OF CHAR): INTEGER;
  VAR n: INTEGER;
BEGIN
  n := 0;
  WHILE (n < LEN(s)) & (s[n] # 0X) DO INC(n) END;
  RETURN n
END Length;

(** Insert -- insert src at position pos in dst *)
PROCEDURE Insert*(src: ARRAY OF CHAR; pos: INTEGER; VAR dst: ARRAY OF CHAR);
  VAR i, n: INTEGER;
BEGIN
  n := Length(src);

  IF pos+n >= LEN(dst)-1 THEN
    dst[LEN(dst)-1] := 0X
  ELSE
    (* Copy the tail part of |dst| *)
    i := Length(dst)-pos;
    IF pos+n+i > LEN(dst)-1 THEN i := SHORT(LEN(dst))-pos-n-1 END;
    dst[pos+n+i] := 0X;
    WHILE i >= 0 DO
      dst[pos+n+i] := dst[pos+i]; DEC(i)
    END;
  END;

  (* Copy the characters of |src| *)
  i := 0;
  WHILE (src[i] # 0X) & (pos+i < LEN(dst)-1) DO
    dst[pos+i] := src[i]; INC(i)
  END
END Insert;

(** Append -- append src at the end of dst *)
PROCEDURE Append*(CONST src: ARRAY OF CHAR; VAR dst: ARRAY OF CHAR);
BEGIN
  Insert(src, Length(dst), dst)
END Append;

(* Delete -- delete n characters from position pos in src *)
PROCEDURE Delete*(VAR src: ARRAY OF CHAR; pos, n: INTEGER);
  VAR i, m: INTEGER;
BEGIN
  m := Length(src);
  IF pos+n >= m THEN
    src[pos] := 0X
  ELSE
    i := 0;
    WHILE src[pos+n+i] # 0X DO
      src[pos+i] := src[pos+n+i]; INC(i)
    END;
    src[pos+i] := 0X
  END
END Delete;

(** Replace -- replace a substring of dst at position pos with src *)
PROCEDURE Replace*(src: ARRAY OF CHAR; pos: INTEGER; VAR dst: ARRAY OF CHAR);
BEGIN
  Delete(dst, pos, Length(src));
  Insert(src, pos, dst)
END Replace;

(** Extract -- copy a substring length n at position pos from src into dst *)
PROCEDURE Extract*(src: ARRAY OF CHAR; pos, n: INTEGER; 
						VAR dst: ARRAY OF CHAR);
  VAR i: INTEGER;
BEGIN
  i := 0;
  WHILE (src[pos+i] # 0X) & (i < n) & (i < LEN(dst)-1) DO
    dst[i] := src[pos+i]; INC(i)
  END;
  dst[i] := 0X
END Extract;

(** Pos -- the first position after pos where pat occurs in txt, or -1 *)
PROCEDURE Pos*(CONST pat, text: ARRAY OF CHAR; pos: INTEGER): INTEGER;
  VAR i, j: INTEGER;
BEGIN
  i := pos;
  LOOP
    j := 0;
    WHILE (pat[j] = text[i+j]) & (pat[j] # 0X) DO INC(j) END;
    IF pat[j] = 0X THEN RETURN i
    ELSIF text[i+j] = 0X THEN RETURN -1
    END;
    INC(i)
  END
END Pos;

(** Cap -- convert letters in src to upper case *)
PROCEDURE Cap*(VAR src: ARRAY OF CHAR);
  VAR i: INTEGER;
BEGIN
  i := 0;
  WHILE src[i] # 0X DO src[i] := CAP(src[i]); INC(i) END
END Cap;

(** Compare -- compare two strings *)
PROCEDURE Compare*(s, t: ARRAY OF CHAR): INTEGER IS + "COMPARE";

END Strings.
