MODULE FfiTest;

IMPORT Out, Files, SYSTEM;

PROCEDURE fputc(ch: CHAR; fp: SYSTEM.LONGPTR) IS "fputc";
PROCEDURE strcat(VAR dst: ARRAY OF CHAR; src: ARRAY OF CHAR) IS "strcat";
PROCEDURE puts(s: ARRAY OF CHAR) IS "puts";
PROCEDURE sqrtf(x: REAL): REAL IS "sqrtf";
PROCEDURE sqrt(x: LONGREAL): LONGREAL IS "sqrt";
PROCEDURE atan2(y, x: LONGREAL): LONGREAL IS "atan2";
PROCEDURE atoll(s: ARRAY OF CHAR): LONGINT IS "atoll";

VAR s: ARRAY 256 OF CHAR;

BEGIN
  fputc('H', Files.stdout.file);
  s[0] := 0X;
  strcat(s, "ello");
  strcat(s, " world");
  puts(s);
  Out.Real(sqrtf(2)); Out.Ln;
  Out.LongReal(sqrt(2)); Out.Ln;
  Out.LongReal(6*atan2(1, sqrt(3))); Out.Ln;
  Out.LongInt(atoll("678901234567890"), 0); Out.Ln
END FfiTest.
