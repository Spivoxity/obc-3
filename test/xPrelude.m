MODULE xPrelude;

(* Mixed prelude code for all the tests *)

IMPORT Out, xTypes;

(* For tFib *)

(* It's called PROC because that's a keyword in .i files *)
CONST PROC* = "fib";

PROCEDURE Fib*(n: INTEGER): INTEGER;
  VAR a, b, c, i: INTEGER;
BEGIN
  IF n = 0 THEN
    RETURN 0
  ELSE
    a := 0; b := 1; i := 1;
    WHILE i < n DO
      c := a + b; a := b; b := c;
      i := i + 1
    END;
    RETURN b
  END
END Fib;

(* For tHide *)

TYPE blob* = RECORD x, y*: INTEGER END;

PROCEDURE (VAR b: blob) one;
BEGIN
  Out.String("blob.one"); Out.Ln;
  b.x := 12
END one;

PROCEDURE (VAR b: blob) two*;
BEGIN
  Out.String("blob.two "); Out.Int(b.x, 0); Out.Ln
END two;

PROCEDURE (VAR b: blob) proc*;
BEGIN
  b.one; b.two
END proc;

(* For tSuper2 *)
TYPE
  s1 = ABSTRACT RECORD END;
  s2* = ABSTRACT RECORD (s1) END;

PROCEDURE (VAR x: s1) Foo*; BEGIN Out.String("s1.Foo"); Out.Ln END Foo;
ABSTRACT PROCEDURE (VAR x: s1) Baz*;

(* For tEnum *)
TYPE colour* = (red*, blue*, green*);

VAR x*: colour;

(* For tTypeRef *)
PROCEDURE rPrint*(VAR x: xTypes.rec);
BEGIN
  Out.Int(x.x, 0); Out.Ln
END rPrint;

TYPE xptr* = POINTER TO xrec; xrec* = RECORD (xTypes.rec) END;

(* For tLimits *)
CONST maxlong* = MAX(LONGREAL);

(* For tPtrExp.m *)
TYPE ptr1* = POINTER TO ARRAY OF INTEGER;
TYPE ptr2* = POINTER TO ARRAY OF CHAR;

END xPrelude.
