MODULE SemError;

IMPORT Out, SemDefs;

CONST N = 23; Y = N / (N-N);
VAR x: INTEGER;
VAR x, y: REAL;
VAR b: BOOLEAN; long: LONGINT;
CONST xx = x;

TYPE t1 = POINTER TO t2; t2 = INTEGER;  
TYPE t3 = Types.Name; t4 = x.type; t5 = x;

TYPE multidef = INTEGER; multidef = INTEGER;

PROCEDURE foo(x: INTEGER): INTEGER;
BEGIN
  RETURN;
  RETURN 'c'
END foo;

PROCEDURE baz(x: INTEGER);
BEGIN
  RETURN 7
END baz;

PROCEDURE (x: t1) method; END method;

PROCEDURE (VAR x: t2) method2; END method2;

TYPE rec = RECORD x: INTEGER END; ptr = POINTER TO rec;

PROCEDURE (VAR r: rec) x; END x;

PROCEDURE (VAR r: rec) m; END m;

PROCEDURE (r: ptr) m; END m;

PROCEDURE (r: ptr) m2; END m2;

TYPE rec2 = RECORD (rec) END; ptr2 = POINTER TO rec2;

PROCEDURE (VAR r: rec2) m*(q: REAL): REAL; END m;

PROCEDURE (VAR r: rec2) mm; END mm;

PROCEDURE super;
  VAR r: rec2;
BEGIN
  r.mm^
END super;

TYPE rec3 = RECORD END;

PROCEDURE VP(VAR r: rec);
BEGIN
  b := r IS rec3
END VP;

TYPE aaa = bbb; bbb = INTEGER;

TYPE ccc = ARRAY 10 OF ARRAY OF INTEGER;
  ddd = ARRAY OF ARRAY OF REAL;

TYPE fff = RECORD (ptr) END;

PROCEDURE flopsy(): rec; END flopsy;

PROCEDURE mopsy;
  PROCEDURE (VAR x: ptr) metholate; END metholate;
  PROCEDURE digest; END digest;
  VAR f: PROCEDURE;
BEGIN
  f := digest
END mopsy;

TYPE flex = POINTER TO ARRAY OF CHAR;

VAR f: flex; u: SemDefs.fred; p: POINTER TO SemDefs.fred; q: ptr; r: rec;

VAR g: SemError.flex;

PROCEDURE Max(a: ARRAY OF INTEGER): INTEGER;
BEGIN
  RETURN 0;
END Max;

BEGIN
  CASE x OF 3..2: | 1: | 1: | 'a': END;
  CASE 'x' OF 'a': | 'a': END;
  CASE TRUE OF TRUE: END;
  N := 3;
  foo(3);
  RETURN 4;
  IF 3 THEN END;
  WHILE NIL DO END;
  REPEAT ;;; UNTIL x;
  EXIT;
  FOR y := 2 TO 3 DO END;
  FOR N := 2 TO 3 DO END;
  FOR x := 2.0 TO 3 DO END;
  FOR x := 2.0 TO 3.0 DO END;
  FOR x := 2 TO 3 BY y DO END;
  FOR x := 2 TO 3 BY N DO END;
  FOR x := 2 TO 3 BY 1.0 DO END;
  WITH x: REAL DO x := 4.0 END;
  NEW(f, FALSE);
  NEW(NIL);
  x := qqq^;
  x := x^;
  x := t1;
  x := x[y];
  x := x.x;
  Out.Anything;
  u.smile;
  u.frown;
  x := u.mint;
  x := u.humbug;
  foo(3)^ := 4;
  x := x - FALSE;
  x := x / FALSE;
  x := +FALSE;
  x := ~x;
  x := x DIV y;
  x := x < y;
  b := 'foo' = 'f';
  b := 'a' = 3;
  b := TRUE & 3;
  b := x IS INTEGER;
  b := u IS rec;
  b := p^ IS rec;
  b := q IS rec;
  b := q IS ptr; (* OK *)
  b := r IS rec;
  b := q(rec2) IS ptr2;
  r.m2;
  q.m2(r);
  x := Max(f^);
  VP(3);
  Max(3);
  WITH q: ptr DO undef; undef END;
  x := 9999999999;
  f^[0] := f^[long];
  f[0] := f[undecl]
END SemError.
