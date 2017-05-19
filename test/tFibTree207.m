MODULE tFibTree207;

IMPORT Out, GC;

TYPE 
  tree = POINTER TO node;
  node = ARRAY 8 OF RECORD a: INTEGER; b: ARRAY 4 OF tree END;

PROCEDURE Cons(l, r: tree): tree;
  VAR p: tree;
BEGIN
  NEW(p);
  p[0].b[3] := l; p[6].b[2] := r;
  RETURN p
END Cons;

PROCEDURE Left(t: tree): tree; BEGIN RETURN t[0].b[3] END Left;
PROCEDURE Right(t: tree): tree; BEGIN RETURN t[6].b[2] END Right;

PROCEDURE Build(n: INTEGER): tree;
  VAR t: tree;
BEGIN
  IF n <= 1 THEN
    GC.Collect;
    t := NIL
  ELSE
    t := Cons(Build(n-2), Build(n-1))
  END
RETURN t
END Build;

PROCEDURE Print(t:tree);
BEGIN
  IF t = NIL THEN
    Out.Char('.')
  ELSE
    Out.Char('(');
    Print(Left(t));
    Print(Right(t));
    Out.Char(')')
  END
END Print;

PROCEDURE count(t:tree): INTEGER;
VAR c: INTEGER;
BEGIN
  IF t = NIL THEN
    c := 1
  ELSE
    c := count(Left(t)) + count(Right(t))
  END
RETURN c
END count;

VAR i: INTEGER; p: tree;

BEGIN 
  GC.Debug("gs");

  FOR i := 0 TO 7 DO
    p := Build(i);
    GC.Collect;
    Print(p); Out.Ln();
    Out.String("Count = "); Out.Int(count(p), 0); 
    Out.Ln(); Out.Ln();
  END
END tFibTree207.

(*<<
[gc][gc].
Count = 1

[gc][gc].
Count = 1

[gc][gc][gc](..)
Count = 2

[gc][gc][gc][gc](.(..))
Count = 3

[gc][gc][gc][gc][gc][gc]((..)(.(..)))
Count = 5

[gc][gc][gc][gc][gc][gc][gc][gc][gc]((.(..))((..)(.(..))))
Count = 8

[gc][gc][gc][gc][gc][gc][gc][gc][gc][gc][gc][gc][gc][gc](((..)(.(..)))((.(..))((..)(.(..)))))
Count = 13

[gc][gc][gc][gc][gc][gc][gc][gc][gc][gc][gc][gc][gc][gc][gc][gc][gc][gc][gc][gc][gc][gc](((.(..))((..)(.(..))))(((..)(.(..)))((.(..))((..)(.(..))))))
Count = 21

>>*)

(*[[
!! (SYMFILE #tFibTree207 STAMP #tFibTree207.%main 1)
!! (CHKSUM STAMP)
!! 
MODULE tFibTree207 STAMP 0
IMPORT Out STAMP
IMPORT GC STAMP
ENDHDR

PROC tFibTree207.Cons 4 4 0x00310001
! PROCEDURE Cons(l, r: tree): tree;
!   NEW(p);
CONST 160
GLOBAL tFibTree207.node
GLOBAL NEW
CALLW 2
STLW -4
!   p[0].b[3] := l; p[6].b[2] := r;
LDLW 12
LDLW -4
NCHECK 13
STNW 16
LDLW 16
LDLW -4
NCHECK 13
STNW 132
!   RETURN p
LDLW -4
RETURNW
END

PROC tFibTree207.Left 0 4 0x00100001
! PROCEDURE Left(t: tree): tree; BEGIN RETURN t[0].b[3] END Left;
LDLW 12
NCHECK 17
LDNW 16
RETURNW
END

PROC tFibTree207.Right 0 4 0x00100001
! PROCEDURE Right(t: tree): tree; BEGIN RETURN t[6].b[2] END Right;
LDLW 12
NCHECK 18
LDNW 132
RETURNW
END

PROC tFibTree207.Build 4 4 0x00010001
! PROCEDURE Build(n: INTEGER): tree;
!   IF n <= 1 THEN
LDLW 12
CONST 1
JGT L6
!     GC.Collect;
GLOBAL GC.Collect
CALL 0
!     t := NIL
CONST 0
STLW -4
JUMP L4
LABEL L6
!     t := Cons(Build(n-2), Build(n-1))
LDLW 12
DEC
GLOBAL tFibTree207.Build
CALLW 1
LDLW 12
CONST 2
MINUS
GLOBAL tFibTree207.Build
STKMAP 0x00000005
CALLW 1
GLOBAL tFibTree207.Cons
CALLW 2
STLW -4
LABEL L4
! RETURN t
LDLW -4
RETURNW
END

PROC tFibTree207.Print 0 4 0x00100001
! PROCEDURE Print(t:tree);
!   IF t = NIL THEN
LDLW 12
JNEZ L10
!     Out.Char('.')
CONST 46
ALIGNC
GLOBAL Out.Char
CALL 1
RETURN
LABEL L10
!     Out.Char('(');
CONST 40
ALIGNC
GLOBAL Out.Char
CALL 1
!     Print(Left(t));
LDLW 12
GLOBAL tFibTree207.Left
CALLW 1
GLOBAL tFibTree207.Print
CALL 1
!     Print(Right(t));
LDLW 12
GLOBAL tFibTree207.Right
CALLW 1
GLOBAL tFibTree207.Print
CALL 1
!     Out.Char(')')
CONST 41
ALIGNC
GLOBAL Out.Char
CALL 1
RETURN
END

PROC tFibTree207.count 4 4 0x00100001
! PROCEDURE count(t:tree): INTEGER;
!   IF t = NIL THEN
LDLW 12
JNEZ L13
!     c := 1
CONST 1
STLW -4
JUMP L11
LABEL L13
!     c := count(Left(t)) + count(Right(t))
LDLW 12
GLOBAL tFibTree207.Left
CALLW 1
GLOBAL tFibTree207.count
CALLW 1
LDLW 12
GLOBAL tFibTree207.Right
CALLW 1
GLOBAL tFibTree207.count
CALLW 1
PLUS
STLW -4
LABEL L11
! RETURN c
LDLW -4
RETURNW
END

PROC tFibTree207.%main 0 4 0
!   GC.Debug("gs");
CONST 3
GLOBAL tFibTree207.%1
GLOBAL GC.Debug
CALL 2
!   FOR i := 0 TO 7 DO
CONST 0
STGW tFibTree207.i
LABEL L14
LDGW tFibTree207.i
CONST 7
JGT L15
!     p := Build(i);
LDGW tFibTree207.i
GLOBAL tFibTree207.Build
CALLW 1
STGW tFibTree207.p
!     GC.Collect;
GLOBAL GC.Collect
CALL 0
!     Print(p); Out.Ln();
LDGW tFibTree207.p
GLOBAL tFibTree207.Print
CALL 1
GLOBAL Out.Ln
CALL 0
!     Out.String("Count = "); Out.Int(count(p), 0); 
CONST 9
GLOBAL tFibTree207.%2
GLOBAL Out.String
CALL 2
CONST 0
LDGW tFibTree207.p
GLOBAL tFibTree207.count
CALLW 1
GLOBAL Out.Int
CALL 2
!     Out.Ln(); Out.Ln();
GLOBAL Out.Ln
CALL 0
GLOBAL Out.Ln
CALL 0
!   FOR i := 0 TO 7 DO
LDGW tFibTree207.i
INC
STGW tFibTree207.i
JUMP L14
LABEL L15
RETURN
END

! Global variables
GLOVAR tFibTree207.i 4
GLOVAR tFibTree207.p 4

! Pointer map
DEFINE tFibTree207.%gcmap
WORD GC_BASE
WORD tFibTree207.p
WORD 0
WORD GC_END

! String "gs"
DEFINE tFibTree207.%1
STRING 677300

! String "Count = "
DEFINE tFibTree207.%2
STRING 436F756E74203D2000

! Descriptor for *anon*
DEFINE tFibTree207.%3
WORD 0x0000003d
WORD 0
WORD tFibTree207.%3.%anc

DEFINE tFibTree207.%3.%anc
WORD tFibTree207.%3

! Descriptor for node
DEFINE tFibTree207.node
WORD tFibTree207.node.%map

! Pointer maps
DEFINE tFibTree207.node.%map
WORD GC_REPEAT
WORD 0
WORD 8
WORD 20
WORD 0x0000003d
WORD GC_END

! End of file
]]*)
