MODULE tFibTree2;

IMPORT Out, SYSTEM;

TYPE 
  tree = POINTER TO node;
  node = ARRAY 8 OF RECORD a: INTEGER; b: ARRAY 4 OF tree END;

PROCEDURE Cons(l, r: tree): tree;
  VAR p: tree;
BEGIN
  NEW(p);
  p[0].b[3] := l; p[6].b[2] := r;
  RETURN p;
END Cons;

PROCEDURE Left(t: tree): tree; BEGIN RETURN t[0].b[3] END Left;
PROCEDURE Right(t: tree): tree; BEGIN RETURN t[6].b[2] END Right;

PROCEDURE Build(n: INTEGER): tree;
BEGIN
  IF n <= 1 THEN
    SYSTEM.GC;
    RETURN NIL
  ELSE
    RETURN Cons(Build(n-2), Build(n-1))
  END
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
BEGIN
  IF t = NIL THEN
    RETURN 1
  ELSE
    RETURN count(Left(t)) + count(Right(t))
  END
END count;

VAR i: INTEGER; p: tree;

BEGIN 
  FOR i := 0 TO 7 DO
    p := Build(i);
    SYSTEM.GC;
    Print(p); Out.Ln();
    Out.String("Count = "); Out.Int(count(p), 0); 
    Out.Ln(); Out.Ln();
  END
END tFibTree2.

(*<<
.
Count = 1

.
Count = 1

(..)
Count = 2

(.(..))
Count = 3

((..)(.(..)))
Count = 5

((.(..))((..)(.(..))))
Count = 8

(((..)(.(..)))((.(..))((..)(.(..)))))
Count = 13

(((.(..))((..)(.(..))))(((..)(.(..)))((.(..))((..)(.(..))))))
Count = 21

>>*)

(*[[
!! (SYMFILE #tFibTree2 STAMP #tFibTree2.%main 1 #tFibTree2.m)
!! (CHKSUM STAMP)
!! 
MODULE tFibTree2 STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tFibTree2.Cons 4 4 0x00310001
! PROCEDURE Cons(l, r: tree): tree;
!   NEW(p);
CONST 160
GLOBAL tFibTree2.node
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
!   RETURN p;
LDLW -4
RETURN
END

PROC tFibTree2.Left 0 3 0x00100001
! PROCEDURE Left(t: tree): tree; BEGIN RETURN t[0].b[3] END Left;
LDLW 12
NCHECK 17
LDNW 16
RETURN
END

PROC tFibTree2.Right 0 3 0x00100001
! PROCEDURE Right(t: tree): tree; BEGIN RETURN t[6].b[2] END Right;
LDLW 12
NCHECK 18
LDNW 132
RETURN
END

PROC tFibTree2.Build 0 3 0
! PROCEDURE Build(n: INTEGER): tree;
!   IF n <= 1 THEN
LDLW 12
CONST 1
JGT L5
!     SYSTEM.GC;
GLOBAL SYSTEM.GC
CALL 0
!     RETURN NIL
CONST 0
RETURN
LABEL L5
!     RETURN Cons(Build(n-2), Build(n-1))
LDLW 12
DEC
GLOBAL tFibTree2.Build
CALLW 1
LDLW 12
CONST 2
MINUS
GLOBAL tFibTree2.Build
STKMAP 0x00000005
CALLW 1
GLOBAL tFibTree2.Cons
CALLW 2
RETURN
END

PROC tFibTree2.Print 0 2 0x00100001
! PROCEDURE Print(t:tree);
!   IF t = NIL THEN
LDLW 12
JNEQZ L9
!     Out.Char('.')
CONST 46
ALIGNC
GLOBAL Out.Char
CALL 1
RETURN
LABEL L9
!     Out.Char('(');
CONST 40
ALIGNC
GLOBAL Out.Char
CALL 1
!     Print(Left(t));
LDLW 12
GLOBAL tFibTree2.Left
CALLW 1
GLOBAL tFibTree2.Print
CALL 1
!     Print(Right(t));
LDLW 12
GLOBAL tFibTree2.Right
CALLW 1
GLOBAL tFibTree2.Print
CALL 1
!     Out.Char(')')
CONST 41
ALIGNC
GLOBAL Out.Char
CALL 1
RETURN
END

PROC tFibTree2.count 0 3 0x00100001
! PROCEDURE count(t:tree): INTEGER;
!   IF t = NIL THEN
LDLW 12
JNEQZ L12
!     RETURN 1
CONST 1
RETURN
LABEL L12
!     RETURN count(Left(t)) + count(Right(t))
LDLW 12
GLOBAL tFibTree2.Left
CALLW 1
GLOBAL tFibTree2.count
CALLW 1
LDLW 12
GLOBAL tFibTree2.Right
CALLW 1
GLOBAL tFibTree2.count
CALLW 1
PLUS
RETURN
END

PROC tFibTree2.%main 0 3 0
!   FOR i := 0 TO 7 DO
CONST 0
STGW tFibTree2.i
LABEL L13
LDGW tFibTree2.i
CONST 7
JGT L14
!     p := Build(i);
LDGW tFibTree2.i
GLOBAL tFibTree2.Build
CALLW 1
STGW tFibTree2.p
!     SYSTEM.GC;
GLOBAL SYSTEM.GC
CALL 0
!     Print(p); Out.Ln();
LDGW tFibTree2.p
GLOBAL tFibTree2.Print
CALL 1
GLOBAL Out.Ln
CALL 0
!     Out.String("Count = "); Out.Int(count(p), 0); 
CONST 9
GLOBAL tFibTree2.%1
GLOBAL Out.String
CALL 2
CONST 0
LDGW tFibTree2.p
GLOBAL tFibTree2.count
CALLW 1
GLOBAL Out.Int
CALL 2
!     Out.Ln(); Out.Ln();
GLOBAL Out.Ln
CALL 0
GLOBAL Out.Ln
CALL 0
!   FOR i := 0 TO 7 DO
LDGW tFibTree2.i
INC
STGW tFibTree2.i
JUMP L13
LABEL L14
RETURN
END

! Global variables
GLOVAR tFibTree2.i 4
GLOVAR tFibTree2.p 4

! Global pointer map
DEFINE tFibTree2.%gcmap
WORD GC_POINTER
WORD tFibTree2.p
WORD GC_END

! String "Count = "
DEFINE tFibTree2.%1
STRING 436F756E74203D2000

! Descriptor for *anon*
DEFINE tFibTree2.%2
WORD 0x0000003d
WORD 0
WORD tFibTree2.%2.%anc

DEFINE tFibTree2.%2.%anc
WORD tFibTree2.%2

! Descriptor for node
DEFINE tFibTree2.node
WORD tFibTree2.node.%map

! Pointer maps
DEFINE tFibTree2.node.%map
WORD GC_REPEAT
WORD 0
WORD 8
WORD 20
WORD 0x0000003d
WORD GC_END
WORD GC_END

! End of file
]]*)
