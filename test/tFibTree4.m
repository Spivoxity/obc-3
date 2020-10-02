MODULE tFibTree4;

IMPORT Out, SYSTEM;

TYPE 
  tree = POINTER TO node;
  node = 
    ARRAY 8 OF RECORD 
      a: INTEGER; 
      b: ARRAY 40 OF RECORD x: tree; y: INTEGER; END; 
      c: tree 
    END;

PROCEDURE Cons(l, r: tree): tree;
  VAR p: tree;
BEGIN
  NEW(p);
  p[0].b[3].x := l; p[6].c := r;
  RETURN p;
END Cons;

PROCEDURE Left(t: tree): tree; BEGIN RETURN t[0].b[3].x END Left;
PROCEDURE Right(t: tree): tree; BEGIN RETURN t[6].c END Right;

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
END tFibTree4.

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
!! (SYMFILE #tFibTree4 STAMP #tFibTree4.%main 1 #tFibTree4.m)
!! (CHKSUM STAMP)
!! 
MODULE tFibTree4 STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tFibTree4.Cons 4 4 0x00310001
! PROCEDURE Cons(l, r: tree): tree;
!   NEW(p);
CONST 2624
GLOBAL tFibTree4.node
GLOBAL NEW
CALLW 2
STLW -4
!   p[0].b[3].x := l; p[6].c := r;
LDLW 12
LDLW -4
NCHECK 18
STNW 28
LDLW 16
LDLW -4
NCHECK 18
STNW 2292
!   RETURN p;
LDLW -4
RETURN
END

PROC tFibTree4.Left 0 3 0x00100001
! PROCEDURE Left(t: tree): tree; BEGIN RETURN t[0].b[3].x END Left;
LDLW 12
NCHECK 22
LDNW 28
RETURN
END

PROC tFibTree4.Right 0 3 0x00100001
! PROCEDURE Right(t: tree): tree; BEGIN RETURN t[6].c END Right;
LDLW 12
NCHECK 23
LDNW 2292
RETURN
END

PROC tFibTree4.Build 0 3 0
! PROCEDURE Build(n: INTEGER): tree;
!   IF n <= 1 THEN
LDLW 12
CONST 1
JGT L6
!     SYSTEM.GC;
GLOBAL SYSTEM.GC
CALL 0
!     RETURN NIL
CONST 0
RETURN
LABEL L6
!     RETURN Cons(Build(n-2), Build(n-1))
LDLW 12
DEC
GLOBAL tFibTree4.Build
CALLW 1
LDLW 12
CONST 2
MINUS
GLOBAL tFibTree4.Build
STKMAP 0x00000005
CALLW 1
GLOBAL tFibTree4.Cons
CALLW 2
RETURN
END

PROC tFibTree4.Print 0 2 0x00100001
! PROCEDURE Print(t:tree);
!   IF t = NIL THEN
LDLW 12
JNEQZ L10
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
GLOBAL tFibTree4.Left
CALLW 1
GLOBAL tFibTree4.Print
CALL 1
!     Print(Right(t));
LDLW 12
GLOBAL tFibTree4.Right
CALLW 1
GLOBAL tFibTree4.Print
CALL 1
!     Out.Char(')')
CONST 41
ALIGNC
GLOBAL Out.Char
CALL 1
RETURN
END

PROC tFibTree4.count 0 3 0x00100001
! PROCEDURE count(t:tree): INTEGER;
!   IF t = NIL THEN
LDLW 12
JNEQZ L13
!     RETURN 1
CONST 1
RETURN
LABEL L13
!     RETURN count(Left(t)) + count(Right(t))
LDLW 12
GLOBAL tFibTree4.Left
CALLW 1
GLOBAL tFibTree4.count
CALLW 1
LDLW 12
GLOBAL tFibTree4.Right
CALLW 1
GLOBAL tFibTree4.count
CALLW 1
PLUS
RETURN
END

PROC tFibTree4.%main 0 3 0
!   FOR i := 0 TO 7 DO
CONST 0
STGW tFibTree4.i
LABEL L14
LDGW tFibTree4.i
CONST 7
JGT L15
!     p := Build(i);
LDGW tFibTree4.i
GLOBAL tFibTree4.Build
CALLW 1
STGW tFibTree4.p
!     SYSTEM.GC;
GLOBAL SYSTEM.GC
CALL 0
!     Print(p); Out.Ln();
LDGW tFibTree4.p
GLOBAL tFibTree4.Print
CALL 1
GLOBAL Out.Ln
CALL 0
!     Out.String("Count = "); Out.Int(count(p), 0); 
CONST 9
GLOBAL tFibTree4.%1
GLOBAL Out.String
CALL 2
CONST 0
LDGW tFibTree4.p
GLOBAL tFibTree4.count
CALLW 1
GLOBAL Out.Int
CALL 2
!     Out.Ln(); Out.Ln();
GLOBAL Out.Ln
CALL 0
GLOBAL Out.Ln
CALL 0
!   FOR i := 0 TO 7 DO
LDGW tFibTree4.i
INC
STGW tFibTree4.i
JUMP L14
LABEL L15
RETURN
END

! Global variables
GLOVAR tFibTree4.i 4
GLOVAR tFibTree4.p 4

! Global pointer map
DEFINE tFibTree4.%gcmap
WORD GC_POINTER
WORD tFibTree4.p
WORD GC_END

! String "Count = "
DEFINE tFibTree4.%1
STRING 436F756E74203D2000

! Descriptor for *anon*
DEFINE tFibTree4.%2
WORD 0x00000003
WORD 0
WORD tFibTree4.%2.%anc

DEFINE tFibTree4.%2.%anc
WORD tFibTree4.%2

! Descriptor for *anon*
DEFINE tFibTree4.%3
WORD tFibTree4.%3.%map
WORD 0
WORD tFibTree4.%3.%anc

DEFINE tFibTree4.%3.%anc
WORD tFibTree4.%3

! Descriptor for node
DEFINE tFibTree4.node
WORD tFibTree4.node.%map

! Pointer maps
DEFINE tFibTree4.%3.%map
WORD GC_REPEAT
WORD 4
WORD 40
WORD 8
WORD 0
WORD GC_END
WORD 324
WORD GC_END

DEFINE tFibTree4.node.%map
WORD GC_REPEAT
WORD 0
WORD 8
WORD 328
WORD GC_REPEAT
WORD 4
WORD 40
WORD 8
WORD 0
WORD GC_END
WORD 324
WORD GC_END
WORD GC_END

! End of file
]]*)
