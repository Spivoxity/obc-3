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

PROCEDURE GcDebug(flags: ARRAY OF CHAR) IS "gc_debug";

BEGIN 
  GcDebug("gs");

  FOR i := 0 TO 7 DO
    p := Build(i);
    SYSTEM.GC;
    Print(p); Out.Ln();
    Out.String("Count = "); Out.Int(count(p), 0); 
    Out.Ln(); Out.Ln();
  END
END tFibTree4.

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
!! (SYMFILE #tFibTree4 STAMP #tFibTree4.%main 1)
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
RETURNW
END

PROC tFibTree4.Left 0 4 0x00100001
! PROCEDURE Left(t: tree): tree; BEGIN RETURN t[0].b[3].x END Left;
LDLW 12
NCHECK 22
LDNW 28
RETURNW
END

PROC tFibTree4.Right 0 4 0x00100001
! PROCEDURE Right(t: tree): tree; BEGIN RETURN t[6].c END Right;
LDLW 12
NCHECK 23
LDNW 2292
RETURNW
END

PROC tFibTree4.Build 0 4 0
! PROCEDURE Build(n: INTEGER): tree;
!   IF n <= 1 THEN
LDLW 12
CONST 1
JGT L7
!     SYSTEM.GC;
GLOBAL SYSTEM.GC
CALL 0
!     RETURN NIL
CONST 0
RETURNW
LABEL L7
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
RETURNW
END

PROC tFibTree4.Print 0 4 0x00100001
! PROCEDURE Print(t:tree);
!   IF t = NIL THEN
LDLW 12
JNEZ L11
!     Out.Char('.')
CONST 46
ALIGNC
GLOBAL Out.Char
CALL 1
RETURN
LABEL L11
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

PROC tFibTree4.count 0 4 0x00100001
! PROCEDURE count(t:tree): INTEGER;
!   IF t = NIL THEN
LDLW 12
JNEZ L14
!     RETURN 1
CONST 1
RETURNW
LABEL L14
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
RETURNW
END

PRIMDEF tFibTree4.GcDebug gc_debug VX

PROC tFibTree4.%main 0 4 0
!   GcDebug("gs");
CONST 3
GLOBAL tFibTree4.%1
GLOBAL tFibTree4.GcDebug
CALL 2
!   FOR i := 0 TO 7 DO
CONST 0
STGW tFibTree4.i
LABEL L15
LDGW tFibTree4.i
CONST 7
JGT L16
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
GLOBAL tFibTree4.%2
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
JUMP L15
LABEL L16
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

! String "gs"
DEFINE tFibTree4.%1
STRING 677300

! String "Count = "
DEFINE tFibTree4.%2
STRING 436F756E74203D2000

! Descriptor for *anon*
DEFINE tFibTree4.%3
WORD 0x00000003
WORD 0
WORD tFibTree4.%3.%anc

DEFINE tFibTree4.%3.%anc
WORD tFibTree4.%3

! Descriptor for *anon*
DEFINE tFibTree4.%4
WORD tFibTree4.%4.%map
WORD 0
WORD tFibTree4.%4.%anc

DEFINE tFibTree4.%4.%anc
WORD tFibTree4.%4

! Descriptor for node
DEFINE tFibTree4.node
WORD tFibTree4.node.%map

! Pointer maps
DEFINE tFibTree4.%4.%map
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
