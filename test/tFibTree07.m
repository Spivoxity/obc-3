MODULE tFibTree07;

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

IMPORT Out, SYSTEM;

TYPE 
  tree = POINTER TO node;
  node = RECORD left, right: tree END;

PROCEDURE Build(n: INTEGER): tree;
  VAR t: tree;
BEGIN
  IF n <= 1 THEN
    SYSTEM.GC;
    t := NIL
  ELSE
    NEW(t);
    t.left := Build(n-2);
    t.right := Build(n-1);
  END
RETURN t
END Build;

PROCEDURE Print(t:tree);
BEGIN
  IF NIL = t THEN
    Out.Char('.')
  ELSE
    Out.Char('(');
    Print(t.left);
    Print(t.right);
    Out.Char(')')
  END
END Print;

PROCEDURE count(t:tree): INTEGER;
  VAR c: INTEGER;
BEGIN
  IF t = NIL THEN
    c := 1
  ELSE
    c := count(t.left) + count(t.right)
  END
RETURN c
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
END tFibTree07.

(*[[
!! (SYMFILE #tFibTree07 STAMP #tFibTree07.%main 1 #tFibTree07.m)
!! (CHKSUM STAMP)
!! 
MODULE tFibTree07 STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tFibTree07.Build 4 3 0x00010001
! PROCEDURE Build(n: INTEGER): tree;
!   IF n <= 1 THEN
LDLW 12
CONST 1
JGT L4
!     SYSTEM.GC;
GLOBAL SYSTEM.GC
CALL 0
!     t := NIL
CONST 0
STLW -4
JUMP L2
LABEL L4
!     NEW(t);
CONST 8
GLOBAL tFibTree07.node
GLOBAL NEW
CALLW 2
STLW -4
!     t.left := Build(n-2);
LDLW 12
CONST 2
MINUS
GLOBAL tFibTree07.Build
CALLW 1
LDLW -4
NCHECK 44
STOREW
!     t.right := Build(n-1);
LDLW 12
DEC
GLOBAL tFibTree07.Build
CALLW 1
LDLW -4
NCHECK 45
STNW 4
LABEL L2
! RETURN t
LDLW -4
RETURN
END

PROC tFibTree07.Print 0 2 0x00100001
! PROCEDURE Print(t:tree);
!   IF NIL = t THEN
LDLW 12
JNEQZ L7
!     Out.Char('.')
CONST 46
ALIGNC
GLOBAL Out.Char
CALL 1
RETURN
LABEL L7
!     Out.Char('(');
CONST 40
ALIGNC
GLOBAL Out.Char
CALL 1
!     Print(t.left);
LDLW 12
NCHECK 56
LOADW
GLOBAL tFibTree07.Print
CALL 1
!     Print(t.right);
LDLW 12
NCHECK 57
LDNW 4
GLOBAL tFibTree07.Print
CALL 1
!     Out.Char(')')
CONST 41
ALIGNC
GLOBAL Out.Char
CALL 1
RETURN
END

PROC tFibTree07.count 4 3 0x00100001
! PROCEDURE count(t:tree): INTEGER;
!   IF t = NIL THEN
LDLW 12
JNEQZ L10
!     c := 1
CONST 1
STLW -4
JUMP L8
LABEL L10
!     c := count(t.left) + count(t.right)
LDLW 12
NCHECK 68
LOADW
GLOBAL tFibTree07.count
CALLW 1
LDLW 12
NCHECK 68
LDNW 4
GLOBAL tFibTree07.count
CALLW 1
PLUS
STLW -4
LABEL L8
! RETURN c
LDLW -4
RETURN
END

PROC tFibTree07.%main 0 3 0
!   FOR i := 0 TO 7 DO
CONST 0
STGW tFibTree07.i
LABEL L11
LDGW tFibTree07.i
CONST 7
JGT L12
!     p := Build(i);
LDGW tFibTree07.i
GLOBAL tFibTree07.Build
CALLW 1
STGW tFibTree07.p
!     SYSTEM.GC;
GLOBAL SYSTEM.GC
CALL 0
!     Print(p); Out.Ln();
LDGW tFibTree07.p
GLOBAL tFibTree07.Print
CALL 1
GLOBAL Out.Ln
CALL 0
!     Out.String("Count = "); Out.Int(count(p), 0); 
CONST 9
GLOBAL tFibTree07.%1
GLOBAL Out.String
CALL 2
CONST 0
LDGW tFibTree07.p
GLOBAL tFibTree07.count
CALLW 1
GLOBAL Out.Int
CALL 2
!     Out.Ln(); Out.Ln();
GLOBAL Out.Ln
CALL 0
GLOBAL Out.Ln
CALL 0
!   FOR i := 0 TO 7 DO
LDGW tFibTree07.i
INC
STGW tFibTree07.i
JUMP L11
LABEL L12
RETURN
END

! Global variables
GLOVAR tFibTree07.i 4
GLOVAR tFibTree07.p 4

! Global pointer map
DEFINE tFibTree07.%gcmap
WORD GC_POINTER
WORD tFibTree07.p
WORD GC_END

! String "Count = "
DEFINE tFibTree07.%1
STRING 436F756E74203D2000

! Descriptor for node
DEFINE tFibTree07.node
WORD 0x00000007
WORD 0
WORD tFibTree07.node.%anc

DEFINE tFibTree07.node.%anc
WORD tFibTree07.node

! End of file
]]*)
