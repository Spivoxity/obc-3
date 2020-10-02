MODULE tFibTree;

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
    RETURN NIL
  ELSE
    NEW(t);
    t.left := Build(n-2);
    t.right := Build(n-1);
    RETURN t
  END
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
BEGIN
  IF t = NIL THEN
    RETURN 1
  ELSE
    RETURN count(t.left) + count(t.right)
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
END tFibTree.

(*[[
!! (SYMFILE #tFibTree STAMP #tFibTree.%main 1 #tFibTree.m)
!! (CHKSUM STAMP)
!! 
MODULE tFibTree STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tFibTree.Build 4 3 0x00010001
! PROCEDURE Build(n: INTEGER): tree;
!   IF n <= 1 THEN
LDLW 12
CONST 1
JGT L4
!     SYSTEM.GC;
GLOBAL SYSTEM.GC
CALL 0
!     RETURN NIL
CONST 0
RETURN
LABEL L4
!     NEW(t);
CONST 8
GLOBAL tFibTree.node
GLOBAL NEW
CALLW 2
STLW -4
!     t.left := Build(n-2);
LDLW 12
CONST 2
MINUS
GLOBAL tFibTree.Build
CALLW 1
LDLW -4
NCHECK 44
STOREW
!     t.right := Build(n-1);
LDLW 12
DEC
GLOBAL tFibTree.Build
CALLW 1
LDLW -4
NCHECK 45
STNW 4
!     RETURN t
LDLW -4
RETURN
END

PROC tFibTree.Print 0 2 0x00100001
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
GLOBAL tFibTree.Print
CALL 1
!     Print(t.right);
LDLW 12
NCHECK 57
LDNW 4
GLOBAL tFibTree.Print
CALL 1
!     Out.Char(')')
CONST 41
ALIGNC
GLOBAL Out.Char
CALL 1
RETURN
END

PROC tFibTree.count 0 3 0x00100001
! PROCEDURE count(t:tree): INTEGER;
!   IF t = NIL THEN
LDLW 12
JNEQZ L10
!     RETURN 1
CONST 1
RETURN
LABEL L10
!     RETURN count(t.left) + count(t.right)
LDLW 12
NCHECK 67
LOADW
GLOBAL tFibTree.count
CALLW 1
LDLW 12
NCHECK 67
LDNW 4
GLOBAL tFibTree.count
CALLW 1
PLUS
RETURN
END

PROC tFibTree.%main 0 3 0
!   FOR i := 0 TO 7 DO
CONST 0
STGW tFibTree.i
LABEL L11
LDGW tFibTree.i
CONST 7
JGT L12
!     p := Build(i);
LDGW tFibTree.i
GLOBAL tFibTree.Build
CALLW 1
STGW tFibTree.p
!     SYSTEM.GC;
GLOBAL SYSTEM.GC
CALL 0
!     Print(p); Out.Ln();
LDGW tFibTree.p
GLOBAL tFibTree.Print
CALL 1
GLOBAL Out.Ln
CALL 0
!     Out.String("Count = "); Out.Int(count(p), 0); 
CONST 9
GLOBAL tFibTree.%1
GLOBAL Out.String
CALL 2
CONST 0
LDGW tFibTree.p
GLOBAL tFibTree.count
CALLW 1
GLOBAL Out.Int
CALL 2
!     Out.Ln(); Out.Ln();
GLOBAL Out.Ln
CALL 0
GLOBAL Out.Ln
CALL 0
!   FOR i := 0 TO 7 DO
LDGW tFibTree.i
INC
STGW tFibTree.i
JUMP L11
LABEL L12
RETURN
END

! Global variables
GLOVAR tFibTree.i 4
GLOVAR tFibTree.p 4

! Global pointer map
DEFINE tFibTree.%gcmap
WORD GC_POINTER
WORD tFibTree.p
WORD GC_END

! String "Count = "
DEFINE tFibTree.%1
STRING 436F756E74203D2000

! Descriptor for node
DEFINE tFibTree.node
WORD 0x00000007
WORD 0
WORD tFibTree.node.%anc

DEFINE tFibTree.node.%anc
WORD tFibTree.node

! End of file
]]*)
