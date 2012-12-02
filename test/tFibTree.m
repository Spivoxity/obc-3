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

IMPORT Out, GC;

TYPE 
  tree = POINTER TO node;
  node = RECORD left, right: tree END;

PROCEDURE Build(n: INTEGER): tree;
  VAR t: tree;
BEGIN
  IF n <= 1 THEN
    GC.Collect;
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
    GC.Collect;
    Print(p); Out.Ln();
    Out.String("Count = "); Out.Int(count(p), 0); 
    Out.Ln(); Out.Ln();
  END
END tFibTree.

(*[[
!! SYMFILE #tFibTree STAMP #tFibTree.%main 1
!! END STAMP
!! 
MODULE tFibTree STAMP 0
IMPORT Out STAMP
IMPORT GC STAMP
ENDHDR

PROC tFibTree.Build 1 16 0x00010001
! PROCEDURE Build(n: INTEGER): tree;
!   IF n <= 1 THEN
LDLW 12
CONST 1
JGT 3
!     GC.Collect;
CONST GC.Collect
CALL 0
!     RETURN NIL
CONST 0
RETURNW
LABEL 3
!     NEW(t);
CONST 8
CONST tFibTree.node
LOCAL -4
CONST NEW
CALL 3
!     t.left := Build(n-2);
LDLW 12
CONST 2
MINUS
CONST tFibTree.Build
CALLW 1
LDLW -4
NCHECK 44
STOREW
!     t.right := Build(n-1);
LDLW 12
DEC
CONST tFibTree.Build
CALLW 1
LDLW -4
NCHECK 45
STNW 4
!     RETURN t
LDLW -4
RETURNW
END

PROC tFibTree.Print 0 16 0x00100001
! PROCEDURE Print(t:tree);
!   IF NIL = t THEN
LDLW 12
JNEQZ 5
!     Out.Char('.')
CONST 46
ALIGNC
CONST Out.Char
CALL 1
RETURN
LABEL 5
!     Out.Char('(');
CONST 40
ALIGNC
CONST Out.Char
CALL 1
!     Print(t.left);
LDLW 12
NCHECK 56
LOADW
CONST tFibTree.Print
CALL 1
!     Print(t.right);
LDLW 12
NCHECK 57
LDNW 4
CONST tFibTree.Print
CALL 1
!     Out.Char(')')
CONST 41
ALIGNC
CONST Out.Char
CALL 1
RETURN
END

PROC tFibTree.count 0 16 0x00100001
! PROCEDURE count(t:tree): INTEGER;
!   IF t = NIL THEN
LDLW 12
JNEQZ 7
!     RETURN 1
CONST 1
RETURNW
LABEL 7
!     RETURN count(t.left) + count(t.right)
LDLW 12
NCHECK 67
LOADW
CONST tFibTree.count
CALLW 1
LDLW 12
NCHECK 67
LDNW 4
CONST tFibTree.count
CALLW 1
PLUS
RETURNW
END

PROC tFibTree.%main 0 16 0
!   FOR i := 0 TO 7 DO
CONST 0
STGW tFibTree.i
JUMP 9
LABEL 8
!     p := Build(i);
LDGW tFibTree.i
CONST tFibTree.Build
CALLW 1
STGW tFibTree.p
!     GC.Collect;
CONST GC.Collect
CALL 0
!     Print(p); Out.Ln();
LDGW tFibTree.p
CONST tFibTree.Print
CALL 1
CONST Out.Ln
CALL 0
!     Out.String("Count = "); Out.Int(count(p), 0); 
CONST 9
CONST tFibTree.%1
CONST Out.String
CALL 2
CONST 0
LDGW tFibTree.p
CONST tFibTree.count
CALLW 1
CONST Out.Int
CALL 2
!     Out.Ln(); Out.Ln();
CONST Out.Ln
CALL 0
CONST Out.Ln
CALL 0
!   FOR i := 0 TO 7 DO
LDGW tFibTree.i
INC
STGW tFibTree.i
LABEL 9
LDGW tFibTree.i
CONST 7
JLEQ 8
RETURN
END

! Global variables
GLOBAL tFibTree.i 4
GLOBAL tFibTree.p 4

! Pointer map
DEFINE tFibTree.%gcmap
WORD GC_BASE
WORD tFibTree.p
WORD 0
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
