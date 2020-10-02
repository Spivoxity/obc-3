MODULE tCast;

(* Bug: taking address of a cast in various circumstances *)

IMPORT Out;

TYPE BasePtr = POINTER TO Base;
  Base = RECORD END;

TYPE NodePtr = POINTER TO Node;
  Node = RECORD (Base) val: INTEGER END;

PROCEDURE PrintNode(n: Node);
BEGIN
  Out.Int(n.val, 0); Out.Ln
END PrintNode;

PROCEDURE Print(p: BasePtr); 
BEGIN
  PrintNode(p^(Node));
  Out.Int(p(NodePtr).val, 0); Out.Ln;
  Out.Int(p^(Node).val, 0); Out.Ln
END Print;

VAR a: NodePtr;

BEGIN
  NEW(a); 
  a.val := 17;
  Print(a)
END tCast.

(*<<
17
17
17
>>*)

(*[[
!! (SYMFILE #tCast STAMP #tCast.%main 1 #tCast.m)
!! (CHKSUM STAMP)
!! 
MODULE tCast STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tCast.PrintNode 4 3 0
! PROCEDURE PrintNode(n: Node);
LOCAL -4
LDLW 12
CONST 4
FIXCOPY
!   Out.Int(n.val, 0); Out.Ln
CONST 0
LDLW -4
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tCast.Print 0 5 0x00100001
! PROCEDURE Print(p: BasePtr); 
!   PrintNode(p^(Node));
LDLW 12
DUP 0
NCHECK 20
LDNW -4
DUP 0
LDNW 4
CONST 1
JGEQ L3
POP 1
JUMP L2
LABEL L3
LDNW 8
LDNW 4
GLOBAL tCast.Node
JEQ L1
LABEL L2
ERROR E_CAST 20
LABEL L1
GLOBAL tCast.PrintNode
CALL 1
!   Out.Int(p(NodePtr).val, 0); Out.Ln;
CONST 0
LDLW 12
DUP 0
NCHECK 21
LDNW -4
DUP 0
LDNW 4
CONST 1
JGEQ L6
POP 1
JUMP L5
LABEL L6
LDNW 8
LDNW 4
GLOBAL tCast.Node
JEQ L4
LABEL L5
ERROR E_CAST 21
LABEL L4
LOADW
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!   Out.Int(p^(Node).val, 0); Out.Ln
CONST 0
LDLW 12
DUP 0
NCHECK 22
LDNW -4
DUP 0
LDNW 4
CONST 1
JGEQ L9
POP 1
JUMP L8
LABEL L9
LDNW 8
LDNW 4
GLOBAL tCast.Node
JEQ L7
LABEL L8
ERROR E_CAST 22
LABEL L7
LOADW
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tCast.%main 0 3 0
!   NEW(a); 
CONST 4
GLOBAL tCast.Node
GLOBAL NEW
CALLW 2
STGW tCast.a
!   a.val := 17;
CONST 17
LDGW tCast.a
NCHECK 29
STOREW
!   Print(a)
LDGW tCast.a
GLOBAL tCast.Print
CALL 1
RETURN
END

! Global variables
GLOVAR tCast.a 4

! Global pointer map
DEFINE tCast.%gcmap
WORD GC_POINTER
WORD tCast.a
WORD GC_END

! Descriptor for Base
DEFINE tCast.Base
WORD 0
WORD 0
WORD tCast.Base.%anc

DEFINE tCast.Base.%anc
WORD tCast.Base

! Descriptor for Node
DEFINE tCast.Node
WORD 0
WORD 1
WORD tCast.Node.%anc

DEFINE tCast.Node.%anc
WORD tCast.Base
WORD tCast.Node

! End of file
]]*)
