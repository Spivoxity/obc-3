MODULE tInorder;

IMPORT Random, Out;

TYPE 
  Tree = POINTER TO Node;
  Node = RECORD data: INTEGER; left, right: Tree; END;

PROCEDURE Traverse1(t: Tree);
BEGIN
  IF t # NIL THEN
    Traverse1(t.left);
    Out.Int(t.data, 4);
    Traverse1(t.right)
  END
END Traverse1;

PROCEDURE Traverse2(t: Tree);
  VAR 
    u: Tree;
    sp: INTEGER;
    stack: ARRAY 1000 OF Tree;
BEGIN
  u := t; sp := 0;

  (* Invariant: 
      trav t = output ++ trav u 
                ++ concat [r.data : trav r.right | r <- rev stack] *)
  LOOP
    WHILE u # NIL DO
      stack[sp] := u; sp := sp+1;
      u := u.left
    END;

    IF sp = 0 THEN EXIT END;

    sp := sp-1; u := stack[sp];
    Out.Int(u.data, 4);
    u := u.right
  END
END Traverse2;

PROCEDURE RandTree(depth: INTEGER): Tree;
  VAR t: Tree;
BEGIN
  IF Random.Roll(8) < depth THEN RETURN NIL END;

  NEW(t);
  t.data := Random.Roll(1000);
  t.left := RandTree(depth+1);
  t.right := RandTree(depth+1);
  RETURN t
END RandTree;

VAR t: Tree;

BEGIN
  t := RandTree(0);
  Traverse1(t); Out.Ln;
  Traverse2(t); Out.Ln
END tInorder.

(*<<
 157  70 520 690  70 834 674 901 411 409 399 742 882 261 753 469 959 129
 157  70 520 690  70 834 674 901 411 409 399 742 882 261 753 469 959 129
>>*)

(*[[
!! SYMFILE #tInorder STAMP #tInorder.%main 1
!! END STAMP
!! 
MODULE tInorder STAMP 0
IMPORT Random STAMP
IMPORT Out STAMP
ENDHDR

PROC tInorder.Traverse1 0 3 0x00100001
! PROCEDURE Traverse1(t: Tree);
!   IF t # NIL THEN
LDLW 12
JEQZ 2
!     Traverse1(t.left);
LDLW 12
NCHECK 12
LDNW 4
GLOBAL tInorder.Traverse1
CALL 1
!     Out.Int(t.data, 4);
CONST 4
LDLW 12
NCHECK 13
LOADW
GLOBAL Out.Int
CALL 2
!     Traverse1(t.right)
LDLW 12
NCHECK 14
LDNW 8
GLOBAL tInorder.Traverse1
CALL 1
LABEL 2
RETURN
END

PROC tInorder.Traverse2 1002 4 tInorder.Traverse2.%map
! PROCEDURE Traverse2(t: Tree);
!   u := t; sp := 0;
LDLW 12
STLW -4
CONST 0
STLW -8
JUMP 6
LABEL 5
!       stack[sp] := u; sp := sp+1;
LDLW -4
LOCAL -4008
LDLW -8
CONST 1000
BOUND 31
STIW
INCL -8
!       u := u.left
LDLW -4
NCHECK 32
LDNW 4
STLW -4
LABEL 6
!     WHILE u # NIL DO
LDLW -4
JNEQZ 5
!     IF sp = 0 THEN EXIT END;
LDLW -8
JEQZ 4
!     sp := sp-1; u := stack[sp];
DECL -8
LOCAL -4008
LDLW -8
CONST 1000
BOUND 37
LDIW
STLW -4
!     Out.Int(u.data, 4);
CONST 4
LDLW -4
NCHECK 38
LOADW
GLOBAL Out.Int
CALL 2
!     u := u.right
LDLW -4
NCHECK 39
LDNW 8
STLW -4
JUMP 6
LABEL 4
RETURN
END

PROC tInorder.RandTree 1 4 0x00010001
! PROCEDURE RandTree(depth: INTEGER): Tree;
!   IF Random.Roll(8) < depth THEN RETURN NIL END;
CONST 8
GLOBAL Random.Roll
CALLW 1
LDLW 12
JGEQ 10
CONST 0
RETURNW
LABEL 10
!   NEW(t);
CONST 12
GLOBAL tInorder.Node
LOCAL -4
GLOBAL NEW
CALL 3
!   t.data := Random.Roll(1000);
CONST 1000
GLOBAL Random.Roll
CALLW 1
LDLW -4
NCHECK 49
STOREW
!   t.left := RandTree(depth+1);
LDLW 12
INC
GLOBAL tInorder.RandTree
CALLW 1
LDLW -4
NCHECK 50
STNW 4
!   t.right := RandTree(depth+1);
LDLW 12
INC
GLOBAL tInorder.RandTree
CALLW 1
LDLW -4
NCHECK 51
STNW 8
!   RETURN t
LDLW -4
RETURNW
END

PROC tInorder.%main 0 4 0
!   t := RandTree(0);
CONST 0
GLOBAL tInorder.RandTree
CALLW 1
STGW tInorder.t
!   Traverse1(t); Out.Ln;
LDGW tInorder.t
GLOBAL tInorder.Traverse1
CALL 1
GLOBAL Out.Ln
CALL 0
!   Traverse2(t); Out.Ln
LDGW tInorder.t
GLOBAL tInorder.Traverse2
CALL 1
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tInorder.t 4

! Pointer map
DEFINE tInorder.%gcmap
WORD GC_BASE
WORD tInorder.t
WORD 0
WORD GC_END

! Descriptor for Node
DEFINE tInorder.Node
WORD 0x0000000d
WORD 0
WORD tInorder.Node.%anc

DEFINE tInorder.Node.%anc
WORD tInorder.Node

! Pointer maps
DEFINE tInorder.Traverse2.%map
WORD 76
WORD 60
WORD GC_BLOCK
WORD -3944
WORD 1000
WORD GC_END

! End of file
]]*)
