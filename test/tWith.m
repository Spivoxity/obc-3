MODULE tWith;

(* Unlike Wirth's compilers, WITH is a kind of local redeclaration *)

IMPORT Out;

TYPE
  Obj* = POINTER TO Empty;
  Empty = RECORD (*nothing*) END;

  OpObj = POINTER TO OpNode;
  OpNode = RECORD (Empty)
	name : CHAR;
	left, right : Obj;
    END;

PROCEDURE doeval (ex: Obj): INTEGER;
BEGIN
  WITH ex : OpObj DO
    CASE ex.name OF
      "+" : RETURN doeval(ex.left) + doeval(ex.right)
    END
  ELSE
    RETURN 3
  END;
END doeval;

VAR p: Obj; q: OpObj;

BEGIN
  NEW(p); NEW(q);
  q.name := "+"; q.left := p; q.right := p;
  Out.Int(doeval(q), 0); Out.Ln
END tWith.

(*<<
6
>>*)

(*[[
!! (SYMFILE #tWith STAMP #tWith.%main 1 #tWith.m)
!! (TYPE #Obj* !1 (POINTER))
!! (TARGET =1 !2 (RECORD #tWith.Empty 0 VOID))
!! (CHKSUM STAMP)
!! 
MODULE tWith STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tWith.doeval 0 3 0x00100001
! PROCEDURE doeval (ex: Obj): INTEGER;
!   WITH ex : OpObj DO
LDLW 12
NCHECK 19
LDNW -4
DUP 0
LDNW 4
CONST 1
JGEQ L7
POP 1
JUMP L2
LABEL L7
LDNW 8
LDNW 4
GLOBAL tWith.OpNode
JNEQ L2
!     CASE ex.name OF
LDLW 12
NCHECK 20
LOADC
CONST 43
JNEQ L4
!       "+" : RETURN doeval(ex.left) + doeval(ex.right)
LDLW 12
NCHECK 21
LDNW 4
GLOBAL tWith.doeval
CALLW 1
LDLW 12
NCHECK 21
LDNW 8
GLOBAL tWith.doeval
CALLW 1
PLUS
RETURN
LABEL L4
ERROR E_CASE 20
JUMP L1
LABEL L2
!     RETURN 3
CONST 3
RETURN
LABEL L1
ERROR E_RETURN 17
END

PROC tWith.%main 0 3 0
!   NEW(p); NEW(q);
CONST 0
GLOBAL tWith.Empty
GLOBAL NEW
CALLW 2
STGW tWith.p
CONST 12
GLOBAL tWith.OpNode
GLOBAL NEW
CALLW 2
STGW tWith.q
!   q.name := "+"; q.left := p; q.right := p;
CONST 43
LDGW tWith.q
NCHECK 32
STOREC
LDGW tWith.p
LDGW tWith.q
NCHECK 32
STNW 4
LDGW tWith.p
LDGW tWith.q
NCHECK 32
STNW 8
!   Out.Int(doeval(q), 0); Out.Ln
CONST 0
LDGW tWith.q
GLOBAL tWith.doeval
CALLW 1
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tWith.p 4
GLOVAR tWith.q 4

! Global pointer map
DEFINE tWith.%gcmap
WORD GC_POINTER
WORD tWith.p
WORD GC_POINTER
WORD tWith.q
WORD GC_END

! Descriptor for Empty
DEFINE tWith.Empty
WORD 0
WORD 0
WORD tWith.Empty.%anc

DEFINE tWith.Empty.%anc
WORD tWith.Empty

! Descriptor for OpNode
DEFINE tWith.OpNode
WORD 0x0000000d
WORD 1
WORD tWith.OpNode.%anc

DEFINE tWith.OpNode.%anc
WORD tWith.Empty
WORD tWith.OpNode

! End of file
]]*)
