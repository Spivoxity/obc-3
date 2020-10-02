MODULE tTypeCase07;

IMPORT Out;

TYPE
  Obj* = POINTER TO Empty;
  Empty = RECORD (*nothing*) END;

  OpObj = POINTER TO OpNode;
  OpNode = RECORD (Empty)
	name : CHAR;
	left, right : Obj
    END;

PROCEDURE doeval (ex: Obj): INTEGER;
  VAR val: INTEGER;
BEGIN
  CASE ex OF
      OpObj:
        CASE ex.name OF
          "+" : val := doeval(ex.left) + doeval(ex.right)
        END
    | Obj: val := 3
  END
RETURN val
END doeval;

VAR p: Obj; q: OpObj;

BEGIN
  NEW(p); NEW(q);
  q.name := "+"; q.left := p; q.right := p;
  Out.Int(doeval(q), 0); Out.Ln
END tTypeCase07.

(*<<
6
>>*)

(*[[
!! (SYMFILE #tTypeCase07 STAMP #tTypeCase07.%main 1 #tTypeCase07.m)
!! (TYPE #Obj* !1 (POINTER))
!! (TARGET =1 !2 (RECORD #tTypeCase07.Empty 0 VOID))
!! (CHKSUM STAMP)
!! 
MODULE tTypeCase07 STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tTypeCase07.doeval 4 4 0x00100001
! PROCEDURE doeval (ex: Obj): INTEGER;
!   CASE ex OF
LDLW 12
NCHECK 18
CONST -4
OFFSET
DUP 0
LOADW
DUP 0
LDNW 4
CONST 1
JGEQ L7
POP 1
JUMP L2
LABEL L7
LDNW 8
LDNW 4
GLOBAL tTypeCase07.OpNode
JNEQ L2
POP 1
!         CASE ex.name OF
LDLW 12
NCHECK 20
LOADC
CONST 43
JNEQ L4
!           "+" : val := doeval(ex.left) + doeval(ex.right)
LDLW 12
NCHECK 21
LDNW 4
GLOBAL tTypeCase07.doeval
CALLW 1
LDLW 12
NCHECK 21
LDNW 8
GLOBAL tTypeCase07.doeval
CALLW 1
PLUS
STLW -4
JUMP L1
LABEL L4
ERROR E_CASE 20
JUMP L1
LABEL L2
DUP 0
LOADW
DUP 0
LDNW 4
JGEQZ L10
POP 1
JUMP L8
LABEL L10
LDNW 8
LOADW
GLOBAL tTypeCase07.Empty
JNEQ L8
POP 1
!     | Obj: val := 3
CONST 3
STLW -4
JUMP L1
LABEL L8
POP 1
ERROR E_CASE 18
LABEL L1
! RETURN val
LDLW -4
RETURN
END

PROC tTypeCase07.%main 0 3 0
!   NEW(p); NEW(q);
CONST 0
GLOBAL tTypeCase07.Empty
GLOBAL NEW
CALLW 2
STGW tTypeCase07.p
CONST 12
GLOBAL tTypeCase07.OpNode
GLOBAL NEW
CALLW 2
STGW tTypeCase07.q
!   q.name := "+"; q.left := p; q.right := p;
CONST 43
LDGW tTypeCase07.q
NCHECK 32
STOREC
LDGW tTypeCase07.p
LDGW tTypeCase07.q
NCHECK 32
STNW 4
LDGW tTypeCase07.p
LDGW tTypeCase07.q
NCHECK 32
STNW 8
!   Out.Int(doeval(q), 0); Out.Ln
CONST 0
LDGW tTypeCase07.q
GLOBAL tTypeCase07.doeval
CALLW 1
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tTypeCase07.p 4
GLOVAR tTypeCase07.q 4

! Global pointer map
DEFINE tTypeCase07.%gcmap
WORD GC_POINTER
WORD tTypeCase07.p
WORD GC_POINTER
WORD tTypeCase07.q
WORD GC_END

! Descriptor for Empty
DEFINE tTypeCase07.Empty
WORD 0
WORD 0
WORD tTypeCase07.Empty.%anc

DEFINE tTypeCase07.Empty.%anc
WORD tTypeCase07.Empty

! Descriptor for OpNode
DEFINE tTypeCase07.OpNode
WORD 0x0000000d
WORD 1
WORD tTypeCase07.OpNode.%anc

DEFINE tTypeCase07.OpNode.%anc
WORD tTypeCase07.Empty
WORD tTypeCase07.OpNode

! End of file
]]*)
