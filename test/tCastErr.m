MODULE tCastErr;

TYPE r0 = RECORD END; rp1 = POINTER TO RECORD (r0) END;

VAR p0: POINTER TO r0; p1: rp1;

BEGIN
  NEW(p0);
  p1 := p0(rp1)
END tCastErr.

(*<<
Runtime error: dynamic type error in cast on line 9 in module tCastErr
In procedure tCastErr.%main
   called from MAIN
>>*)

(*[[
!! (SYMFILE #tCastErr STAMP #tCastErr.%main 1 #tCastErr.m)
!! (CHKSUM STAMP)
!! 
MODULE tCastErr STAMP 0
ENDHDR

PROC tCastErr.%main 0 4 0
!   NEW(p0);
CONST 0
GLOBAL tCastErr.r0
GLOBAL NEW
CALLW 2
STGW tCastErr.p0
!   p1 := p0(rp1)
LDGW tCastErr.p0
DUP 0
NCHECK 9
LDNW -4
DUP 0
LDNW 4
CONST 1
JGEQ L4
POP 1
JUMP L3
LABEL L4
LDNW 8
LDNW 4
GLOBAL tCastErr.%1
JEQ L2
LABEL L3
ERROR E_CAST 9
LABEL L2
STGW tCastErr.p1
RETURN
END

! Global variables
GLOVAR tCastErr.p0 4
GLOVAR tCastErr.p1 4

! Global pointer map
DEFINE tCastErr.%gcmap
WORD GC_POINTER
WORD tCastErr.p0
WORD GC_POINTER
WORD tCastErr.p1
WORD GC_END

! Descriptor for r0
DEFINE tCastErr.r0
WORD 0
WORD 0
WORD tCastErr.r0.%anc

DEFINE tCastErr.r0.%anc
WORD tCastErr.r0

! Descriptor for *anon*
DEFINE tCastErr.%1
WORD 0
WORD 1
WORD tCastErr.%1.%anc

DEFINE tCastErr.%1.%anc
WORD tCastErr.r0
WORD tCastErr.%1

! End of file
]]*)
