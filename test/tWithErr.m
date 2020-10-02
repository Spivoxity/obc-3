MODULE tWithErr;

IMPORT Out;

TYPE r0 = RECORD END; rp1 = POINTER TO RECORD (r0) END;

VAR p0: POINTER TO r0;

BEGIN
  NEW(p0);
  WITH p0: rp1 DO Out.String("Failed!"); Out.Ln END
END tWithErr.

(*<<
Runtime error: no matching type guard in WITH statement on line 11 in module tWithErr
In procedure tWithErr.%main
   called from MAIN
>>*)

(*[[
!! (SYMFILE #tWithErr STAMP #tWithErr.%main 1 #tWithErr.m)
!! (CHKSUM STAMP)
!! 
MODULE tWithErr STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tWithErr.%main 0 3 0
!   NEW(p0);
CONST 0
GLOBAL tWithErr.r0
GLOBAL NEW
CALLW 2
STGW tWithErr.p0
!   WITH p0: rp1 DO Out.String("Failed!"); Out.Ln END
LDGW tWithErr.p0
NCHECK 11
LDNW -4
DUP 0
LDNW 4
CONST 1
JGEQ L6
POP 1
JUMP L4
LABEL L6
LDNW 8
LDNW 4
GLOBAL tWithErr.%2
JNEQ L4
CONST 8
GLOBAL tWithErr.%1
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
LABEL L4
ERROR E_WITH 11
RETURN
END

! Global variables
GLOVAR tWithErr.p0 4

! Global pointer map
DEFINE tWithErr.%gcmap
WORD GC_POINTER
WORD tWithErr.p0
WORD GC_END

! String "Failed!"
DEFINE tWithErr.%1
STRING 4661696C65642100

! Descriptor for r0
DEFINE tWithErr.r0
WORD 0
WORD 0
WORD tWithErr.r0.%anc

DEFINE tWithErr.r0.%anc
WORD tWithErr.r0

! Descriptor for *anon*
DEFINE tWithErr.%2
WORD 0
WORD 1
WORD tWithErr.%2.%anc

DEFINE tWithErr.%2.%anc
WORD tWithErr.r0
WORD tWithErr.%2

! End of file
]]*)
