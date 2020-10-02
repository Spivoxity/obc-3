MODULE tPtrVal07;

(*<<
2
>>*)

IMPORT Out;

TYPE ptr = POINTER TO RECORD x: INTEGER END;

PROCEDURE p(x: ptr);
BEGIN
  NEW(x); x.x := 3
END p;

VAR q: ptr;

BEGIN
  NEW(q); q.x := 2; 
  p(q);
  Out.Int(q.x, 0); Out.Ln
END tPtrVal07.  

(*[[
!! (SYMFILE #tPtrVal07 STAMP #tPtrVal07.%main 1 #tPtrVal07.m)
!! (CHKSUM STAMP)
!! 
MODULE tPtrVal07 STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tPtrVal07.p 0 3 0x00100001
! PROCEDURE p(x: ptr);
!   NEW(x); x.x := 3
CONST 4
GLOBAL tPtrVal07.%1
GLOBAL NEW
CALLW 2
STLW 12
CONST 3
LDLW 12
NCHECK 13
STOREW
RETURN
END

PROC tPtrVal07.%main 0 3 0
!   NEW(q); q.x := 2; 
CONST 4
GLOBAL tPtrVal07.%1
GLOBAL NEW
CALLW 2
STGW tPtrVal07.q
CONST 2
LDGW tPtrVal07.q
NCHECK 19
STOREW
!   p(q);
LDGW tPtrVal07.q
GLOBAL tPtrVal07.p
CALL 1
!   Out.Int(q.x, 0); Out.Ln
CONST 0
LDGW tPtrVal07.q
NCHECK 21
LOADW
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tPtrVal07.q 4

! Global pointer map
DEFINE tPtrVal07.%gcmap
WORD GC_POINTER
WORD tPtrVal07.q
WORD GC_END

! Descriptor for *anon*
DEFINE tPtrVal07.%1
WORD 0
WORD 0
WORD tPtrVal07.%1.%anc

DEFINE tPtrVal07.%1.%anc
WORD tPtrVal07.%1

! End of file
]]*)
