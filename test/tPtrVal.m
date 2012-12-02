MODULE tPtrVal;

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
END tPtrVal.  

(*[[
!! SYMFILE #tPtrVal STAMP #tPtrVal.%main 1
!! END STAMP
!! 
MODULE tPtrVal STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tPtrVal.p 0 16 0x00100001
! PROCEDURE p(x: ptr);
!   NEW(x); x.x := 3
CONST 4
CONST tPtrVal.%1
LOCAL 12
CONST NEW
CALL 3
CONST 3
LDLW 12
NCHECK 13
STOREW
RETURN
END

PROC tPtrVal.%main 0 16 0
!   NEW(q); q.x := 2; 
CONST 4
CONST tPtrVal.%1
CONST tPtrVal.q
CONST NEW
CALL 3
CONST 2
LDGW tPtrVal.q
NCHECK 19
STOREW
!   p(q);
LDGW tPtrVal.q
CONST tPtrVal.p
CALL 1
!   Out.Int(q.x, 0); Out.Ln
CONST 0
LDGW tPtrVal.q
NCHECK 21
LOADW
CONST Out.Int
CALL 2
CONST Out.Ln
CALL 0
RETURN
END

! Global variables
GLOBAL tPtrVal.q 4

! Pointer map
DEFINE tPtrVal.%gcmap
WORD GC_BASE
WORD tPtrVal.q
WORD 0
WORD GC_END

! Descriptor for *anon*
DEFINE tPtrVal.%1
WORD 0
WORD 0
WORD tPtrVal.%1.%anc

DEFINE tPtrVal.%1.%anc
WORD tPtrVal.%1

! End of file
]]*)
