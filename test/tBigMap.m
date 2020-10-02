MODULE tBigMap;

IMPORT Out;

CONST MAXWORD = 500000;

TYPE
  Exp = POINTER TO ExpTree;
  ExpTree = RECORD value : INTEGER END;

VAR
  exp1*: ARRAY MAXWORD OF Exp;
  exp2*: ARRAY 31 OF Exp;
  exp3*: ARRAY 32 OF Exp;

BEGIN
  Out.Int(LEN(exp1), 0); Out.Ln
END tBigMap.

(*<<
500000
>>*)

(*[[
!! (SYMFILE #tBigMap STAMP #tBigMap.%main 1 #tBigMap.m)
!! (GLOBAL #exp1* #tBigMap.exp1 !1 (ARRAY 500000 !2 (POINTER)))
!! (TARGET =2 !3 (RECORD #tBigMap.ExpTree 4 VOID
!!     (FIELD #value 0 INTEGER)))
!! (GLOBAL #exp2* #tBigMap.exp2 !4 (ARRAY 31 =2))
!! (GLOBAL #exp3* #tBigMap.exp3 !5 (ARRAY 32 =2))
!! (CHKSUM STAMP)
!! 
MODULE tBigMap STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tBigMap.%main 0 3 0
!   Out.Int(LEN(exp1), 0); Out.Ln
CONST 0
CONST 500000
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tBigMap.exp1 2000000
GLOVAR tBigMap.exp2 124
GLOVAR tBigMap.exp3 128

! Global pointer map
DEFINE tBigMap.%gcmap
WORD GC_BASE
WORD tBigMap.exp1
WORD GC_BLOCK
WORD 0
WORD 500000
WORD GC_BASE
WORD tBigMap.exp2
WORD 0xffffffff
WORD GC_BASE
WORD tBigMap.exp3
WORD GC_BLOCK
WORD 0
WORD 32
WORD GC_END

! Descriptor for ExpTree
DEFINE tBigMap.ExpTree
WORD 0
WORD 0
WORD tBigMap.ExpTree.%anc

DEFINE tBigMap.ExpTree.%anc
WORD tBigMap.ExpTree

! End of file
]]*)
