MODULE tOrdSet;

IMPORT Out;

VAR s: SET;

BEGIN
  s := {0, 1, 2, 3, 5, 8, 13, 21};
  Out.Int(ORD(s), 0); Out.Ln
END tOrdSet.

(*<<
2105647
>>*)

(*[[
!! (SYMFILE #tOrdSet STAMP #tOrdSet.%main 1 #tOrdSet.m)
!! (CHKSUM STAMP)
!! 
MODULE tOrdSet STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tOrdSet.%main 0 3 0
!   s := {0, 1, 2, 3, 5, 8, 13, 21};
CONST 2105647
STGW tOrdSet.s
!   Out.Int(ORD(s), 0); Out.Ln
CONST 0
LDGW tOrdSet.s
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tOrdSet.s 4

! End of file
]]*)
