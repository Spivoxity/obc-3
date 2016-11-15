MODULE tAbsLong;

IMPORT Out;

VAR x: LONGINT;

BEGIN
  x := -123456789012345678;
  x := ABS(x);
  Out.LongInt(x, 0);
  Out.Ln
END tAbsLong.

(*<<
123456789012345678
>>*)

(*[[
!! (SYMFILE #tAbsLong 0x00000301 #tAbsLong.%main 1)
!! (CHKSUM 0x4f9b7389)
!! 
MODULE tAbsLong STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tAbsLong.%main 0 4 0
!   x := -123456789012345678;
QCONST -123456789012345678
STGQ tAbsLong.x
!   x := ABS(x);
LDGQ tAbsLong.x
GLOBAL ABSQUAD
CALLQ 2
STGQ tAbsLong.x
!   Out.LongInt(x, 0);
CONST 0
LDGQ tAbsLong.x
GLOBAL Out.LongInt
CALL 3
!   Out.Ln
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tAbsLong.x 8

! End of file
]]*)
