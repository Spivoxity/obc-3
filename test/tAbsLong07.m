MODULE tAbsLong07;

IMPORT Out;

VAR x: LONGINT;

BEGIN
  x := -123456789012345678;
  x := ABS(x);
  Out.LongInt(x, 0);
  Out.Ln
END tAbsLong07.

(*<<
123456789012345678
>>*)

(*[[
!! (SYMFILE #tAbsLong07 STAMP #tAbsLong07.%main 1 #tAbsLong07.m)
!! (CHKSUM STAMP)
!! 
MODULE tAbsLong07 STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tAbsLong07.%main 0 4 0
!   x := -123456789012345678;
QCONST -123456789012345678
STGQ tAbsLong07.x
!   x := ABS(x);
LDGQ tAbsLong07.x
GLOBAL ABSLONG
CALLQ 2
STGQ tAbsLong07.x
!   Out.LongInt(x, 0);
CONST 0
LDGQ tAbsLong07.x
GLOBAL Out.LongInt
CALL 3
!   Out.Ln
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tAbsLong07.x 8

! End of file
]]*)
