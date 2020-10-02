MODULE tAssert;

(*<<
Runtime error: assertion failed (0) on line 12 in module tAssert
In procedure tAssert.Fail
   called from tAssert.%main
   called from MAIN
>>*)

PROCEDURE Fail;
BEGIN
  ASSERT(1 < 0)
END Fail;

BEGIN
  Fail
END tAssert.

(*[[
!! (SYMFILE #tAssert STAMP #tAssert.%main 1 #tAssert.m)
!! (CHKSUM STAMP)
!! 
MODULE tAssert STAMP 0
ENDHDR

PROC tAssert.Fail 0 3 0
! PROCEDURE Fail;
CONST 0
CONST 12
GLOBAL EASSERT
CALL 2
RETURN
END

PROC tAssert.%main 0 1 0
!   Fail
GLOBAL tAssert.Fail
CALL 0
RETURN
END

! End of file
]]*)
