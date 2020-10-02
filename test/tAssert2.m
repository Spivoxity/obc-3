MODULE tAssert2;

VAR x: INTEGER;

BEGIN
  x := 3;
  ASSERT(x = 2, 999)
END tAssert2.

(*<<
Runtime error: assertion failed (999) on line 7 in module tAssert2
In procedure tAssert2.%main
   called from MAIN
>>*)

(*[[
!! (SYMFILE #tAssert2 STAMP #tAssert2.%main 1 #tAssert2.m)
!! (CHKSUM STAMP)
!! 
MODULE tAssert2 STAMP 0
ENDHDR

PROC tAssert2.%main 0 3 0
!   x := 3;
CONST 3
STGW tAssert2.x
!   ASSERT(x = 2, 999)
LDGW tAssert2.x
CONST 2
JEQ L2
CONST 999
CONST 7
GLOBAL EASSERT
CALL 2
LABEL L2
RETURN
END

! Global variables
GLOVAR tAssert2.x 4

! End of file
]]*)
