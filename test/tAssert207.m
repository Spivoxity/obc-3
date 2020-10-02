MODULE tAssert207;

VAR x: INTEGER;

BEGIN
  x := 3;
  ASSERT(x = 2, 999)
END tAssert207.

(*<<
Runtime error: assertion failed (999) on line 7 in module tAssert207
In procedure tAssert207.%main
   called from MAIN
>>*)

(*[[
!! (SYMFILE #tAssert207 STAMP #tAssert207.%main 1 #tAssert207.m)
!! (CHKSUM STAMP)
!! 
MODULE tAssert207 STAMP 0
ENDHDR

PROC tAssert207.%main 0 3 0
!   x := 3;
CONST 3
STGW tAssert207.x
!   ASSERT(x = 2, 999)
LDGW tAssert207.x
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
GLOVAR tAssert207.x 4

! End of file
]]*)
