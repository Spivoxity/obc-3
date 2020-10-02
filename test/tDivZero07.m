MODULE tDivZero07;

(*<<
Runtime error: DIV or MOD by zero on line 13 in module tDivZero07
In procedure tDivZero07.%main
   called from MAIN
>>*)

VAR x: INTEGER;

BEGIN
  x := 0;
  x := 1 DIV x
END tDivZero07.

(*[[
!! (SYMFILE #tDivZero07 STAMP #tDivZero07.%main 1 #tDivZero07.m)
!! (CHKSUM STAMP)
!! 
MODULE tDivZero07 STAMP 0
ENDHDR

PROC tDivZero07.%main 0 2 0
!   x := 0;
CONST 0
STGW tDivZero07.x
!   x := 1 DIV x
CONST 1
LDGW tDivZero07.x
ZCHECK 13
DIV
STGW tDivZero07.x
RETURN
END

! Global variables
GLOVAR tDivZero07.x 4

! End of file
]]*)
