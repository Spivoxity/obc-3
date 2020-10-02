MODULE tDivZ6407;

(*<<
Runtime error: DIV or MOD by zero on line 13 in module tDivZ6407
In procedure tDivZ6407.%main
   called from MAIN
>>*)

VAR x: LONGINT;

BEGIN
  x := 0;
  x := 1 DIV x
END tDivZ6407.

(*[[
!! (SYMFILE #tDivZ6407 STAMP #tDivZ6407.%main 1 #tDivZ6407.m)
!! (CHKSUM STAMP)
!! 
MODULE tDivZ6407 STAMP 0
ENDHDR

PROC tDivZ6407.%main 0 4 0
!   x := 0;
CONST 0
CONVNQ
STGQ tDivZ6407.x
!   x := 1 DIV x
CONST 1
CONVNQ
LDGQ tDivZ6407.x
QZCHECK 13
QDIV
STGQ tDivZ6407.x
RETURN
END

! Global variables
GLOVAR tDivZ6407.x 8

! End of file
]]*)
