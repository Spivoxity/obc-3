MODULE tBoundErr;

VAR a: ARRAY 5 OF INTEGER;

BEGIN
  a[5] := 7
END tBoundErr.

(*<<
Runtime error: array bound error on line 6 in module tBoundErr
In procedure tBoundErr.%main
   called from MAIN
>>*)

(*[[
!! (SYMFILE #tBoundErr STAMP #tBoundErr.%main 1 #tBoundErr.m)
!! (CHKSUM STAMP)
!! 
MODULE tBoundErr STAMP 0
ENDHDR

PROC tBoundErr.%main 0 4 0
!   a[5] := 7
CONST 7
GLOBAL tBoundErr.a
CONST 5
CONST 5
BOUND 6
STIW
RETURN
END

! Global variables
GLOVAR tBoundErr.a 20

! End of file
]]*)
