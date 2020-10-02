MODULE tPNil07;

(*<<
Runtime error: null pointer error on line 13 in module tPNil07
In procedure tPNil07.%main
   called from MAIN
>>*)

VAR x: PROCEDURE;

BEGIN
  x := NIL;
  x
END tPNil07.

(*[[
!! (SYMFILE #tPNil07 STAMP #tPNil07.%main 1 #tPNil07.m)
!! (CHKSUM STAMP)
!! 
MODULE tPNil07 STAMP 0
ENDHDR

PROC tPNil07.%main 0 2 0
!   x := NIL;
CONST 0
STGW tPNil07.x
!   x
LDGW tPNil07.x
NCHECK 13
CALL 0
RETURN
END

! Global variables
GLOVAR tPNil07.x 4

! End of file
]]*)
