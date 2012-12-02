MODULE tPNil;

(*<<
Runtime error: null pointer error on line 13 in module tPNil
In procedure tPNil.%main
   called from MAIN
>>*)

VAR x: PROCEDURE;

BEGIN
  x := NIL;
  x
END tPNil.

(*[[
!! SYMFILE #tPNil STAMP #tPNil.%main 1
!! END STAMP
!! 
MODULE tPNil STAMP 0
ENDHDR

PROC tPNil.%main 0 8 0
!   x := NIL;
CONST 0
STGW tPNil.x
!   x
LDGW tPNil.x
NCHECK 13
CALL 0
RETURN
END

! Global variables
GLOBAL tPNil.x 4

! End of file
]]*)
