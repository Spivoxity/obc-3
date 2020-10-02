MODULE tNoRet;

IMPORT Out;

PROCEDURE merit(x: INTEGER): INTEGER;
BEGIN
  IF ODD(x) THEN RETURN 3 END
END merit;

BEGIN
  Out.Int(merit(8), 0); Out.Ln
END tNoRet.

(*<<
Runtime error: function failed to return a result on line 5 in module tNoRet
In procedure tNoRet.merit
   called from tNoRet.%main
   called from MAIN
>>*)

(*[[
!! (SYMFILE #tNoRet STAMP #tNoRet.%main 1 #tNoRet.m)
!! (CHKSUM STAMP)
!! 
MODULE tNoRet STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tNoRet.merit 0 2 0
! PROCEDURE merit(x: INTEGER): INTEGER;
!   IF ODD(x) THEN RETURN 3 END
LDLW 12
CONST 1
BITAND
JEQZ L3
CONST 3
RETURN
LABEL L3
ERROR E_RETURN 5
END

PROC tNoRet.%main 0 3 0
!   Out.Int(merit(8), 0); Out.Ln
CONST 0
CONST 8
GLOBAL tNoRet.merit
CALLW 1
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

! End of file
]]*)
