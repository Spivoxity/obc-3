MODULE tPi;

(*<<
3.14159265359
>>*)

IMPORT MathL, Out;

BEGIN
  Out.LongReal(MathL.pi); Out.Ln
END tPi.

(*[[
!! (SYMFILE #tPi 0x00000301 #tPi.%main 1)
!! (CHKSUM 0x104cb0bf)
!! 
MODULE tPi STAMP 0
IMPORT MathL STAMP
IMPORT Out STAMP
ENDHDR

PROC tPi.%main 0 3 0
!   Out.LongReal(MathL.pi); Out.Ln
DCONST 3.14159265359
GLOBAL Out.LongReal
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

! End of file
]]*)
